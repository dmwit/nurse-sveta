import json
import PIL.Image as pil
import PIL.ImageDraw as pild
import sys
import torch as t
import torch.nn as tn
import torch.nn.functional as tf
import torch.optim as to
import torch.utils.tensorboard as tb
import torchvision.transforms.functional as til

kernel_size = 3
filter_count = 1
rounds = 1
leakage = 0.1
regularization = 1e-4
learning_rate = 0.1
momentum = 0.9
dty = t.float64

channels = \
	{ 'a':  0, 'b':  1, 'c':  2
	, 'e':  3, 'f':  4, 'g':  5
	, 'm':  3, 'n':  4, 'o':  5
	, 'i':  3, 'j':  4, 'k':  5
	, 'u':  9, 'v': 10, 'w': 11
	, 'q':  6, 'r':  7, 's':  8
	, 'd': 12 # we'll trim this channel away
	}

def parse_example(example):
	board_raw, pill_raw, won_raw, cleared_raw, duration_raw, moves_raw = example

	board_channels = [[channels[board_raw[8*(15-y) + x]] for y in range(16)] for x in range(8)]
	board_channels_tensor = t.tensor(board_channels)
	board_one_hot = tf.one_hot(board_channels_tensor, 13)
	board_with_extra_channel = t.transpose(t.transpose(board_one_hot, 2, 1), 1, 0)
	board = t.narrow(board_with_extra_channel, 0, 0, 12).type(dty)

	pill_channels = [channels[pill_raw[i]] % 3 for i in range(2)]
	pill_channels_tensor = t.tensor(pill_channels)
	pill_one_hot = tf.one_hot(pill_channels_tensor, 3)
	pill_scalars = t.reshape(pill_one_hot, [6,1,1])
	pill = t.broadcast_tensors(pill_scalars, t.empty(1,8,16))[0].type(dty)

	moves_dense = \
		[ t.sparse_coo_tensor(
			[
				[0]*(len(ms)//3),
				[ms[i] for i in range(0,len(ms),3)],
				[ms[i] for i in range(1,len(ms),3)]
			],
			[ms[i] for i in range(2,len(ms),3)],
			[1,8,16]
			, dtype=dty)
		for ms in moves_raw
		]
	moves = t.cat(moves_dense).to_dense()

	won, cleared, duration = [t.tensor(val, dtype=dty) for val in [won_raw, cleared_raw, duration_raw]]

	return (t.cat([board, pill]), won, cleared, duration, moves)

def load_examples(filepath):
	with open(filepath, 'rb') as file:
		return tensors_from_examples(map(parse_example, json.load(file)))

def tensors_from_examples(es):
	return [t.stack([val for val in field]) for field in zip(*es)]

class Residual2d(tn.Module):
	def __init__(self, channels, kernel_size):
		super(Residual2d, self).__init__()
		self.prefix_layer = tn.Sequential \
			( tn.Conv2d(channels, channels, kernel_size, padding=(kernel_size-1)//2)
			, tn.BatchNorm2d(channels)
			, tn.LeakyReLU(negative_slope=leakage)
			, tn.Conv2d(channels, channels, kernel_size, padding=(kernel_size-1)//2)
			, tn.BatchNorm2d(channels)
			)
		self.suffix_layer = tn.LeakyReLU(negative_slope=leakage)

	def forward(self, x):
		return self.suffix_layer(x + self.prefix_layer.forward(x))

class DrMarioFinalLayer(tn.Module):
	def __init__(self, channels):
		super(DrMarioFinalLayer, self).__init__()
		self.num_outputs = 1 + 1 + 1 + 4*8*16 # just like the live documentation lmao
		self.fully_connected_layer = tn.Conv2d(channels, self.num_outputs, [8,16])
		self.won_layer = tn.Tanh()
		self.cleared_layer = tn.Softplus(threshold=5)
		self.duration_layer = tn.Softplus(threshold=5)
		self.moves_layer = tn.Sigmoid()

	def forward(self, x):
		# raw_outputs is n x self.num_outputs x 1 x 1
		raw_outputs = self.fully_connected_layer.forward(x)
		pre_won = raw_outputs[:,0,0,0]
		pre_cleared = raw_outputs[:,1,0,0]
		pre_duration = raw_outputs[:,2,0,0]
		pre_moves = t.reshape(raw_outputs[:,3:,0,0],[x.size()[0],4,8,16])
		unnormalized_moves = self.moves_layer(pre_moves)
		moves = unnormalized_moves / t.sum(unnormalized_moves, [1,2,3], keepdim=True)
		return ( self.won_layer(pre_won)
		       , self.cleared_layer(pre_cleared)
		       , self.duration_layer(pre_duration)
		       , moves
		       )

def build_net():
	layers = \
		[ tn.Conv2d(18, filter_count, kernel_size, padding=1)
		, tn.BatchNorm2d(filter_count)
		, tn.LeakyReLU(negative_slope=leakage)
		] + \
		[ Residual2d(filter_count, kernel_size) for i in range(rounds) ] + \
		[ DrMarioFinalLayer(filter_count) ]

	layers = tn.Sequential(*layers)
	layers.type(dty)
	return layers

def loss_without_logging(_writer, _i, nn, nn_outputs, tr_outputs):
	nn_won, nn_cleared, nn_duration, nn_moves = nn_outputs
	tr_won, tr_cleared, tr_duration, tr_moves = tr_outputs
	one = t.tensor(1, dtype=dty)

	# all these sums need to be separate, because they have different
	# dimensions and we don't want broadcasting behavior to introduce
	# double-counting
	loss = t.sum(
		(nn_won - tr_won)**2 +
		((nn_cleared - tr_cleared) / t.max(tr_cleared, one))**2 +
		((nn_duration - tr_duration) / t.max(tr_duration, one))**2
		) - t.sum(tr_moves * t.log(nn_moves))
	for p in nn.parameters(): loss = loss + regularization * t.sum(p*p)

	return loss

def loss_with_logging(writer, i, nn, nn_outputs, tr_outputs):
	nn_won, nn_cleared, nn_duration, nn_moves = nn_outputs
	tr_won, tr_cleared, tr_duration, tr_moves = tr_outputs
	one = t.tensor(1, dtype=dty)

	won_loss = t.sum((nn_won - tr_won)**2)
	cleared_loss = t.sum(((nn_cleared - tr_cleared) / t.max(tr_cleared, one))**2)
	duration_loss = t.sum(((nn_duration - tr_duration) / t.max(tr_duration, one))**2)
	moves_loss = -1*t.sum(tr_moves * t.log(nn_moves))
	parameters_loss = t.tensor(0, dtype=dty)
	for p in nn.parameters(): parameters_loss = parameters_loss + regularization * t.sum(p*p)

	writer.add_scalar('won_loss', won_loss, i)
	writer.add_scalar('cleared_loss', cleared_loss, i)
	writer.add_scalar('duration_loss', duration_loss, i)
	writer.add_scalar('moves_loss', moves_loss, i)
	writer.add_scalar('parameters_loss', parameters_loss, i)

	return won_loss + cleared_loss + duration_loss + moves_loss + parameters_loss

def load_images():
	imgs = t.stack([t.transpose(til.to_tensor(pil.open(filename)), 1, 2) for filename in
			[ 'imgs/' + color + '-' + shape + '.png'
			for shape in ['virus', 'disconnected', 'west', 'east']
			for color in ['red', 'yellow', 'blue']
			] +
			[ 'imgs/chevron-' + direction + '.png'
			for direction in ['east', 'south', 'west', 'north']
			]
		])
	# was:              12 channels x 3 colors x             8 x-pixels x          8 y-pixels
	# now: 1 examples x 12 channels x 3 colors x 1 columns x 8 x-pixels x 1 rows x 8 y-pixels
	imgs = t.unsqueeze(t.unsqueeze(t.unsqueeze(imgs, 3), 2), 0)
	return imgs

def images_from_nn_data(imgs, nn_inputs, nn_outputs):
	nn_won, nn_cleared, nn_duration, nn_moves = nn_outputs

	nexample = nn_inputs.size()[0]
	pills_left  = t.cat([t.zeros(nexample, 6, 1, dtype=dty), nn_inputs[:,12:15,0:1,0], t.zeros(nexample, 7, 1, dtype=dty)], 1)
	pills_right = t.cat([t.zeros(nexample, 9, 1, dtype=dty), nn_inputs[:,15:18,0:1,0], t.zeros(nexample, 4, 1, dtype=dty)], 1)
	pills = t.cat([t.zeros(nexample, 16, 3, dtype=dty), pills_left, pills_right, t.zeros(nexample, 16, 3, dtype=dty)], 2)
	# was: n examples x 16 channels x            8 columns
	# now: n examples x 16 channels x 1 colors x 8 columns x 1 x-pixels x 1 rows x 1 y-pixels
	pills = t.unsqueeze(t.unsqueeze(t.unsqueeze(t.unsqueeze(pills, 3), 3), 3), 2)

	# was: n examples x 18 channels x            8 columns x              16 rows
	# now: n examples x 12 channels x 1 colors x 8 columns x 1 x-pixels x 16 rows x 1 y-pixels
	boards = t.unsqueeze(t.unsqueeze(t.unsqueeze(t.flip(nn_inputs[:,0:12,:,:], [3]), 4), 3), 2)
	# was: n examples x 4 directions x            8 columns x              16 rows
	# now: n examples x 4 directions x 1 colors x 8 columns x 1 x-pixels x 16 rows x 1 y-pixels
	moves = t.unsqueeze(t.unsqueeze(t.unsqueeze(t.flip(nn_moves, [3]), 4), 3), 2)
	moves = moves / max(0.00001, min(1, t.max(moves)))

	# n examples x 16 images x 1 colors x 8 columns x 1 x-pixels x 17 rows x 1 y-pixels
	boards_moves_pills = t.cat([pills, t.cat([boards, moves], dim=1)], dim=5)
	# n examples x 3 colors x 8 columns x w x-pixels x 17 rows x h y-pixels
	board_images = t.sum(boards_moves_pills * imgs, 1)
	nexample, ncolor, ncol, w, nrow, h = board_images.size()
	board_images = t.reshape(board_images, [nexample, ncolor, ncol*w, nrow*h])

	footers = [pil.new('RGB', (ncol*w, 35), color=(0, 0, 0)) for i in range(nexample)]
	for i, footer in enumerate(footers):
		msg = 'won: %f\ncleared: %f\nduration: %f' % (nn_won[i], nn_cleared[i], nn_duration[i])
		pild.Draw(footer).text((1, 0), msg, fill=(255, 255, 255), spacing=1)
	footer_images = t.stack([t.transpose(til.to_tensor(footer), 1, 2) for footer in footers])
	footer_images = footer_images.type(dty)

	return t.cat([board_images, footer_images], dim=3)

if __name__ == '__main__':
	writer = tb.SummaryWriter()
	imgs = load_images()

	if len(sys.argv) == 2:
		es = load_examples(sys.argv[1])
		net = build_net()
		optimizer = to.SGD(net.parameters(), lr=learning_rate, momentum=momentum)
		for i in range(100):
			net.zero_grad()
			nn_outputs = net.forward(es[0])
			loss = loss_with_logging(writer, i, net, nn_outputs, es[1:])
			writer.add_scalar('loss', loss, i)
			board_images = images_from_nn_data(imgs, es[0], nn_outputs)
			writer.add_images('moves', board_images, i, dataformats='NCWH')
			loss.backward()
			optimizer.step()
	else:
		print('USAGE: nsaid FILE')
		print('')
		print('Do very simple neural net training for the examples in FILE')
		sys.exit(1)
