import json
import sys
import torch as t
import torch.nn as tn
import torch.nn.functional as tf
import torch.optim as to

kernel_size = 3
filter_count = 1
rounds = 1
leakage = 0.1
regularization = 1e-4
learning_rate = 0.000001
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
	return [t.cat([t.unsqueeze(val, 0) for val in field]) for field in zip(*es)]

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

def loss(nn, nn_outputs, tr_outputs):
	nn_won, nn_cleared, nn_duration, nn_moves = nn_outputs
	tr_won, tr_cleared, tr_duration, tr_moves = tr_outputs

	# all these sums need to be separate, because they have different
	# dimensions and we don't want broadcasting behavior to introduce
	# double-counting
	loss = t.sum(
		(nn_won - tr_won)**2 +
		(nn_cleared - tr_cleared)**2 +
		(nn_duration - tr_duration)**2
		) - t.sum(tr_moves * t.log(nn_moves))
	for p in nn.parameters(): loss = loss + regularization * t.sum(p*p)

	return loss

if __name__ == '__main__':
	if len(sys.argv) == 2:
		es = load_examples(sys.argv[1])
		net = build_net()
		optimizer = to.SGD(net.parameters(), lr=learning_rate, momentum=momentum)
		for i in range(100):
			net.zero_grad()
			x = loss(net, net.forward(es[0]), es[1:])
			print(x)
			x.backward()
			optimizer.step()
	else:
		print('USAGE: nsaid FILE')
		print('')
		print('Do very simple neural net training for the examples in FILE')
		sys.exit(1)
