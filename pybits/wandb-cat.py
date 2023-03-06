import wandb
import sys

def variant(v):
    try: v = int(v)
    except ValueError:
        try: v = float(v)
        except ValueError: pass
    return v

strings = [
    'local directory: ',
    'project name: ',
    'short run description: ',
    'run id for resuming (blank to start afresh): ',
    'current config is {}',
    'config key (blank to finish): ',
    'config value: ',
    'step: ',
    'metric name: ',
    'metric value: ',
    ]

if __name__ == '__main__':
    if len(sys.argv) > 2: raise ValueError
    if len(sys.argv) == 2:
        if sys.argv[1] in ['-q', '--quiet']:
            strings = ['' for _ in strings]
        else: raise ValueError

    args = {
        'dir'    : input(strings[0]),
        'project': input(strings[1]),
        'name'   : input(strings[2]),
        'id'     : input(strings[3]),
        'config' : {},
        }
    if args['id'] == '': del args['id']
    while True:
        if strings[4]: print(strings[4].format(args['config']))
        k = input(strings[5])
        if k == '': break
        args['config'][k] = variant(input(strings[6]))

    wandb.init(**args)

    while True:
        try: step = int(input(strings[7]))
        except EOFError: break
        wandb.log({input(strings[8]): variant(input(strings[9]))}, step=step)

    wandb.finish()
