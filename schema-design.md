# Introduction

We have three tools that need to communicate:

1. nurse-sveta-multi-play uses MCTS to play high-quality games
2. nsaid looks at previously-played games and use them to train a neural net
3. brain-pattern runs a neural net on Dr. Mario positions

(nurse-sveta-multi-play sure is a mouthful. Henceforth I will refer to it as
nsmp instead.)

Information flows basically one-way from nsmp to nsaid (sending the results of
the gameplay to use during training), and from nsaid to brain-pattern (sending
the neural net configuration and weights). Because it's one-way, a database is
a sensible choice, and this document describes the data that needs to be
communicated and how it will be stored.

Information needs to flow back and forth between nsmp and brain-pattern, in a
request-response pattern: nsmp sends a board position, and brain-pattern must
send back its evaluation of that board. A database is a less sensible
intermediary for this, so I leave that discussion to another document.

Besides the bare minimum of information needed for the tools to do their job,
it's also desirable to store some information that lets the user track progress
and do post-facto debugging. Notably, we store times of some interesting events
associated with the data. All times are in UTC.

Unless otherwise specified, none of the fields are nullable.

# nurse-sveta-multi-play to nsaid

There are six concepts that each have their own table:

* A *batch* is a collection of games that were all played by the same nsmp instance.
* A *game* is a sequence of board positions and the moves that mediate between them.
* A *position* is an arrangement of viruses and pill pieces on the playfield.
* A *move* is a single placement of a pill.
* A *pill sequence* is the collection of color pairs that the AI is supposed to
  choose placements for.
* nsmp allows you to request that it not use the CPU for a little while, to
  give you a chance to play Rocket League or something. A *pause* is such a
  period of time where nsmp is running but not calculating.

In the next six subsections we describe each of those tables in detail, then
there's a final subsection with the schema in SQL format.

## batch

Ideally, this table describes all the parameters you would need to recompute
exactly the same move choices and board evaluations stored in the rest of the
database for this batch. It has these fields:

* `id` (the primary key): no special meaning/format other than being unique per
  batch
* `iterations`: how many MCMC iterations did the AI run for on each move?
* `net`: a path on the filesystem to the configuration and weights of the
  neural net used; this is nullable, because on the very first run of the AI
  there is no neural net trained yet
* `commit`: the git hash of the repo that nsmp was built from
* `dirty`: were there any changes to the working tree when nsmp was built?
* `scoring`: a version number of the evaluation metric for how good the board
  position is at the end of a game; this has no special semantics other than
  that nsaid and nsmp should agree on it (i.e. there's not claim that higher
  version numbers are better evaluation metrics or anything like that)
* `stall`: how many moves the AI is allowed to make without clearing a virus
  before the game is forcibly ended
* `seed`: the data used to seed the AI's RNG; the format isn't specified here
  and is up to the nsmp implementation
* `threads`: how parallel the nsmp run was

## game

* `id` (the primary key): no special meaning/format other than being unique per
  game
* `batch`: foreign key for the batch table
* `sequence_hash`: the hash of the pill sequence used in this game (see the
  description of the pill sequence table below for details on computing this
  hash)
* `outcome`: how did the game end? this field is nullable, and you'll see nulls
  while a game is still being computed
    * `win`: the AI successfully cleared all viruses
    * `lose`: the AI topped out
    * `stall`: the AI played a lot of moves in a row that didn't do anything
      productive
    * `killed`: the user ended nsmp before the game was finished calculating
    * `error`: the AI crashed or something? lol

## move

* `game`: foreign key for the game table
* `index`: moves start at 0, count up, and are dense (i.e. don't skip any
  numbers); moves bigger than the length of the pill sequence wrap around, so
  take this mod the sequence length to get the right pill sequence index
* `start`: the time that calculation on this move began
* `completion`: the time that calculation on this move completed
* `rotation`: the number of clockwise rotations of the pill (mod 4) from its
  starting configuration to when it was placed
* `x`: the column of the bottom left of the pill when it was placed, 0 on the
  left
* `y`: the row of the bottom left of the pill when it was placed, 0 on the
  bottom
* `evaluation`: a 4x8x16 array of numbers in [0,1], each one nullable. The
  indices are rotation, x, and y coordinate of a placement (using the same
  conventions as the fields above). The value is the probability that the AI
  chooses that move, with null indicating that the given placement is illegal
  or not reachable. The values will sum to 1 (up to rounding).

The `game` and `index` together are the primary key.

## position

This is a game board configuration. The configuration before move *n* has index
*n*, and the configuration after that move was made and the clears and gravity
and stuff was all settled and had a family has index *n+1*.

* `game`: foreign key for the game table
* `index`: indices start at 0, count up, and are dense; there is always exactly
  one more position index associated with a game than there are moves
* `board`: 128 bytes in the format described in the [maryodel
  protocol](maryodel/protocol/README.md)

The `game` and `index` together are the primary key.

## pill\_sequence

* `sequence_hash`: the sha256 hash of the pills in the sequence, concatenated
  and in [maryodel protocol format](maryodel/protocol/README.md). For example,
  for the sequence containing just two pills, the first with yellow on the left
  and blue on the right, the second with red in both halves, you would hash the
  bytes of the ASCII string `rwqu`, getting a hash of

        8f3441cdfd8ed2aa72b58e8b9df7a20817ded2b818170c2b654376374e2e46aa

* `index`: indices start at 0, count up, and are dense
* `pill`: two bytes in maryodel protocol format describing the pill; calculate
  as the first byte\*256 + the second byte

The `sequence_hash` and `index` together are the primary key.

A minor side calculation: if you would like to store 2^40 hashes, and have a
probability of 2^-40 or less of a collision, you need a ~119-bit hash. Those
numbers are already both pretty overkill, so 256 bits is completely ridiculous,
but hey. It's simple.

## pause

* `batch`: foreign key for the batch table
* `pause_request`: the time at which a pause was requested by the
  user
* `pause_begin`: the time at which all threads announced they were paused
* `resume_request`: the time at which the user requested that computation begin
  again
* `resume_begin`: the time at which all threads announced they had resumed

The `batch` and `pause_request` together are the primary key.

All of `pause_begin`, `resume_request`, and `resume_begin` are nullable, and
will transition in order from null to non-null. You can expect that
`pause_request <. pause_begin`, `pause_request <. resume_request`, and
`resume_request <. resume_begin`, where `<.` is a version of less than where
null is minimal; that is,

    null <. null
    null <. a non-null time
    t1 <. t2 whenever t1 and t2 are non-null and t1 < t2

## SQL schema

    create table batch (
        id serial primary key,
        iterations bigint not null check (0 <= iterations),
        net bytea not null,
        commit bytea not null,
        dirty boolean not null,
        scoring integer not null,
        stall integer not null check (0 <= stall),
        seed bytea not null,
        threads smallint not null check (1 <= threads)
    );

    create table pill_sequence (
        sequence_hash bytea not null check (octet_length(sequence_hash) = 32),
        index integer not null check (0 <= index),
        pill smallint not null check (pill in (29045,29046,29047,29301,29302,29303,29557,29558,29559)),
        primary key (sequence_hash, index)
    );

    create type outcome as enum ('error', 'killed', 'stall', 'lose', 'win');
    create table game (
        id serial primary key,
        batch integer references batch not null,
        sequence_hash bytea not null, /* TODO: how to check that this hash is somewhere in the pill_sequence table? */
        outcome outcome
    );

    create table move (
        game integer references game,
        index integer check (0 <= index),
        start timestamp not null,
        completion timestamp not null check (start < completion),
        rotation smallint not null check (0 <= rotation and rotation <= 3),
        x smallint not null check (0 <= x and x <= 7),
        y smallint not null check (0 <= y and y <= 15),
        evaluation real[4][8][16], /* TODO: check that it's actually [4][8][16] and in 0-1 range? */
        primary key (game, index)
    );

    create table position (
        game integer references game,
        index integer check (0 <= index),
        board bytea not null check (octet_length(board) = 128),
        primary key (game, index)
    );

    create table pause (
        batch integer references batch,
        pause_request timestamp,
        pause_begin timestamp check (pause_request < pause_begin),
        resume_request timestamp check (pause_request < resume_request),
        resume_begin timestamp check (resume_request < resume_begin),
        primary key (batch, pause_request)
    );

# nsaid to brain-pattern

(TODO)
