
# Fuse vs Bit

Fuses are listed when they are encoded as a `0` bit in the POF file.

## Bits

The bitstream in a POF file is a stream of bits `0` or `1`.

The majority of bits are `1`s,i
a very simple experiment contains about 6% `0`s.

## Fuses

Experiments typically work with fuse lists, a list of fuse numbers.

Do we list fuses that have their bit as `0` or `1`?

If we where looking to optimize we would want the least number of
fuses in our list most of the time, therefore we would list
fuses that have their bit `0`.

*CHOICE*: Fuses are listed when they are encoded as a `0` bit in the POF file.

## User code

The [user code](experiments/src/user_code_experiment.erl) experiment
gives support to this choice.

It found that user code `1` bits where encoded as `0` in the POF file.

Our choice means that user code fuses are listed when the bit is a `1`,
which is quite natural to work with.

