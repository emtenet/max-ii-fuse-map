
# Fuse vs Bit

Fuses are listed when they are encoded as a `0` bit in the POF file.

## Bits

The bitstream in a POF file is a stream of bits `0` or `1`.

The majority of bits are `1`s,i
a very simple experiment contains about 6% `0`s.

## Fuse lists

Experiments typically work with fuse lists, a list of fuse numbers.

Do we list fuses that have their bit as `0` or `1`?

If we where looking to optimize we would want the least number of
fuses in our list most of the time, therefore we would list
fuses that have their bit `0`.

*CHOICE*: Fuses are listed when they are encoded as a `0` bit in the POF file.

This choice means:

 * `pof_file:fuses/1` collects a list of fuses where the bit is `0`,
 * `matrix:build/1` records a pattern of `0` when a fuse is found in a list,
 * `matrix:print/1` prints a `*` for `1` bits and ` ` for `0` bits.

## Fuse meaning

Fuses will still be described relative to their bit value.

For, example `FUSE_X` represents:

 * a `0` bit means ...
 * a `1` bit means ...

