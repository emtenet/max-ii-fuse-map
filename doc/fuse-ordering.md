
# Fuse Ordering

We would like to number every fuse `0..`

After compileing we can read fuses from sources:

 * POF files
 * SVF files
 * ...

POF files store the fuses in 8-bits per byte.

SVF files store the fuses as 16-bit words.

It is not obvious which is the first fuse:

 * MSB of byte,
 * LSB of byte,
 * MSB of word,
 * LSB of word.

## Example

An example pair of programming files (only showing the first 64 bits):

```` program.pof
fe ff ff ff fd 3f 9f e7
````

```` program.svf
SDR 16 TDI (7FFF);
SDR 16 TDI (FFFF);
SDR 16 TDI (BFFC);
SDR 16 TDI (F9E7);
````

## Chosen ordering

Number *POF* fuses from bit-0 to bit-7 then the next byte.

Number *SVF* fuses from bit-15 to bit-0 for each SDR.

    Bits  0..15 of above: 0, 1, 1, 1; 1, 1, 1, 1; 1, 1, 1, 1; 1, 1, 1, 1.
    Bits 16..31 of above: 1, 1, 1, 1; 1, 1, 1, 1; 1, 1, 1, 1; 1, 1, 1, 1.
    Bits 32..47 of above: 1, 0, 1, 1; 1, 1, 1, 1; 1, 1, 1, 1; 1, 1, 0, 0.
    Bits 48..63 of above: 1, 1, 1, 1; 1, 0, 0, 1; 1, 1, 1, 0; 0, 1, 1, 1.

