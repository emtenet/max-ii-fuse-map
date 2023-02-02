
# Default User Code

By default the Quartus compiler was setting the user code bits to
be the computed check sum. This added unwanted noise to the fuse
list when comparing two experiments.

Therefore a default user code is used for each compilation.
See `quartus:compile_defaults/1`.

A user code of `00000000` was chosen rathe than `FFFFFFFF`
since it produced less fuses.

