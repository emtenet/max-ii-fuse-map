
# Altera MAX II Fuse Map (by experiment)

This is a puzzle.
Can I work out the fuse map of MAX II devices by
feeding the Quartus tools
and observing the outputs?

My experiments are being run in the following
[environment](doc/environment.md).

## Assumptions

 * [Fuse ordering](doc/fuse-ordering.md)
 * [Fuse vs bit](doc/fuse-vs-bit.md)
 * [Default user code](doc/default-user-code.md)

## Experiments

 * [How to indicate a global clock](experiments/src/global_clock_experiment.erl)
 * [User code](experiments/src/user_code_experiment.erl)
 * [Fuse count](experiments/src/fuse_count_experiment.erl)
 * [Generate pins](experiments/src/generate_pins.erl)
 * [Bus-hold](experiments/src/bus_hold_experiment.erl)
 * [Weak pull-up](experiments/src/weak_pull_up_experiment.erl)
 * [LUT](experiments/src/lut_experiment.erl)
 * [LC clk & clr](experiments/src/lc_clk_clr_experiment.erl)
 * [LAB clk1 global](experiments/src/lab_clk1_global_experiment.erl)
 * [LAB clk2 global](experiments/src/lab_clk2_global_experiment.erl)
 * [LAB clr1 global](experiments/src/lab_clr1_global_experiment.erl)
 * [Local line](experiments/src/local_line_experiment.erl) outputs from LCs
 * [Output](experiments/src/output_experiment.erl) constant

## Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

### `{ioc(), bus_hold}` and `{ioc(), weak_pull_up}`

Each IOC can have bus-hold or weak pull-up enabled.

The feature is enabled when the POF bit is `0`.

Warning: only enable *one* at a time.

### `{ioc(), enable_guess}` and `{ioc(), invert_guess}`

These are observed fuses, but the naming is a guess.

The `enable_guess` fuse was observed in the `output_experiment` but
since all unused pins are outputs by default, the fuse is more likely
to be an *enable* rather than *direction*.

The `invert_guess` may alternativly be a mux selecting alternative outputs.

### `{lc(), lut, {a0 | a1, b0 | b1, c0 | c1, d0 | d1}}`

Each LC as a 16-entry LUT with a fuse per entry.

The fuse for LUT term `a AND (NOT b) AND c AND d`
is named `{lc(), lut, {a1, b0, c1, d1}}`.

The stored bit is the result of the lookup.

### `{lc(), clk}` and `{lc(), clr}`

Each LAB can have two clock and two clears defined.

Each LC then can choose from those two clock/clears.

The fuse bit selects:

 * `1` LAB clk1 or clr1,
 * `0` LAB clk2 or clk2.

### `{lc(), local_line}`

Each LC can drive the LUT or register output to the local interconnect
via it's *local line*.

This fuse either:

 * enables that output, or
 * selects between the LUT and registers to output.

### `{lab(), clk#_global#}`

Each LAB can select a clk1 & clk2
from amongst the four global signals (0..3) (and others).

The four global signals are selected with a one-shot fuse per selection,
the selection is active with a bit of `0`.

### `{lab(), clk#_invert}`

Each LAB's clk1 & clk2 can be inverted. Invert is selected when the bit is `0`.

### `{lab(), clr#_global#}`

Each LAB can select a clr1 & clk2
from amongst the four global signals (0..3) (and others).

The four global signals are selected with a one-shot fuse per selection,
the selection is active with a bit of `0`.

### `{lab(), clr#_invert}`

Each LAB's clr1 can be inverted. Invert is selected when the bit is `0`.

