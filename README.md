
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
 * Data MUX [playground](experiments/src/data_mux_playground.erl)
   and [theory](experiments/src/data_mux_theory.erl)
 * Output MUX [playground](experiments/src/output_mux_playground.erl)
   and [theory](experiments/src/output_mux_theory.erl)
 * [Direct link](experiments/src/direct_link_experiment.erl) (`lut_out`)
 * [LAB interconnect limit](experiments/src/lab_interconnect_limit_experiment.erl)
 * [LAB interconnect mux](experiments/src/lab_interconnect_mux_experiment.erl)

## Fuse map

The `fuse_database` collected during experiments is then
encoded in source as a [fuse map](experiments/src/fuse_map.erl)
in both directions.

## Mux map

The following mux mappings are encoded:

 * [data mux](experiments/src/data_mux_map.erl)
 * [output mux](experiments/src/output_mux_map.erl)

## LAB interconnect mux

A database of LAB interconnect mux mappings has been collected and som
[mapping tables printed](experiments/src/lab_interconnect_mux_database.erl)

## Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

### `{ioc(), bus_hold}` and `{ioc(), weak_pull_up}`

Each IOC can have bus-hold or weak pull-up enabled.

The feature is enabled when the POF bit is `0`.

Warning: only enable __one__ at a time.

### `{ioc(), enable}` and `{ioc(), invert}`

These are observed fuses, but the naming is a __guess__.

The `enable` fuse was observed in the `output_experiment` but
since all unused pins are outputs by default, the fuse is more likely
to be an __enable__ rather than __direction__.

The `invert` may alternativly be a mux selecting alternative outputs.

### `{ioc(), fast_out}`

Selects output value from fast-out link of neighbouring LAB
instead of via the output muxes.

### `{ioc(), output3, mux#}`, `{ioc(), output4, mux#}`  and `{ioc(), output6, mux#}`

The IOC outputs are selected from local interconnects
via two dimentional muxes.
Side IOCs have one of size 3, and the other of size 6.
Top/bottom IOCS have one of size 3, and the other of size 4.

These muxes are one-cold.

For example the fuses
`{ioc(), output6, mux2}` and `{ioc(), output3, mux1}`
select local interconnect 7.

For example the fuses
`{ioc(), output4, mux3}` and `{ioc(), output3, mux2}`
select local line 9.

### `{lc(), data_#3, mux#}` and `{lc(), data_#6, mux#}`

The LUT inputs `data_a`, `data_b`, `data_c` & `data_d` are
selected from local interconnects via two dimentional muxes,
one of size 3, and the other of size 6.

These muxes are one-cold.

For example the fuses
`{lc(), data_a3, mux1}` and `{lc(), data_a6, mux0}`
select local interconnect 3.

For example the fuses
`{lc(), data_c3, mux2}` and `{lc(), data_c6, mux5}`
select local line 7.

### `{lc(), lut, a#b#c#d#}`

Each LC as a 16-entry LUT with a fuse per entry.

The fuse for LUT term `a AND (NOT b) AND c AND d`
is named `{lc(), lut, a1b0c1d1}`.

The stored bit is the result of the lookup.

### `{lc(), clk}` and `{lc(), clr}`

Each LAB can have two clock and two clears defined.

Each LC then can choose from those two clock/clears.

The fuse bit selects:

 * `1` LAB clk1 or clr1,
 * `0` LAB clk2 or clk2.

### `{lc(), local_line}`

Each LC can drive the LUT or register output to the local interconnect
via it's __local line__.

This fuse either:

 * enables that output, or
 * selects between the LUT and registers to output.

### `{lc(), lut_out, left | right}`

Each LC can drive the LUT output to the left or right
via direct-links, r4s & c4s.

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

