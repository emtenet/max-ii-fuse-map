
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
 * [IOB direct-link](experiments/src/iob_direct_link_experiment.erl)
 * [IOB interconnect mux](experiments/src/iob_interconnect_mux_experiment.erl)
 * [IO control](experiments/src/io_control_experiment.erl)
 * [OE mux](experiments/src/output_enable_mux_playground.erl) playground
 * [OE invert](experiments/src/output_enable_invert_experiment.erl)
 * [OE mux](experiments/src/output_enable_mux_theory.erl) theory
 * [C4 fuses](experiments/src/c4_fuses_database.erl) database
 * [R4 fuses](experiments/src/r4_fuses_database.erl) database
 * [C4 fuse name](experiments/src/c4_fuse_name_experiment.erl)
 * [R4 fuse name](experiments/src/r4_fuse_name_experiment.erl)
 * [C4 interconnect](experiments/src/c4_interconnect_database.erl) database
 * [R4 interconnect](experiments/src/r4_interconnect_database.erl) database

## Fuse map

The `fuse_database` collected during experiments is then
encoded in source as a [fuse map](experiments/src/fuse_map.erl)
in both directions.

## Mux map

The following mux mappings are encoded:

 * [data mux](experiments/src/data_mux_map.erl)
 * [output mux](experiments/src/output_mux_map.erl)

## IOB/LAB interconnect mux

A database of IOB & LAB interconnect mux mappings has been collected
and some mapping tables printed:

 * [IOB interconnect](experiments/src/iob_interconnect_mux_database.erl)
 * [LAB interconnect](experiments/src/lab_interconnect_mux_database.erl)

## C4/R4 interconnect mux

Databases are being collected with C4 & R4 interconnect mux details.

List of fuse locations involved:

 * [C4 fuses](experiments/src/c4_fuses_database.erl)
 * [R4 fuses](experiments/src/r4_fuses_database.erl)

After the fuses have been named, a mapping of
 * mux index to interconnect, and
 * mux key to interconnect source
mappings have been collected:

 * [C4 interconnect](experiments/src/c4_interconnect_database.erl)
 * [R4 interconnect](experiments/src/r4_interconnect_database.erl)

## Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

### `{ioc(), bus_hold}` and `{ioc(), weak_pull_up}`

Each IOC can have bus-hold or weak pull-up enabled.

The feature is enabled when the POF bit is `0`.

Warning: only enable __one__ at a time.

### `{ioc(), current_strength_0}` and `{ioc(), current_strength_1}`

These fuses are on or off together.

Both fuses witha POF bit of `0` enable __minimal__ current strength.

### `{ioc(), enable}`

This fuse is used when the IOC is an input or output, but not when unused.

### `{ioc(), enable3, mux#}`, `{ioc(), enable4, mux#}` and `{ioc(), enable6, mux#}`

The IOC output enables are selected from local interconnects.

See `{ioc(), output#, mux#}` for details.

### `{ioc(), enable_invert}`

This fuse inverts the output-enable signal.

NOTE: This fuse is also used when the IOC is an input.

### `{ioc(), fast_out}`

Selects output value from fast-out link of neighbouring LAB
instead of via the output muxes.

### `{ioc(), input_delay}`

This fuse adds a delay buffer to the input.

### `{ioc(), open_drain}`

This fuse enables open-drain for the output.

### `{ioc(), output}` and `{ioc(), schmitt_trigger}`

Although these fuses are named uniquely they operate as a pair.

| `output` | `schmitt_trigger` | operation |
|  :---:   |  :---:            | --- |
|    1     |    1              | input |
|    1     |    0              | input with schmitt trigger |
|    0     |    1              |     |
|    0     |    0              | output |

### `{ioc(), output3, mux#}`, `{ioc(), output4, mux#}` and `{ioc(), output6, mux#}`

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

### `{ioc(), output_invert}`

This fuse inverts the output.

NOTE: This fuse is also used when the IOC is an input.

NOTE: This fuse is also used when the IOC is unused and driven to ground.

### `{iob(), {interconnect, #}, direct_link}`

Selects a direct-link from a neighbouring LAB onto an IOB's interconnect.

Only applicable for IOBs on the left and right sides.

Not used at the same time as the interconnect muxes below.

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

### `{lab(), {interconnect, #}, direct_link}`

Selects a direct-link from a neighbouring LAB onto a LAB's interconnect.

Not used at the same time as the interconnect muxes below.

### `{lab(), {interconnect, #}, from4, mux#}` and `{lab(), {interconnect, #}, from3, mux#}`

Selects a direct-link, c4 or r4 onto a LAB's interconnect.

Not used at the same time as the dedicated direct-link above.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

### `{c4_port(), port4, mux#}` and `{c4_port(), port4, mux#}` and `{c4_port(), direct_link}`

Where `c4_port()` is `{c4, x(), y(), port, 0..13}`.

Select a direct-link, C4 or R4 into a C4's interconnect.

The fuse names the port into the C4 interconnect,
not the C4 interconnect itself.

Either select:

 * via the direct-link fuse, or
 * via the two dimentional mux of size 4 x 3
   selecting from 12 alternative sources.

### `{r4_port(), port4, mux#}` and `{r4_port(), port4, mux#}` and `{r4_port(), direct_link}`

Where `r4_port()` is `{r4, x(), y(), port, 0..13}`.

Select a direct-link, C4 or R4 into a R4's interconnect.

The fuse names the port into the C4 interconnect,
not the R4 interconnect itself.

Either select:

 * via the direct-link fuse, or
 * via the two dimentional mux of size 4 x 3
   selecting from 12 alternative sources.

