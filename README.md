
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
 * [C4 interconnect](experiments/src/c4_interconnect_database.erl) database
 * [R4 interconnect](experiments/src/r4_interconnect_database.erl) database
 * [C4 interconnect](experiments/src/c4_interconnect_map.erl) map
 * [R4 interconnect](experiments/src/r4_interconnect_map.erl) map
 * [C4 head](experiments/src/c4_head_experiment.erl)
 * [C4 tail](experiments/src/c4_tail_experiment.erl)
 * [Global enable](experiments/src/global_enable_experiment.erl)
 * [Global interconnect mux](experiments/src/global_interconnect_mux_experiment.erl) experiment
 * [LAB clk1](experiments/src/lab_clk1_playground.erl) playground
 * [LAB control](experiments/src/lab_control_playground.erl) playground
 * [LAB control mux](experiments/src/lab_control_mux_experiment.erl)
 * [Global network](experiments/src/global_network_experiment.erl)
 * [LAB interconnect global](experiments/src/lab_interconnect_global.erl)
 * [LAB interconnect](experiments/src/lab_interconnect_database.erl) database
 * [IOB interconnect global](experiments/src/iob_interconnect_global.erl)
 * [IOB interconnect](experiments/src/iob_interconnect_database.erl) database

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

These are being replaced by route cache based databases:

 * [IOB interconnect](experiments/src/iob_interconnect_database.erl)
 * [LAB interconnect](experiments/src/lab_interconnect_database.erl)

## C4/R4 interconnect mux

A mapping of
 * mux index to interconnect, and
 * mux key to interconnect source
mappings have been collected:

 * [C4 interconnect](experiments/src/c4_interconnect_database.erl)
 * [R4 interconnect](experiments/src/r4_interconnect_database.erl)

## C4/R4 interconnect map

A mapping from fuse locations to internconnect indexes is encoded in:

 * [C4 interconnect map](experiments/src/c4_interconnect_map.erl)
 * [R4 interconnect map](experiments/src/r4_interconnect_map.erl)

## Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

### `{global(), row, off}` and `{global(), {column, #}, off}`

Each of the four global clock networks are driven horizontally (row)
and then to each column. Unused clock networks are turned off either
at individual columns or for the whole row.

### `{global(), interconnect}`

Each of the global clock networks can be driven from either:

 * dedicated pins, or
 * internal interconnects.

This fuse selects an interconnect, rather than the default dedicated pin.

### `{global(), from6, mux#}`, `{global(), from4, mux#}` and `{global(), from3, mux#}`

Selects an interconnect into each of the four global networks.

For the EPM240,
each global network has a two dimentional mux of size 6 x 3
selecting from 18 interconnects at `{iob,1,3}`.

For other densities,
each global network has a two dimentional mux of size 4 x 3
selecting from 10 interconnects
at `{iob,9,3}`, `{iob,11,3}` & `{iob,13,3}` respectively.

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

### `{iob(), {interconnect, #}, from4, mux# / gclk}` and `{iob(), {interconnect, #}, from3, mux#}`

Selects a direct-link, C4 or R4 onto an IOB's interconnect.

Not used at the same time as the dedicated direct-link above.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

As a special case, row interconnects 8 and 17 have an extra fuse expanding the
mux to size 5 x 3. The extra 3 alternatives source from the global clock
networks.

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

### `{lab(), a_clr1, control_5_not_4}`

Selects the asyncronous clr1 line from either control line 4 or 5:

 * a `0` bit selects 5
 * a `1` bit selects 4

### `{lab(), a_load, control_3_not_2}`

Selects the asyncronous load line from either control line 2 or 3:

 * a `0` bit selects 3
 * a `1` bit selects 2

### `{lab(), clk1, control_0_not_1}`

Selects the asyncronous load line from either control line 2 or 3:

 * a `0` bit selects 0
 * a `1` bit selects 1

### `{lab(), clk#, global#}`

Each LAB can select a clk1 & clk2
from amongst the four global signals (0..3) (and others).

The four global signals are selected with a one-shot fuse per selection,
the selection is active with a bit of `0`.

### `{lab(), clk#, invert}`

Each LAB's clk1 & clk2 can be inverted. Invert is selected when the bit is `0`.

### `{lab(), clr#, global#}`

Each LAB can select a clr1 & clk2
from amongst the four global signals (0..3) (and others).

The four global signals are selected with a one-shot fuse per selection,
the selection is active with a bit of `0`.

### `{lab(), clr#, invert}`

Each LAB's clr1 can be inverted. Invert is selected when the bit is `0`.

### `{lab(), {control, #}, from6, mux#}` and `{lab(), {control, #}, from3, mux#}`

Six LAB control lines are selected from local interconnects
via two dimentional muxes, one of size 3, and the other of size 6.

These muxes are one-cold.

### `{lab(), {interconnect, #}, direct_link}`

Selects a direct-link from a neighbouring LAB onto a LAB's interconnect.

Not used at the same time as the interconnect muxes below.

### `{lab(), {interconnect, #}, from4, mux#}`, `{lab(), {interconnect, #}, from3, mux#}` and `{lab(), {interconnect, 12/25}, from4, gclk}`

Selects a direct-link, C4 or R4 onto a LAB's interconnect.

Not used at the same time as the dedicated direct-link above.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

As a special case, interconnects 12 and 25 have an extra fuse expanding the
mux to size 5 x 3. The extra 3 alternatives source from the global clock
networks.

### `{c4(), {mux, #}, from4, mux#}` and `{c4(), {mux, #}, from3, mux#}` and `{c4(), {mux, #}, direct_link}`

C4 blocks located either side (left & right) of LABs:

 * where `c4()` is `{c4, x(), y()}`,
 * having 14 interconnects numbered `{mux, 0}` to `{mux, 13}`.

These fuses select a direct-link, C4 or R4 onto the interconnects:

 * via the direct-link fuse, or
 * via the two dimentional mux of size 4 x 3
   selecting from 12 alternative sources.

### `{c4(), {mux, #}, io_data_in#}`

C4 blocks located in each of the top and bottom IO blocks:

 * where `c4()` is `{c4, x(), y()}`,
 * having 10 interconnects numbered `{mux, 0}` to `{mux, 9}`.

These fuses select an IO cell input via a one dimentional mux of size 2
(`io_data_in0` and `io_data_in1`).

### `{r4(), {mux, #}, from4, mux#}` and `{r4(), {mux, #}, from3, mux#}` and `{r4(), {mux, #}, direct_link}`

R4 blocks located either side (left & right) of LABs:

 * where `r4()` is `{r4, x(), y()}`,
 * having 16 interconnects numbered `{mux, 0}` to `{mux, 15}`.

These fuses select a direct-link, C4 or R4 onto the interconnects:

 * via the direct-link fuse, or
 * via the two dimentional mux of size 4 x 3
   selecting from 12 alternative sources.

