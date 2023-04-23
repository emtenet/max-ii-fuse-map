
-record(metric, {
    density :: density:density(),
    top_io :: max_ii:y(),
    top_lab :: max_ii:y(),
    left_io :: max_ii:x(),
    left_lab :: max_ii:x(),
    right_io :: max_ii:x(),
    right_lab :: max_ii:x(),
    bottom_io :: max_ii:y(),
    bottom_lab :: max_ii:y(),
    indent_left_io :: max_ii:x(),
    indent_left_lab :: max_ii:x(),
    indent_right_lab :: max_ii:x(),
    indent_bottom_io :: max_ii:y(),
    indent_bottom_lab :: max_ii:y(),
    pattern_x :: 0..3,
    pattern_y :: 0..3
}).

