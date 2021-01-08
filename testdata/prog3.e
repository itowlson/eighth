func add_squares [i32 i32] -> [i32] {
    dup    *     swap
    dup    *
    +
}

func sub_squares [i32 i32] -> [i32] {
    dup * swap dup * -
}
