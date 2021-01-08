func square [i32] -> [i32] {
    dup *
}

func add_squares [i32 i32] -> [i32] {
    square     swap
    square
    +
}

func sub_squares [i32 i32] -> [i32] {
    square swap square -
}
