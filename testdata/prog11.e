import wasi_unstable fd_write [i32 i32 i32 i32] -> [i32]

const HSTDOUT i32 1  # standard output
const PRINT_MEM i32 0
const PRINT_NWRITTEN_BUF i32 10000

global MALLOC_LOC i32 20000

data 40 "The sum of "
data 60 " and "
data 80 " is "
data 100 "\n"

data 200 "0123456789"
const DIGIT_BASE i32 200

struct str {
    $ptr i32
    $len i32
}

func malloc [i32] -> [i32] {
    MALLOC_LOC
    dup $0 + >MALLOC_LOC
}

func to_str [i32] -> [str] {
    $0 10 < if
        $0 DIGIT_BASE + 1
    else
        $0 10 / to_str
        $0 10 % to_str
        concat
    then
}

func concat [str str] -> [str] {
    $0.$len $1.$len +
    malloc
    dup
    dup
    $0.$ptr $0.$ptr $0.$len + 1 - do
        dup
        index load8u
        store8
        1 +
    loop
    $1.$ptr $1.$ptr $1.$len + 1 - do
        dup
        index load8u
        store8
        1 +
    loop
    swap
    -
}

func print_sum [i32 i32] -> [] {
    40 11                   # The sum of
    $0 to_str concat        # The sum of 123
    60 5                    # and
    $1 to_str concat        # and 456
    concat                  # The sum of 123 and 456
    80 4 concat             # The sum of 123 and 456 is
    $0 $1 + to_str concat   # The sum of 123 and 456 is 579
    100 1 concat            # trailing newline
    write_line
}

func write_line [str] -> [] {
    PRINT_MEM     $0.$ptr store  # addr at offset 0
    PRINT_MEM 4 + $0.$len store  # len at offset 4
    HSTDOUT PRINT_MEM 1 PRINT_NWRITTEN_BUF fd_write
    drop
}
