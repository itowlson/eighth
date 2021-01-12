import wasi_unstable fd_write [i32 i32 i32 i32] -> [i32]

const HSTDOUT i32 1  # standard output
const PRINT_MEM i32 0
const PRINT_NWRITTEN_BUF i32 10000

global MALLOC_LOC i32 20000

data 40 "hello "
data 60 "au revoir "
data 80 "fie on you "
data 100 "world\n"
data 120 "tout le monde\n"
data 140 "Gary\n"

struct str {
    $ptr i32
    $len i32
}

func malloc [i32] -> [i32] {
    MALLOC_LOC
    dup $0 + >MALLOC_LOC
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

func write_123 [str str str str] -> [] {
    $0 $1 concat
    $2 $3 concat
    write_line
    write_line
}

func write_line [str] -> [] {
    PRINT_MEM     $0.$ptr store  # addr at offset 0
    PRINT_MEM 4 + $0.$len store  # len at offset 4
    HSTDOUT PRINT_MEM 1 PRINT_NWRITTEN_BUF fd_write
    drop
}
