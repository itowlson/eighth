import wasi_unstable fd_write [i32 i32 i32 i32] -> [i32]

const HSTDOUT i32 1
const PRINT_MEM i32 0
const PRINT_NWRITTEN_BUF i32 10000

const FW_START i32 80

data 40 "hello world\n"
data 80 "au revoir tout le monde\n"

struct str {
    $ptr i32
    $len i32
}

func write_line [str] -> [] {
    PRINT_MEM     $0.$ptr store
    PRINT_MEM 4 + $0.$len store
    HSTDOUT PRINT_MEM 1 PRINT_NWRITTEN_BUF fd_write
    drop
}
