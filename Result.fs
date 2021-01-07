module Result

type Result<'T, 'E> =
| Ok of 'T
| Err of 'E
