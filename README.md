# Morello ESR decoding utility

This is a quick hack to decode `ESR`, the exception syndrome register in
AArch64.  It includes some Morello specific faults, taken from the [Morello
Supplement to the Arm ARM](https://developer.arm.com/documentation/ddi0606/latest).

Build it using the `build` script.  You will need OCaml and zarith.
