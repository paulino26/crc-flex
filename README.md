# crc-flex

This library designed to allow the calculation of CRCs for arbitrary configurations of width, underlying polynomial, initial value, final XOR and reflecting of the input / output.

This module is based on the information and subsequent C code implementation
given ["A PAINLESS GUIDE TO CRC ERROR DETECTION ALGORITHM"](http://read.pudn.com/downloads137/doc/585979/crc-Ross.pdf), Ross N. Williams, 19th August 1993.

The library is focused on the flexibility to model many CRCs easily and is not optimised for speed or memory usage.
