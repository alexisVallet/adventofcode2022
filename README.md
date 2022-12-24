# Advent of code solutions (mostly 2022)

My solutions for advent of code 2022, in Haskell.

## Installation

This package has only been used and tested on linux, specifically pop os 22.04.

### Dependencies
This package depends on the following:
- Dependencies for the Accelerate package, specifically its LLVM PTX backend
  using CUDA GPUs. Please follow installation instructions here for the 
  dependencies: https://github.com/AccelerateHS/accelerate-llvm/tree/master/accelerate-llvm 
  Notes:
  - libffi 7 seems to be required: it won't find the `.so` otherwise at build time
  - LLVM 9 is required: newer versions won't work, so you will probably need to
    follow the `Building from source` section in the paragraph above.
  - To build LLVM 9, I needed to add the following flag to `cmake`: `-DLLVM_ENABLE_RTTI=ON`
  - To build LLVM 9, I needed to downgrade to use GCC 10. GCC 11 did not work for some reason.
  - I was able to fix the following error by updating to the latest nvidia drivers: `CUDA Exception: forward compatibility was attempted on non supported HW`
  - Accelerate is only required for a few solutions, though accelerate is set as
    a global dependency. I should probably change that somehow, because installing
    Accelerate is painful.
- Haskell stack: https://docs.haskellstack.org/en/stable/
- Some BLAS and LAPACK distributions for HMatrix
- The Z3 library (i.e. `libz3-dev` on ubuntu at the time of writing)

### Running solutions
You may run solutions for a specific day by running:
`stack run aoc20YY-day-DD`

For instance, for day 01 of 2022:

`stack run aoc2022-day-01`

Note that some solutions may take a long time (up to a few hours), depending on 
available CPU/GPU computing resources.

