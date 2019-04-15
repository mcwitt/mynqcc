# mynqcc

Experimental C99 compiler written in Haskell, following Nora Sandler's
excellent blog post series on [Writing a C Compiler][wacc].

## Installation

1. Make sure [Stack](https://docs.haskellstack.org/en/stable/README/) is installed
2. `cd mynqcc`
3. `stack install`
4. `stack test` to run tests

## Usage

1. To generate x86 assembly,
    ```shell
	stack exec mynqcc-exe -o my_program.s my_program.c
	```
2. Then, to generate an executable,
    ```shell
	gcc -m32 -w -o my_program my_program.s
	```

(For convenience, these steps are automated in the script `mycc.sh`.)

## Running `nqcc` tests

Nora Sandler's blog post series is accompanied by a [test
suite](https://github.com/nlsandler/write_a_c_compiler), which is
included as a submodule of this repo. To run the tests for stages 1
through 9, do

```shell
git clone https://github.com/mcwitt/mynqcc --recurse-submodules
cd mynqcc/write_a_c_compiler
./test_compiler "../mycc.sh /path/to/mynqcc-exe" $(seq 1 9)
```

[wacc]: https://norasandler.com/2017/11/29/Write-a-Compiler.html
