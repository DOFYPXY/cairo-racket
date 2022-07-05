This is a virtual machine of [Cairo Programming Language](https://www.cairo-lang.org/) implemented in Racket.

To run the cairo program `simple.json` in this virtual machine, just run the following command: 
```
racket main.rkt tests/simple.json
```

Then you will see the vm outputs the content of memory and the running trace recorded by register values.

The VM doesn't support builtins and hints yet.

# Reference

[How Cairo Works](https://www.cairo-lang.org/docs/how_cairo_works/)

[Official VM](https://github.com/starkware-libs/cairo-lang/tree/master/src/starkware/cairo/lang/vm)

[A Rust Implementation](https://github.com/starkware-libs/cairo-lang/tree/master/src/starkware/cairo/lang/vm)
