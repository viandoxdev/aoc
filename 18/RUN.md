### Deps

- a common lisp compiler/interpreter (sbcl or clisp)
- curl
- make

### Running

```
# for sbcl
make sbcl
# for clisp
make clisp
# for anything else
make input && <common lisp implementation> main.lisp
```

> I couldn't figure out how/be bothered to install 3rd part dependencies for common lisp, so I just curled the input and read the file
> Tested with sbcl 2.2.11
