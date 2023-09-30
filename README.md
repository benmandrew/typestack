Program semantics and type system for a stack-based language. Inspired by exam question [2021P4Q8](https://www.cl.cam.ac.uk/teaching/exams/pastpapers/y2021p4q8.pdf) from the [Semantics of Programming Languages](https://www.cl.cam.ac.uk/teaching/2324/Semantics/) course at the University of Cambridge.

## How to run

[`opam`](https://opam.ocaml.org) is a necessary prerequisite.

```
opam switch create typestack ocaml.5.0.0
eval $(opam env)
opam install --with-test .
dune test
dune exec -- typestack
```
