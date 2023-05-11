# Hensel

Initial functionality consists of an algorithm of factorization of polynomials of the form $x^n - 1$ with appropriate restrictions on $n$ over modular rings into unitary irreducibles.

This topic is closely related to error-correcting codes and I expect to add some functionality related to it. At least transfer several related gists to this repo, probably with dependency on bitvec package.

Here is the contents of help:
```
Usage: runhensel (-d|--degree DEGREE) (-p|--char CHAR) (-n|--power POW)

  Factorization of x^n-1 over Galois fields

Available options:
  -d,--degree DEGREE       Degree of a polynomial
  -p,--char CHAR           Characteristic of the base field
  -n,--power POW           Power of characteristic
  -h,--help                Show this help text
```
