# Clirc

**Clirc** is an interpreter for the
[CIRC](https://introtcs.org/public/lec_03_computation.html#andornotsec)
language, but using a syntax inspired by the LISP family of languages, i.e.,
S-expressions.

As an example, where one would write `abc = NAND(uvw, xyz)` in *CIRC*, the
equivalent expression in *Clirc* would be `(set! abc (nand uvw xyz))`. Also, the
programs input in *CIRC* are written as `X[i]` while in *Clirc* they are written
`(:in i)`; and the outputs are written `Y[i]` in *CIRC* and `(:out i)` in
*Clirc*.

As a more complete example, consider the following *CIRC* program to compute the
*majority* function for 3 inputs:

```python
firstpair  = AND(X[0],X[1])
secondpair = AND(X[1],X[2])
thirdpair  = AND(X[0],X[2])
temp       = OR(secondpair,thirdpair)
Y[0]       = OR(firstpair,temp)
```

The same program, in a direct translation would be written in *Clirc* as:

```lisp
(set! firstpair  (and (:in 0) (:in 1)))
(set! secondpair (and (:in 1) (:in 2)))
(set! thirdpair  (and (:in 0) (:in 2)))
(set! temp       (or secondpair thirdpair))
(set! (:out 0)   (or firstpair temp))
```

## Installation

Download from http://example.com/FIXME.

## Usage

FIXME: explanation

    $ java -jar clirc-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2024 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
