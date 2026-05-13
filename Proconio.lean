module

public import Proconio.Basic
public import Proconio.Source
public import Proconio.Readable
public import Proconio.Marker
public import Proconio.Argio
public import Proconio.Macro

/-!
`proconio` and `argio` library for lean 4. Easy to handle input for competitive programming.

`proconio-lean` provides a short way to read form stdin and prase values. This is made by `Source` and `Readable` class and `argio!`, `input!`, and `read_value!` syntax.

# Usages and Examples
See Example directory for other features and usages.

## Argio
Argio syntax is porting of [tanakh's argio macro](https://github.com/tanakh/argio).

```lean
import Proconio.Argio
-- or import Proconio to import all features.

open Proconio

def solve (n m : Nat) (s : String) (p : Int × Int) (v : Vector Int 3) (vn : Nat) (vd : Vector Int vn) (n1 : Arg Nat_1) : IO Unit := do
  IO.println s!"n: {n}, m: {m}"
  IO.println s!"s: {s}"
  IO.println s!"x: {p}"
  IO.println s!"v: {v.toList}"
  IO.println s!"vn: {vn}"
  IO.println s!"vd: {vd.toList}"
  IO.println s!"n1: {n1}"

def main : IO Unit := do
  let source ← OnceSource.ofStdin

  argio! solve from source
```
`

## SourceRead
This is pure usage of features provided by this library.

```lean
import Proconio.Core
-- or import Proconio to import all features.

open Proconio

def main : IO Unit := do
  let source ← OnceSource.ofStdin

  let n ← source.read Nat
  let m ← source.read Nat
  IO.println s!"n: {n}, m: {m}"

  let s ← source.read String
  IO.println s!"s: {s}"

  let x ← source.read (Int × Int)
  IO.println s!"x: {x}"

  let v ← source.read (Vector Int 3)
  IO.println s!"v: {v.toList}"

  let vn ← source.read Nat
  IO.println s!"vn: {vn}"
  let vd ← source.read <| Vector Int vn
  IO.println s!"vd: {vd.toList}"

  let n1 ← source.read Nat_1
  IO.println s!"n1: {n1}"
```

-/

