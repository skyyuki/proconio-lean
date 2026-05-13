module

meta import Lean.Parser

public import Proconio.Core


public section
namespace Proconio

open Lean.Parser.Term

/--
Read input a value of type from specified source.
```lean
let v ← read_value! type from source
```
-/
macro "read_value!" ty:term "from" source:term : doElem => do
  `(doElem| Readable.read $ty $source)

/--
Read input and define variable from specified source.
```lean
input! {
  from source,
  v: type,
  u: type,
}
```
-/
syntax "input!" "{" "from" term "," (ident ":" term),* "}" doSeq : doElem

macro_rules
| `(doElem| input! { from $source, $[$names : $tys],* } $[$body:doElem]*) => do
  let sourceItem ← `(doSeqItem| let source := $source)

  let args ← names.zip tys |>.mapM (m := Lean.MacroM) fun (name, tys) => do
    `(doSeqItem| let $name ← Readable.read $tys source)

  let bodyItems := body.map fun stx => Lean.mkNode ``doSeqItem #[stx]

  let doElems := #[sourceItem] ++ args ++ bodyItems

  `(doElem| do $doElems*)

