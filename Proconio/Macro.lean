import Lean

import Proconio.Basic


open Lean.Parser.Term

macro "read_value!" ty:term "from" source:term : doElem => do
  `(doElem| Readable.read $ty $source)

syntax "input!" "{" "from" term "," (ident ":" term),* "}" doSeq : doElem

macro_rules
| `(doElem| input! { from $source, $[$names : $tys],* } $[$body:doElem]*) => do
  let sourceItem ← `(doSeqItem| let source := $source)

  let args ← names.zip tys |>.mapM (m := Lean.MacroM) fun (name, tys) => do
    `(doSeqItem| let $name ← Readable.read $tys source)

  let bodyItems := body.map fun stx => Lean.mkNode ``doSeqItem #[stx]

  let doElems := #[sourceItem] ++ args ++ bodyItems

  `(doElem| do $doElems*)

