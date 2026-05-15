import Proconio.Core


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

