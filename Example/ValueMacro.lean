import Proconio.Macro


open Proconio

def main : IO Unit := do
  let source ← OnceSource.ofStdin

  let n ← read_value! Nat from source
  let m ← read_value! Nat from source
  IO.println s!"n: {n}, m: {m}"

  let s ← read_value! String from source
  IO.println s!"s: {s}"

  let x ← read_value! Int × Int from source
  IO.println s!"x: {x}"

  let v ← read_value! Vector Int 3 from source
  IO.println s!"v: {v.toList}"

  let vn ← read_value! Nat from source
  IO.println s!"vn: {vn}"
  let vd ← read_value! Vector Int vn from source
  IO.println s!"vd: {vd.toList}"

  let n1 ← read_value! Nat_1 from source
  IO.println s!"n1: {n1}"

