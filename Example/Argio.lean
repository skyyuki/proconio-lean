import Proconio.Argio


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

