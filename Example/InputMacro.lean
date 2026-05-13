import Proconio.Macro


open Proconio

def main : IO Unit := do
  input! {
    from ← OnceSource.ofStdin,
    n : Nat,
    m : Nat,
    s : String,
    p : Int × Int,
    v : Vector Int 3,
    vn : Nat,
    vd : Vector Int vn,
    n1 : Nat_1
  }

  IO.println s!"n: {n}, m: {m}"
  IO.println s!"s: {s}"
  IO.println s!"p: {p}"
  IO.println s!"v: {v.toList}"
  IO.println s!"vn: {vn}"
  IO.println s!"vd: {vd.toList}"
  IO.println s!"n1: {n1}"

