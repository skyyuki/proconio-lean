module

public import Proconio.Basic
public import Proconio.Readable


public section
namespace Proconio

/--
For 1-indexed index input, interpreted as Nat - 1.
-/
opaque Nat_1 : Type := Nat
instance : Readable Nat_1 Nat where
  read source := Readable.read Nat source <&> (· - 1)

