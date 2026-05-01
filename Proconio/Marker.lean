import Proconio.Basic
import Proconio.Readable

/--
For 1-indexed index input, interpreted as Nat - 1.
-/
def Nat_1 := Nat
instance : Readable Nat_1 Nat where
  read source := Readable.read Nat source <&> (· - 1)

