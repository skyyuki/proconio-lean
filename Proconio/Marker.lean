module

public import Proconio.Basic
public import Proconio.Readable


public section
namespace Proconio

inductive FromFunc (α : Type) {β γ : Type} [Readable α β] (f : β → γ) : Type 
instance {α β γ : Type} [Readable α β] {f : β → γ} : Readable (FromFunc α f) γ where
  read source := Readable.read α source <&> f

/--
For 1-indexed index input, interpreted as Nat - 1.
-/
abbrev Nat_1 : Type := FromFunc Nat (· - 1)
/--
Convert String into `List Char`
-/
abbrev Chars : Type := FromFunc String String.toList
/--
String into `ByteArray`
-/
abbrev Bytes : Type := FromFunc String String.toByteArray
/--
String into `List UInt8`
-/
abbrev BytesList : Type := FromFunc Bytes ByteArray.toList
