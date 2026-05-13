module

public import Proconio.Basic


public section
namespace Proconio

/--
Represent the way to parse a value.
-/
class FromString (α : Type) where
  parse : String.Slice → Option α

instance : FromString String where
  parse := some ∘ String.Slice.toString

instance : FromString Nat where
  parse := String.Slice.toNat?

instance : FromString Int where
  parse := String.Slice.toInt?

instance [FromString α] : Readable α α where
  read source := do
    let token ← Source.nextToken source
    match FromString.parse token with
    | some val => pure val
    | none     => throw <| IO.userError s!"failed to parse from '{token}'"

/- Readable instance definitions -/
instance [Readable α₁ β₁] [Readable α₂ β₂] : Readable (α₁ × α₂) (β₁ × β₂) where
  read source := do
    pure (Prod.mk (← Readable.read α₁ source) (← Readable.read α₂ source))

instance [Readable α β] : Readable (Vector α n) (Vector β n) where
  read source :=
    Vector.ofFnM (fun _ => (Readable.read α source))

instance [Readable α α] (p : α → Prop) [∀ x, Decidable (p x)] : Readable (Subtype p) (Subtype p) where
  read source := do
    let x ← Readable.read α source
    if h : p x then
      pure ⟨x, h⟩
    else
      throw <| IO.userError s!"Did not satisfy subtype requirement"

