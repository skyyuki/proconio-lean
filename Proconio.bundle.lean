
/-
proconio 0.1.0 bundle
https://github.com/skyyuki/proconio-lean
MIT License
-/
import Lean.Parser
import Lean.Meta
import Lean.Elab.Term
-- >>> from proconio-lean/Proconio/Basic.lean
namespace Proconio
class Source (σ : Type) where
  nextToken : σ → IO String.Slice
class Readable (α : Type) (β : outParam Type) where
  read [Source σ] : (source : σ) → IO β
-- >>> from proconio-lean/Proconio/Readable.lean
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
    | none => throw <| IO.userError s!"failed to parse from '{token}'"
instance [Readable α₁ β₁] [Readable α₂ β₂] : Readable (α₁ × α₂) (β₁ × β₂) where
  read source := do
    pure (Prod.mk (← Readable.read α₁ source) (← Readable.read α₂ source))
instance [Readable α β] : Readable (Vector α n) (Vector β n) where
  read source :=
    Vector.ofFnM (fun _ => (Readable.read α source))
instance [Readable α α] (p : α → Prop) [∀ x, Decidable (p x)]
    : Readable (Subtype p) (Subtype p) where
  read source := do
    let x ← Readable.read α source
    if h : p x then
      pure ⟨x, h⟩
    else
      throw <| IO.userError s!"Did not satisfy subtype requirement"
-- >>> from proconio-lean/Proconio/Source.lean
structure OnceSource where
  tokens : IO.Ref (List String.Slice)
instance : Source OnceSource where
  nextToken src := do
    match ← src.tokens.get with
    | .nil => throw .unexpectedEof
    | s :: ss =>
      src.tokens.set ss
      return s
def OnceSource.ofString (s : String) : IO OnceSource := do
  let tokens := s.split Char.isWhitespace |>.filter (!·.isEmpty) |>.toList
  let tokens ← IO.mkRef tokens
  pure { tokens := tokens }
def OnceSource.ofStream (stream : IO.FS.Stream) : IO OnceSource := do
  let buf ← stream.readToEnd
  OnceSource.ofString buf
def OnceSource.ofStdin : IO OnceSource := do
  let stdin ← IO.getStdin
  OnceSource.ofStream stdin
def OnceSource.read (source : OnceSource) (α : Type) [Readable α β] : IO β :=
  Readable.read α source
structure LineSource where
  stream : IO.FS.Stream
  line_tokens : IO.Ref (List String.Slice)
instance : Source LineSource where
  nextToken src := do
    if (← src.line_tokens.get).isEmpty then
      let buf ← src.stream.getLine
      let tokens := buf.split Char.isWhitespace |>.filter (!·.isEmpty) |>.toList
      src.line_tokens.set tokens
    match ← src.line_tokens.get with
    | .nil => throw .unexpectedEof
    | s :: ss =>
      src.line_tokens.set ss
      return s
def LineSource.ofStream (stream : IO.FS.Stream) : IO LineSource := do
  pure { stream := stream, line_tokens := ← IO.mkRef [] }
def LineSource.ofStdio : IO LineSource := do
  let stdin ← IO.getStdin
  LineSource.ofStream stdin
def LineSource.read (source : LineSource) (α : Type) [Readable α β] : IO β :=
  Readable.read α source
-- >>> from proconio-lean/Proconio/Marker.lean
inductive FromFunc (α : Type) {β γ : Type} [Readable α β] (f : β → γ) : Type
instance {α β γ : Type} [Readable α β] {f : β → γ} : Readable (FromFunc α f) γ where
  read source := Readable.read α source <&> f
abbrev Nat_1 : Type := FromFunc Nat (· - 1)
abbrev Chars : Type := FromFunc String String.toList
abbrev Bytes : Type := FromFunc String String.toByteArray
abbrev BytesList : Type := FromFunc Bytes ByteArray.toList
-- >>> from proconio-lean/Proconio/Macro.lean
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
-- >>> from proconio-lean/Proconio/Argio.lean
open Lean Meta Elab Term
@[reducible, expose]
def Arg (α : Type) [Readable α β] := β
syntax (name := argioE) "argio!" term "from" term : term
@[term_elab argioE]
meta def elabArgioE : TermElab := fun stx _ => do
  let `(argio! $fn from $source) := stx | throwUnsupportedSyntax
  let fn ← elabTerm fn none
  let fnType ← inferType fn
  let source ← elabTerm source none
  forallTelescope fnType fun args res => do
    let mut currentExpr := fn
    let mut argActions : Array (Expr × Expr) := #[]
    for arg in args do
      let argType ← inferType arg
      let innerType := if argType.isAppOf ``Arg then argType.getAppArgs[1]! else argType
      let inst ← synthInstance (mkAppN (mkConst ``Readable) #[innerType, (← mkFreshExprMVar none)])
      let readAction ← mkAppOptM
        ``Readable.read
        #[innerType, none, inst, none, none, source]
      currentExpr := mkApp currentExpr arg
      argActions := argActions.push (arg, readAction)
    if !(← isDefEq res <| mkApp (mkConst ``IO) (← mkFreshExprMVar none)) then
      currentExpr ← mkAppOptM ``pure #[(mkConst ``IO), none, none, currentExpr]
    for (arg, readAction) in argActions.reverse do
      currentExpr ← mkAppOptM
        ``Bind.bind
        #[mkConst ``IO, none, none, none, readAction, ← mkLambdaFVars #[arg] currentExpr]
    return currentExpr
-- >>> from proconio-lean/Proconio/Core.lean
end Proconio
