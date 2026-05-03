module

meta import Lean.Meta
public import Lean.Elab.Term

public import Proconio.Core


public section
namespace Proconio

open Lean Meta Elab Term


@[reducible, expose]
def Arg (α : Type) [Readable α β] := β
instance (priority := 900) [Readable α β] : Readable (Arg α) β where
  read source := Readable.read α source


syntax (name := argioE) "argio!" term "from" term : term

@[term_elab argioE]
meta def elabArgioE : TermElab := fun stx _ => do
  let `(argio! $fn from $source) := stx | throwUnsupportedSyntax

  let fn ← elabTerm fn none
  let fnType ← inferType fn
  let source ← elabTerm source none

  forallTelescope fnType fun args _ => do
    let mut currentExpr := fn
    let mut argActions : Array (Expr × Expr) := #[] -- (arg, readAction)

    for arg in args do
      let argType ← inferType arg

      let innerType := if argType.isAppOf `Arg then argType.getAppArgs[1]! else argType

      let inst ← synthInstance (mkAppN (mkConst ``Readable) #[innerType, (← mkFreshExprMVar none)])

      let readAction ← mkAppOptM
        ``Readable.read
        #[innerType, none, inst, none, none, source]

      currentExpr := mkApp currentExpr arg
      argActions := argActions.push (arg, readAction)

    for (arg, readAction) in argActions.reverse do
      currentExpr ← mkAppOptM
        ``Bind.bind
        #[mkConst ``IO, none, none, none, readAction, ← mkLambdaFVars #[arg] currentExpr]


    return currentExpr

