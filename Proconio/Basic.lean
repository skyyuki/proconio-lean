module

public section
namespace Proconio

/--
A class that white space seperated token stream wrapping stdin or else.
-/
class Source (σ : Type) where
  nextToken : σ → IO String.Slice


/--
A class that represent how to make a value from token stream.
`Readable α β` means the marker Type `α` can be used to make a value of Type `β`.
-/
class Readable (α : Type) (β : outParam Type) where
  read [Source σ] : (source : σ) → IO β

end Proconio
