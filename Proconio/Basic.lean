module

public section
namespace Proconio

class Source (σ : Type) where
  nextToken : σ → IO String.Slice
  isEmpty : σ → IO Bool


class Readable (α : Type) (β : outParam Type) where
  read [Source σ] : (source : σ) → IO β

