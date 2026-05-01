import Proconio.Basic


structure OnceSource where
  tokens : IO.Ref (List String.Slice)

instance : Source OnceSource where
  nextToken src := do
    match ← src.tokens.get with
    | .nil => throw .unexpectedEof
    | s :: ss =>
      src.tokens.set ss
      return s
  isEmpty src := do
    src.tokens.get <&> List.isEmpty

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


