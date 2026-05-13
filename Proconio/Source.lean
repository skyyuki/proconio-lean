module

public import Proconio.Basic


public section
namespace Proconio

/--
The source that initially reads all the text into a buffer.
-/
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


/--
The source that reads line by line on time for interactive problems.
-/
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
  pure { stream := stream, line_tokens := ← IO.mkRef []}
  
def LineSource.ofStdio : IO LineSource := do
  let stdin ← IO.getStdin
  LineSource.ofStream stdin

def LineSource.read (source : LineSource) (α : Type) [Readable α β] : IO β :=
  Readable.read α source

