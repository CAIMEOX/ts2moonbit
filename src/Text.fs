module Text

#if FABLE_COMPILER
type StringBuilder (s: string) =
  let mutable s = s
  new () = StringBuilder ("")
  member __.Length = s.Length
  member sb.Append (s': string) = s <- s + s'; sb
  member inline sb.Append (c: char) = sb.Append (string c)
  member inline sb.Append (num: ^n) = sb.Append (sprintf "%d" num)
  member inline sb.Append (o: obj) = sb.Append (string o)
  member inline sb.AppendLine () = sb.Append System.Environment.NewLine
  member inline sb.AppendLine (s: string) = (sb.Append (s)).AppendLine()
  member sb.Remove (startIndex: int, length: int) =
    if startIndex + length >= s.Length
    then s <- s.Substring (0, startIndex)
    else s <- s.Substring (0, startIndex) + s.Substring (startIndex + length)
    sb
  member inline __.ToString (startIndex: int, length: int) =
    s.Substring (startIndex, length)
  override __.ToString() = s
#else
type StringBuilder = System.Text.StringBuilder
#endif

[<StructuredFormatDisplay("{AsString}")>]
type text =
  private
  | Indent of text
  | Concat of text * text
  | Newline
  | Str of string
with
  static member (+) (x, y) = Concat (x, y)
  override this.ToString() = this.ToString(2)
  member this.AsString = this.ToString(0)
  member this.ToString(indentLength: int) =
    let sb = StringBuilder()
    let rec go indent rhs = function
      | Str s ->
        sb.Append s |> ignore;
        match rhs with
        | [] -> sb.ToString()
        | (t, indent) :: rest -> go indent rest t
      | Concat (l, r) ->
        go indent ((r, indent) :: rhs) l
      | Newline ->
        sb.AppendLine() |> ignore;
        match rhs with
        | [] -> sb.ToString()
        | (t, indent) :: rest ->
          sb.Append(String.replicate (indent * indentLength) " ") |> ignore;
          go indent rest t
      | Indent t ->
        sb.Append(String.replicate indentLength " ") |> ignore;
        go (indent + 1) rhs t
    go 0 [] this

let toString indentLength (t: text) = t.ToString(indentLength)

let empty = Str ""

let str (s: string) =
  let lines = s.Split([|System.Environment.NewLine|], System.StringSplitOptions.None)
  match lines.Length with
  | 0 -> empty
  | 1 -> Str lines.[0]
  | _ -> 
    lines |> Array.mapi (fun i x -> if i = 0 then Str x else Newline + Str x)
          |> Array.reduce (+)

let indent x = Indent x

let newline = Newline

let inline (@+) s x = str s + x
let inline (+@) x s = x + str s

let inline tprintf format = Printf.kprintf str format
let inline tprintfn format = Printf.kprintf (fun s -> str s + newline) format

let between l r x = l @+ x +@ r

let join xs =
  match xs with
  | [] -> empty
  | h :: t -> t |> List.fold (+) h

let concat sep xs =
  match xs with
  | [] -> empty
  | h :: t ->
    let rec go result = function
      | [] -> result
      | x :: xs -> go (result + sep + x) xs
    go h t