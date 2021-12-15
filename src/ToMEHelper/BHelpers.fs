module ToMEHelper.BHelpers

let flip f x y = f y x
let trim (x:string) = x.Trim()
let before (delim:string) (x:string) = x.[0.. x.IndexOf delim - 1]
let after (delim:string) (x:string) = x.[x.IndexOf delim + delim.Length ..]
let chunkBy f x =
    let rec loop chunk chunks list =
        match list with
        | [] -> List.rev ((List.rev chunk)::chunks)
        | x::xs when f x && List.isEmpty chunk -> loop [x] chunks xs
        | x::xs when f x -> loop [x] ((List.rev chunk)::chunks) xs
        | x::xs -> loop (x::chunk) chunks xs
    loop [] [] x

module Set =
    let addAll items s =
        (s,items) ||> Seq.fold(fun s v -> Set.add v s)

module Map =
    let addItem k x (m:Map<_,'t list>) =
        if m.ContainsKey k then
            let existing = m.[k]
            m |> Map.add k (x::existing)
        else
            m |> Map.add k [x]

    // merge the data in m2 into m1, using m2's values as additional items in m1's list
    let mergeAsList (m2:Map<'tk,'tv >) (m:Map<'tk, 'tv list>) : Map<'tk, 'tv list> =
        (m,m2)
        ||> Map.fold(fun m k v ->
            m |> addItem k v
        )

module Async =
    let map f x =
        async {
            let! x2 = x
            return f x2
        }

[<Struct>]
type OptionalBuilder =
  member __.Bind(opt, binder) =
    match opt with
    | Some value -> binder value
    | None -> None
  member __.Return(value) =
    Some value
  member __.ReturnFrom x =
    match x with
    | Some (Some x) -> Some x
    | _ -> None

let option = OptionalBuilder()