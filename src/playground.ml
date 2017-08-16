open Ast

let rec init s =
  match s with
  | Assign (_, _, info) -> Info.lab_of info
  | Skip info -> Info.lab_of info
  | Print (_, info) -> Info.lab_of info
  | While (_, _, info) -> Info.lab_of info
  | If (_, _, _, info) -> Info.lab_of info
  | Seq (s, _) -> init s

module Lab = struct
  type t = int
  let compare = compare
  let pp fmt t =
    fpf fmt "%i" t
end
module LabSet = struct
  include Set.Make(Lab)

  let pp fmt t =
    fpf fmt "{";
    let fst = ref true in
    iter (fun l ->
      if !fst then fst := false else fpf fmt ", ";
      fpf fmt "%a" Lab.pp l
    ) t;
    fpf fmt "}"
end

let rec final s =
  match s with
  | Assign (_, _, info) -> LabSet.singleton (Info.lab_of info)
  | Skip info -> LabSet.singleton (Info.lab_of info)
  | Print (_, info) -> LabSet.singleton (Info.lab_of info)
  | While (_, _, info) -> LabSet.singleton (Info.lab_of info)
  | If (_, st, sf, _) -> LabSet.union (final st) (final sf)
  | Seq (_, s) -> final s

let rec labels s =
  match s with
  | Assign (_, _, info) -> LabSet.singleton (Info.lab_of info)
  | Skip info -> LabSet.singleton (Info.lab_of info)
  | Print (_, info) -> LabSet.singleton (Info.lab_of info)
  | While (_, s, info) ->
    LabSet.add (Info.lab_of info) (labels s)
  | If (_, st, sf, info) ->
    LabSet.add (Info.lab_of info)
    (LabSet.union (labels st) (labels sf))
  | Seq (s1, s2) ->
    LabSet.union (labels s1) (labels s2)

module Flow = struct
  type t = Lab.t * Lab.t
  let compare = compare

  let pp fmt (f, t) =
    fpf fmt "(%a, %a)" Lab.pp f Lab.pp t
end
module Flows = struct
  include Set.Make(Flow)

  let reverse t =
    fold (fun (l, l') fs ->
      add (l', l) fs
    ) t empty

  let pp fmt t =
    fpf fmt "{";
    let fst = ref true in
    iter (fun l ->
      if !fst then fst := false else fpf fmt ", ";
      fpf fmt "%a" Flow.pp l
    ) t;
    fpf fmt "}"
end

let rec flow s =
  match s with
  | Assign _ -> Flows.empty
  | Skip _ -> Flows.empty
  | Print _ -> Flows.empty
  | While (_, s, info) ->
    let f1 = flow s in
    let l = Info.lab_of info in
    let f2 = Flows.add (l, init s) f1 in
    LabSet.fold (fun final_of_s fs ->
      Flows.add (final_of_s, l) fs
    ) (final s) f2

  | If (_, st, sf, info) ->
    let f1 = Flows.union (flow st) (flow sf) in
    let l = Info.lab_of info in
    let f2 = Flows.add (l, init st) f1 in
    Flows.add (l, init sf) f2

  | Seq (s1, s2) ->
    let f1 = Flows.union (flow s1) (flow s2) in
    let init_of_s2 = init s2 in
    LabSet.fold (fun final_of_s1 fs ->
      Flows.add (final_of_s1, init_of_s2) fs
    ) (final s1) f1

module Store = Map.Make(struct
  type t = x
  let compare = compare
end)

let rec evalA a store =
  match a with
  | Int n -> n
  | Var x -> Store.find x store
  | ABop (abop, a1, a2) ->
    (_abop abop) (evalA a1 store) (evalA a2 store)
and _abop abop =
  match abop with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div ->  (/)
and evalB b store =
  match b with
  | True -> true
  | False -> false
  | Not b -> not (evalB b store)
  | BBop (bbop, b1, b2) ->
    (_bbop bbop) (evalB b1 store) (evalB b2 store)
  | BRop (brop, a1, a2) ->
    (_brop brop) (evalA a1 store) (evalA a2 store)
and _bbop bbop =
  match bbop with
  | And -> (&&)
  | Or -> (||)
and _brop brop =
  match brop with
  | Eq  -> (=)
  | Neq -> (<>)
  | Lt  -> (<)
  | Gt  -> (>)
  | Le  -> (<=)
  | Ge  -> (>=)

let rec exec s store =
  match s with
  | Assign (x, a, _) ->
    Store.add x (evalA a store) store
  | Skip _ -> store
  | If (b, st, sf, _) ->
    if evalB b store then
      exec st store
    else
      exec sf store
  | Print (a, _) ->
    print_int (evalA a store); print_newline();
    store
  | Seq (s1, s2) ->
    exec s2 (exec s1 store)
  | While (b, s', _) ->
    if evalB b store then
      exec s (exec s' store)
    else store
