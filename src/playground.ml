open Format
open Ast

let fpf = fprintf
let efmt = err_formatter

let rec init s =
  match s with
  | Assign (_, _, _, info) -> Info.lab_of info
  | Skip info -> Info.lab_of info
  | Print (_, info) -> Info.lab_of info
  | While (_, _, info) -> Info.lab_of info
  | If (_, _, _, info) -> Info.lab_of info
  | Seq (s, _) -> init s
  | Filter (_, _, _, info) -> Info.lab_of info
  | Source (_, _, info) -> Info.lab_of info
  | Sink (_, info) -> Info.lab_of info

let rec final s =
  match s with
  | Assign (_, _, _, info) -> LabSet.singleton (Info.lab_of info)
  | Skip info -> LabSet.singleton (Info.lab_of info)
  | Print (_, info) -> LabSet.singleton (Info.lab_of info)
  | While (_, _, info) -> LabSet.singleton (Info.lab_of info)
  | If (_, st, sf, _) -> LabSet.union (final st) (final sf)
  | Seq (_, s) -> final s
  | Filter (_, _, _, info) -> Info.lab_of info |> LabSet.singleton
  | Source (_, _, info) -> Info.lab_of info |> LabSet.singleton
  | Sink (_, info) -> Info.lab_of info |> LabSet.singleton

let rec labels s =
  match s with
  | Assign (_, _, _, info) -> LabSet.singleton (Info.lab_of info)
  | Skip info -> LabSet.singleton (Info.lab_of info)
  | Print (_, info) -> LabSet.singleton (Info.lab_of info)
  | While (_, s, info) ->
    LabSet.add (Info.lab_of info) (labels s)
  | If (_, st, sf, info) ->
    LabSet.add (Info.lab_of info)
    (LabSet.union (labels st) (labels sf))
  | Seq (s1, s2) ->
    LabSet.union (labels s1) (labels s2)
  | Filter (_, _, _, info) -> Info.lab_of info |> LabSet.singleton
  | Source (_, _, info) -> Info.lab_of info |> LabSet.singleton
  | Sink (_, info) -> Info.lab_of info |> LabSet.singleton

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

  let next_of n t =
    fold (fun (p, q) lset ->
      if n = p then
        LabSet.add q lset
      else
        lset
    ) t LabSet.empty

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

  | Filter (_, _, _, info) -> Flows.empty
  | Source (_, _, info) -> Flows.empty
  | Sink (_, info) -> Flows.empty

module CBlocks = struct
  include Set.Make(Ast.CBlock)
end

module DBlocks = struct
  include Map.Make(Lab)

  let add blk t =
    let k = DBlock.lab_of blk in
    let v = blk in
    add k v t

  let load = find
end

let build_cfg s =
  let open CBlock in
  let rec blocks s =
    match s with
    | Assign (x, _, a, info) ->
      let blk = ASSIGN (x, a, info) in
      CBlocks.singleton blk
    | Skip info ->
      let blk = SKIP info in
      CBlocks.singleton blk
    | Print (a, info) ->
      let blk = PRINT (a, info) in
      CBlocks.singleton blk
    | While (b, s, info) ->
      let blk = BEXPR (b, info) in
      CBlocks.add blk (blocks s)
    | If (b, st, sf, info) ->
      let blk = BEXPR (b, info) in
      CBlocks.add blk
        (CBlocks.union (blocks st) (blocks sf))
    | Seq (s1, s2) ->
      CBlocks.union (blocks s1) (blocks s2)
    | Filter (x, _, a, info) ->
      let blk = FILTER (x, a, info) in
      CBlocks.singleton blk
    | Source (x, _, info) ->
      let blk = SOURCE (x, info) in
      CBlocks.singleton blk
    | Sink (a, info) ->
      let blk = SINK (a, info) in
      CBlocks.singleton blk
  in
  blocks s

module RD = struct
  include Map.Make(Var)

  let defined x info t =
    let lset = LabSet.singleton (Info.lab_of info) in
    add x lset t

  let add x info t =
    let lset =
      if mem x t then
        find x t
      else
        LabSet.empty
    in
    let lset = LabSet.add (Info.lab_of info) lset in
    add x lset t

  let merge t1 t2 =
    fold (fun x lset t ->
      LabSet.fold (add x) lset t
    ) t2 t1

  let find x t =
    if mem x t then
      find x t
    else
      LabSet.empty
end

let build_dfg s =
  let open DBlock in
  let def_use_chain infox_opt vars (blks, flows, rd) =
    List.fold_left (fun (blks, flows) (x, info') ->
      let blk = DVAR (x, info') in
      let blks = DBlocks.add blk blks in
      let l' = Info.lab_of info' in
      let flows =
        LabSet.fold (fun l flows ->
          Flows.add (l, l') flows
        ) (RD.find x rd) flows
      in
      let flows =
        match infox_opt with
        | Some infox ->
          Flows.add (l', Info.lab_of infox) flows
        | None -> flows
      in
      blks, flows
    ) (blks, flows) vars
  in
  let open DBlock in
  let rec varsA ?(init=[]) a =
    let rec aux lst a =
      match a with
      | Int _ -> lst
      | Var (x, info) -> (x, info) :: lst
      | ABop (_, a1, a2) ->
        aux (aux lst a1) a2
    in
    aux init a
  and varsB b =
    let rec aux lst b =
      match b with
      | True | False -> lst
      | Not b -> aux lst b
      | BBop (_, b1, b2) -> aux (aux lst b1) b2
      | BRop (_, a1, a2) ->
        varsA ~init:(varsA ~init:lst a1) a2
    in
    aux [] b
  and blocks (s, blks, flows, rd) =
    match s with
    | Assign (x, infox, a, info) ->
      let vars = varsA a in
      let blks, flows =
        def_use_chain (Some infox) vars (blks, flows, rd)
      in
      let blk = DVAR (x, infox) in
      let blks = DBlocks.add blk blks in
      let rd = RD.defined x infox rd in
      blks, flows, rd
    | Skip info ->
      (blks, flows, rd)
    | Print (a, info) ->
      let vars = varsA a in
      let blks, flows =
        def_use_chain None vars (blks, flows, rd)
      in
      (blks, flows, rd)
    | While (b, s, info) ->
      let rd0 = rd in
      let vars = varsB b in
      let blks, flows =
        def_use_chain None vars (blks, flows, rd)
      in
      let blks, flows, rd = blocks (s, blks, flows, rd) in
      let blks, flows =
        def_use_chain None vars (blks, flows, rd)
      in
      let blks, flows, rd = blocks (s, blks, flows, rd) in
      let rd = RD.merge rd0 rd in
      blks, flows, rd
    | If (b, st, sf, info) ->
      let vars = varsB b in
      let blks, flows =
        def_use_chain None vars (blks, flows, rd)
      in
      let blks, flows, rdt = blocks (st, blks, flows, rd) in
      let blks, flows, rdf = blocks (sf, blks, flows, rd) in
      blks, flows, RD.merge rdt rdf
    | Seq (s1, s2) ->
      let blks, flows, rd = blocks (s1, blks, flows, rd) in
      let blks, flows, rd = blocks (s2, blks, flows, rd) in
      blks, flows, rd
    | Filter (x, infox, a, info) ->
      let vars = varsA a in
      let flt_info = Info.duplicate info in
      let flt = DFILTER flt_info in
      let blks = DBlocks.add flt blks in
      let blks, flows =
        def_use_chain (Some flt_info) vars (blks, flows, rd)
      in
      let flows = Flows.add (flt_info, Info.lab_of infox) flows in
      let blk = DVAR (x, infox) in
      let blks = DBlocks.add blk blks in
      let rd = RD.defined x infox rd in
      blks, flows, rd
    | Source (x, infox, info) ->
      let blk = DVAR (x, infox) in
      let blks = DBlocks.add blk blks in
      let rd = RD.defined x infox rd in
      let src_info = Info.duplicate info in
      let src = DSOURCE src_info in
      let blks = DBlocks.add src blks in
      let flows = Flows.add (Info.lab_of src_info, Info.lab_of infox) flows in
      blks, flows, rd
    | Sink (a, info) ->
      let vars = varsA a in
      let sink_info = Info.duplicate info in
      let blks, flows =
        def_use_chain (Some sink_info) vars (blks, flows, rd)
      in
      let sink = DSINK sink_info in
      let blks = DBlocks.add sink blks in
      (blks, flows, rd)
  in
  blocks (s, DBlocks.empty, Flows.empty, RD.empty)

module VarSet = Set.Make(struct type t = x let compare = compare end)

let rec aFV a : VarSet.t =
  match a with
  | Int _ -> VarSet.empty
  | Var (x, _) -> VarSet.singleton x
  | ABop (_, a1, a2) ->
    VarSet.union (aFV a1) (aFV a2)

let rec bFV b : VarSet.t =
  match b with
  | True | False -> VarSet.empty
  | Not b -> bFV b
  | BBop (_, b1, b2) ->
    VarSet.union (bFV b1) (bFV b2)
  | BRop (_, a1, a2) ->
    VarSet.union (aFV a1) (aFV a2)

module Store = Map.Make(struct
  type t = x
  let compare = compare
end)

let rec evalA a store =
  match a with
  | Int n -> n
  | Var (x, _) -> Store.find x store
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
  | Assign (x, _, a, _) ->
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
  | Source (x, _, _) ->
    Store.add x 0 store
  | Filter (x, _, a, _) ->
    Store.add x (evalA a store) store
  | Sink (a, _) ->
    print_int (evalA a store); print_newline();
    store

module Sign = struct
  type t = Even | Odd | Top | Bottom

  let abstraction : int -> t =
    (fun i ->
      if i mod 2 = 0 then Even else Odd
    )

  let plus s1 s2 =
    match s1, s2 with
    | Even, Even -> Even
    | Odd, Even | Even, Odd -> Odd
    | Odd, Odd -> Even
    | Bottom, _ | _, Bottom -> Bottom
    | _ -> Top

  let minus s1 s2 = s1
  let mult s1 s2 = s1
  let div s1 s2 = s1

  let pp fmt t =
    match t with
    | Even -> Format.fprintf fmt "E"
    | Odd -> Format.fprintf fmt "O"
    | Top -> Format.fprintf fmt "T"
    | Bottom -> Format.fprintf fmt "B"
end

let rec evalA a store: Sign.t =
  match a with
  | Int n -> Sign.abstraction n
  | Var (x, _) -> Store.find x store
  | ABop (abop, a1, a2) ->
    (_abop abop) (evalA a1 store) (evalA a2 store)
and _abop abop =
  match abop with
  | Plus -> Sign.plus
  | Minus -> Sign.minus
  | Mult -> Sign.mult
  | Div ->  Sign.div
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
  | Assign (x, _, a, _) ->
    Store.add x (evalA a store) store
  | Skip _ -> store
  | If (b, st, sf, _) ->
    if evalB b store then
      exec st store
    else
      exec sf store
  | Print (a, _) ->
    Format.fprintf Format.std_formatter "%a@."
      Sign.pp (evalA a store);
    store
  | Seq (s1, s2) ->
    exec s2 (exec s1 store)
  | While (b, s', _) ->
    if evalB b store then
      exec s (exec s' store)
    else store
  | Source (x, _, _) ->
    Store.add x Sign.Top store
  | Sink (a, _) ->
    Format.fprintf Format.std_formatter "sink : %a@."
      Sign.pp (evalA a store);
    store
  | Filter (x, _, a, _) ->
    Store.add x (evalA a store) store

let pp_dot fmt (cblks, dblks, cf, df) =
  fpf fmt "digraph {@.";
  CBlocks.iter (fun blk ->
    fpf fmt "  n%a [label=\"%a:%a\"];@."
      Lab.pp (CBlock.lab_of blk)
      Lab.pp (CBlock.lab_of blk)
      CBlock.pp blk
  ) cblks;
  DBlocks.iter (fun lab blk ->
    fpf fmt "  n%a [label=\"%a:%a\" color=\"grey\"];@."
      Lab.pp lab Lab.pp lab DBlock.pp blk
  ) dblks;
  Flows.iter (fun (p, q) ->
    fpf fmt "  n%a -> n%a [color=\"black\"];@." Lab.pp p Lab.pp q
  ) cf;
  Flows.iter (fun (p, q) ->
    fpf fmt "  n%a -> n%a [color=\"blue\"];@." Lab.pp p Lab.pp q
  ) df;
  fpf fmt "}@."

module Visited = struct
  type t = LabSet.t * Lab.t list

  let empty = LabSet.empty, []

  let add n (s, l) =
    LabSet.add n s, n :: l
  
  let mem n (s, _) =
    LabSet.mem n s

  let pp fmt (_, l) =
    let fst = ref true in
    fpf fmt "[";
    List.iter (fun n ->
      if !fst then fst := false else fpf fmt ",";
      fpf fmt "%a" Lab.pp n
    ) (List.rev l);
    fpf fmt "]"
end

let analyzer (dblks, df) =
  let open DBlock in
  let sources =
    DBlocks.fold (fun lab dblk lset ->
      match dblk with
      | DSOURCE _ ->
        LabSet.add lab lset
      | _ -> lset
    ) dblks LabSet.empty
  in
  let rec trace visited n res =
    if Visited.mem n visited then res else
    match DBlocks.load n dblks with
    | DFILTER _ -> res
    | DVAR _ | DSOURCE _ ->
      let nexts = Flows.next_of n df in
      let visited = Visited.add n visited in
      LabSet.fold (trace visited) nexts res
    | DSINK info ->
      let visited = Visited.add n visited in
      visited :: res
  in
  let res =
    LabSet.fold (trace Visited.empty) sources []
  in
  fpf efmt "%i results.@." (List.length res);
  List.iter (fun visited ->
    fpf efmt "%a@." Visited.pp visited
  ) res
