open Ast
open Playground

(* Live variable analysis *)

module MyInt = struct
  type t = int
  let compare = Pervasives.compare
end

module MyIntSet = Set.Make(MyInt)

module Domain = struct
  include Set.Make(Var)

  let from_varset vs =
    VarSet.fold add vs empty
end

module LV = struct
  include Map.Make(Lab)
end

let analysis program =
  let labels = Playground.labels program in
  let blocks = Playground.blocks program in
  let flowR = Flows.reverse (Playground.flow program) in
  let gen block : Domain.t =
    match block with
    | ASSIGN (_, a, _) ->
      Domain.from_varset (aFV a)
    | SKIP _ -> Domain.empty
    | BEXPR _ -> Domain.empty
    | PRINT _ -> Domain.empty
  in

  let kill block : Domain.t =
    Domain.empty
  in

  let entry lab ex : Domain.t =
    let block_opt = Blocks.find_opt lab blocks in
    match block_opt with
    | Some block ->
      Domain.union
        (Domain.diff (LV.find lab ex) (kill block))
        gen block
    | None -> failwith "failed to find a block"
  in

  let exit lab en : Domain.t =
    Domain.empty
  in
  let entry_init, exit_init = LV.empty, LV.empty in
  let rec aux (en, ex) =
    let en', ex' =
      LabSet.fold (fun lab (en, ex) ->
        LV.add lab (entry lab ex) en,
        LV.add lab (exit lab en) ex
      ) labels (en, ex)
    in
    if en = en' && ex = ex' then en', ex' else aux (en', ex')
  in
  aux (entry_init, exit_init)




