
type n = int
type x = string

module Store = Map.Make(struct
  type t = x
  let compare = compare
end)

type e =
  | Int of n
  | Var of x
  | Plus of e * e

type s =
  | Assign of x * e
  | Skip
  | If of e * s * s
  | Print of e
  | Seq of s * s

let rec eval e store =
  match e with
  | Int n -> n
  | Var x -> Store.find x store
  | Plus (e1, e2) ->
    (eval e1 store) + (eval e2 store)

(* x + 1 : Plus (Var "x", Int 1) *)

let rec exec s store =
  match s with
  | Assign (x, e) ->
    Store.add x (eval e store) store
  | Skip -> store
  | If (e, st, sf) ->
    if eval e store = 0 then
      exec sf store
    else
      exec st store
  | Print e ->
    print_int (eval e store);
    store
  | Seq (s1, s2) ->
    exec s2 (exec s1 store)

(* x := 1; print x :
    Seq(
      Assign ("x", Int 1),
      Print (Var "x")
    )
 *)
