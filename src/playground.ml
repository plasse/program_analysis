
type n = int
type x = string

module Store = Map.Make(struct
  type t = x
  let compare = compare
end)

type bop =
  | Plus
  | Minus
  | Mult
  | Div

type e =
  | Int of n
  | Var of x
  | Bop of bop * e * e

type s =
  | Assign of x * e
  | Skip
  | If of e * s * s
  | Print of e
  | Seq of s * s
  | While of e * s

let rec eval e store =
  match e with
  | Int n -> n
  | Var x -> Store.find x store
  | Bop (bop, e1, e2) ->
    (_bop bop) (eval e1 store) (eval e2 store)
and _bop bop =
  match bop with
  | Plus -> (+)
  | Minus -> (-)
  | Mult -> ( * )
  | Div ->  (/)

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
  | While (e, s') ->
    if eval e store = 0 then store
    else exec s (exec s' store)
