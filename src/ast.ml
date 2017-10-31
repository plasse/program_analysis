open Format

let fpf = fprintf
let sfmt = std_formatter

type n = int
type x = string

module Var = struct
  type t = x
  let compare = compare
end

type lab = int

module Lab = struct
  type t = lab
  let compare = compare
  let pp fmt t =
    fpf fmt "%i" t

  let cnt = ref 0

  let get_lab() =
    incr cnt;
    !cnt

  let duplicate _ = get_lab ()
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

module Info = struct
  type t = lab

  let lab_of t = t
  let pp fmt t =
    fpf fmt "%s%i" "@" t

  let duplicate t =
    lab_of t |> Lab.duplicate
end

type abop =
  | Plus
  | Minus
  | Mult
  | Div

type bbop =
  | And
  | Or

type brop =
  | Eq   (* == *)
  | Neq  (* != *)
  | Lt   (* < *)
  | Gt   (* > *)
  | Le   (* <= *)
  | Ge   (* >= *)

type a =
  | Int of n
  | Var of x * Info.t
  | ABop of abop * a * a
and b =
  | True
  | False
  | Not of b
  | BBop of bbop * b * b
  | BRop of brop * a * a

type s =
  | Assign of x * Info.t * a * Info.t
  | Skip of Info.t
  | If of b * s * s * Info.t
  | Print of a * Info.t
  | Seq of s * s
  | While of b * s * Info.t
  | Filter of x * Info.t * a * Info.t
  | Source of x * Info.t * Info.t
  | Sink of a * Info.t

module DBlock = struct
  type t =
    | DFILTER of Info.t
    | DVAR of x * Info.t
    | DSOURCE of Info.t
    | DSINK of Info.t

  let compare = Pervasives.compare

  let info_of t =
    match t with
    | DFILTER info -> info
    | DVAR (_, info) -> info
    | DSOURCE info -> info
    | DSINK info -> info

  let lab_of t = info_of t |> Info.lab_of

  let pp fmt t =
    match t with
    | DFILTER info -> fprintf fmt "FILTER"
    | DVAR (x, info) -> fprintf fmt "VAR.%s" x
    | DSOURCE info -> fprintf fmt "SOURCE"
    | DSINK info -> fprintf fmt "SINK"
end

module CBlock = struct
  type t =
    | ASSIGN of x * a * Info.t
    | SKIP of Info.t
    | BEXPR of b * Info.t
    | PRINT of a * Info.t
    | FILTER of x * a * Info.t
    | SOURCE of x * Info.t
    | SINK of a * Info.t

  let lab_of t =
    let info =
      match t with
      | ASSIGN (_, _, info) -> info
      | SKIP info -> info
      | BEXPR (_, info) -> info
      | PRINT (_, info) -> info
      | FILTER (_, _, info) -> info
      | SOURCE (_, info) -> info
      | SINK (_, info) -> info
    in
    Info.lab_of info

  let pp fmt t =
    match t with
    | ASSIGN (_, _, info) ->
      fprintf fmt "ASSIGN"
    | SKIP info ->
      fprintf fmt "SKIP"
    | BEXPR (_, info) ->
      fprintf fmt "BE"
    | PRINT (_, info) ->
      fprintf fmt "PRINT"
    | FILTER (_, _, info) ->
      fprintf fmt "FILTER"
    | SOURCE (_, info) ->
      fprintf fmt "SOURCE"
    | SINK (_, info) ->
      fprintf fmt "SINK"

  let compare = Pervasives.compare
end

let rec pp_a fmt a =
  match a with
  | Int n -> fpf fmt "%d" n
  | Var (x, info) -> fpf fmt "%s:%a" x Lab.pp (Info.lab_of info)
  | ABop (abop, a1, a2) ->
    fpf fmt "%a %a %a" pp_a a1 pp_abop abop pp_a a2
and pp_abop fmt abop =
  match abop with
  | Plus -> fpf fmt "+"
  | Minus -> fpf fmt "-"
  | Mult -> fpf fmt "*"
  | Div -> fpf fmt "/"
and pp_b fmt b =
  match b with
  | True -> fpf fmt "true"
  | False -> fpf fmt "false"
  | Not b -> fpf fmt "!%a" pp_b b
  | BBop (bbop, b1, b2) ->
    fpf fmt "%a %a %a" pp_b b1 pp_bbop bbop pp_b b2
  | BRop (brop, a1, a2) ->
    fpf fmt "%a %a %a" pp_a a1 pp_brop brop pp_a a2
and pp_bbop fmt bbop =
  match bbop with
  | And -> fpf fmt "&&"
  | Or -> fpf fmt "||"
and pp_brop fmt brop =
  match brop with
  | Eq -> fpf fmt "=="
  | Neq -> fpf fmt "!="
  | Lt -> fpf fmt "<"
  | Gt -> fpf fmt ">"
  | Le -> fpf fmt "<="
  | Ge -> fpf fmt ">="
and pp_s fmt s =
  match s with
  | Assign (x, infox, a, info) ->
    fpf fmt "@[<hov 2>[%s:%a := %a]%a@]" x Info.pp infox pp_a a Info.pp info
  | Skip info ->
    fpf fmt "@[<hov 2>[skip]%a@]" Info.pp info
  | If (b, st, sf, info) ->
    fpf fmt "@[<hov 2>if %a then@." pp_b b;
    fpf fmt "@[<hov 2>%a@]@." pp_s st;
    fpf fmt "else@.";
    fpf fmt "@[<hov 2>%a@]@." pp_s sf;
    fpf fmt "fi@]"
  | Print (a, info) ->
    fpf fmt "@[<hov 2>[print %a]%a@]" pp_a a Info.pp info
  | Seq (s1, s2) ->
    fpf fmt "%a;@.%a" pp_s s1 pp_s s2
  | While (b, s, info) ->
    fpf fmt "@[<hov 2>while %a do %a@." pp_b b Info.pp info;
    fpf fmt "@[<hov 2>%a@]@." pp_s s;
    fpf fmt "done@]"
  | Source (x, infox, info) ->
    fpf fmt "@[<hov 2>[%s:%a := source()]%a@]" x
      Info.pp infox Info.pp info
  | Filter (x, infox, a, info) ->
    fpf fmt "@[<hov 2>[%s:%a := filter(%a)]%a@]"
      x Info.pp infox pp_a a Info.pp info
  | Sink (a, info) ->
    fpf fmt "@[<hov 2>[sink(%a)]%a@]" pp_a a Info.pp info