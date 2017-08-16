
type n = int
type x = string

type lab = int

module Info = struct
  type t = lab

  let lab_of t = t
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
  | Var of x
  | ABop of abop * a * a
and b =
  | True
  | False
  | Not of b
  | BBop of bbop * b * b
  | BRop of brop * a * a

type s =
  | Assign of x * a * Info.t
  | Skip of Info.t
  | If of b * s * s * Info.t
  | Print of a * Info.t
  | Seq of s * s
  | While of b * s * Info.t
