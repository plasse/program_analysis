open Format
open Ast
open Playground

let _ =
  let filename = Sys.argv.(1) in
  let chnl = open_in filename in
  let lexbuf = Lexing.from_channel chnl in
  let s = Parser.s Lexer.token lexbuf in
  fpf err_formatter "AST :@.%a@." Ast.pp_s s;
  (* let init = init s in *)
  (* fpf sfmt "INIT : %i@." init; *)
  (* let final = final s in *)
  (* fpf sfmt "FINAL : %a@." LabSet.pp final; *)
  (* let labels = labels s in *)
  (* fpf sfmt "LABELS : %a@." LabSet.pp labels; *)
  (* let flows = flow s in *)
  (* fpf sfmt "FLOWS : %a@." Flows.pp flows; *)
  (* let flowr = Flows.reverse flows in *)
  (* fpf sfmt "FLOWR : %a@." Flows.pp flowr; *)
  (* Playground.exec s Playground.Store.empty *)
  let cblks = build_cfg s in
  let cf = flow s in
  let dblks, df, _ = build_dfg s in
  let chnl = open_out_bin (filename ^ ".dot") in
  let fmt = Format.formatter_of_out_channel chnl in
  fpf fmt "%a" pp_dot (cblks, dblks, cf, df);
  let _ = close_out chnl in
  let res = analyzer (dblks, df) in
  let _ = pp_res err_formatter res in
  let ss = Playground.mutator s in
  SSet.fold (fun s i ->
    fpf std_formatter "[%i]@.%a@." i pp_s s;
    i + 1
  ) ss 1 |> ignore

