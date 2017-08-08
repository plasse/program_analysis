
let _ =
  let filename = Sys.argv.(1) in
  let chnl = open_in filename in
  let lexbuf = Lexing.from_channel chnl in
  let s = Parser.s Lexer.token lexbuf in
  Playground.exec s Playground.Store.empty