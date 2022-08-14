open Rs

let () = Util.get_program stdin |> Syntax.show_expr |> print_endline
