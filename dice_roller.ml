open Core.Std

let roll number die modifier () =
  let rolls = Random.self_init ();
    List.init number ~f:(fun _ -> Random.int die + 1)
  in
  let total = match List.reduce rolls ~f:(+) with
    | None -> failwith "Error summing up the dice.\n"
    | Some e -> (e + modifier)
  in
  printf "Dice: %s\nTotal: %d" (List.fold rolls ~init:"" ~f:(fun acc r -> acc^" "^(Int.to_string r))) total 

let command =
  Command.basic
    ~summary:("A simple dice roller written in OCaml.")
    ~readme:(fun () -> "\
This very simple program is an exercise used to learn command-line interfaces\n\
using the `Core.Command` library.\n\n\
This software is distributed under the terms of the GNU Public License.\
    ")
    Command.Spec.(
      empty
      +> flag "-n" (optional_with_default 1 int) ~doc:"NUMBER Number of dice to roll. Defaults to 1."
      +> flag "-d" (optional_with_default 6 int) ~doc:"DIE Number of faces on the dice. Defaults to 6."
      +> flag "-m" (optional_with_default 0 int) ~doc:"MODIFIER Modifier added to the total. Defaults to 0."
    )
    roll

let () =
  Command.run command ~version:"0.1" ~build_info:"ocamlbuild 4.02.3"
