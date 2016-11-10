open Core.Std

let roll number die modifier dice_only total_only =
  let rolls = Random.self_init ();
    List.init number ~f:(fun _ -> Random.int die + 1)
  in
  let total = match List.reduce rolls ~f:(+) with
    | None -> failwith "Error summing up the dice.\n"
    | Some e -> (e + modifier)
  in
  let rolls_str = List.fold rolls ~init:"" ~f:(fun acc r -> acc^" "^(Int.to_string r)) in
  if dice_only then
    printf "%s\n" rolls_str
  else if total_only then
    printf "%d\n" total
  else
    printf "Dice: %s\nTotal: %d\n" rolls_str total 

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
      +> flag "-D" no_arg ~doc:" Only prints the dice results. Can't be used with -T."
      +> flag "-T" no_arg ~doc:" Only prints the total of the dice. Can't be used with -D."
    )
    (fun number die modifier dice_only total_only () ->
      if number <= 0 then failwith "The number of dice must be positive.";
      if die <= 0 then failwith "The die must have a positive number of faces.";
      if dice_only && total_only then failwith "Can't used both -D and -T.";
      roll number die modifier dice_only total_only
    )

let () =
  Command.run command ~version:"0.2" ~build_info:"ocamlbuild 4.02.3"
