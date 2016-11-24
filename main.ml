let () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  (*let file_name = read_line () in
  Game.main file_name*)
