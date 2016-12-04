let () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to Hearts<3 !!\n");
  print_endline "Please enter number of human players (int).
Type HELP at any time to read the instructions,
QUIT to end the game,
and LEADERBOARD/STATS before or after a game
to view the top scores/an individual player's stats";
  print_string  "> ";
  (*let file_name = read_line () in
  Game.main file_name*)
