let () =
  print_endline ("\n\nWelcome to Hearts<3 !!\n");
  print_endline "Please enter number of human players (int).
Type HELP at any time to read the instructions,
QUIT to end the game,
LEADERBOARD before a game to display the high scores,
and STATS and a username (separated by a space) before a game
to view that player's stats.";
  print_string  "> ";

  Game_engine.run_game ()