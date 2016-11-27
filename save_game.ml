open Yojson.Basic.Util

type hs_player = {name:string;score:int}
type high_score = hs_player list
type player_stats = {name:string;wins:int;losses:int;
                  win_percentage:float;best_score:int;avg_score:float}

(*[file_name_to_json file_name] returns a json of the file
-[file_name] is the file name*)
let rec file_name_to_json file_name =
  Yojson.Basic.from_file file_name

(*[extract_hs_player_info json]
extracts each player's name and score from the [json]*)
let extract_hs_player_info json =
  let player1 = json |> member "player" |> to_string in
  let score1 = json |> member "points" |> to_int in
  {name=player1;score=score1}

(*[extract_high_scores] returns a list of players with the top 5 scores*)
let extract_high_scores =
  let json = file_name_to_json "high_score.json" in
  json |> member "high_scores" |> to_list |> List.map extract_hs_player_info

(*[hs_to_string p] converts a player [p] to a string*)
let hs_to_string p =
  p.name ^ ": " ^ (string_of_int p.score)

(*[display_high_score hs] prints out the leaderboard containing the players
and their corresponding scores
-[hs] is a list of hs_players*)
let rec display_high_score hs = match hs with
  | [] -> ()
  | h::t -> let p = hs_to_string h in print_endline p; display_high_score t;;

(*[update_player_stats username win] will update an individual player's statistics
  including total wins and losses, average win percentage, and best score
  -[username] name of the player to read the corresponding json file(if any)
    and write to
  -[win] boolean if the player won the game
  -[score] how many points the player gained at the end of the game*)
let update_player_stats username win score= 0
  (*1. see if the name.json file exists.
      1. if it does, extract all the info, else just write to a new file
  2. update wins/losses, average win percentage, best score if they won*)

(*[update_wins wins] returns the total number of wins
of a player after he has just won
-[wins] is an int of the number of games a player has won
prior to the current game*)
let update_wins wins = wins + 1

(*[update_losses losses] returns the total number of looses of a player
after he has just lost
-[losses] is an int of the number of games a player
has lost prior to the current game*)
let update_losses losses = losses + 1

(*[avg_win_percentage wins losses] is the player's win percentage
-[wins] is the number of times a player has won
-[losses] is the number of times a player has lost*)
let avg_win_percentage wins losses = (float_of_int wins) /. (float_of_int (wins + losses))

(*[update_best_score score high_score] updates the player's best score
if the score he has earned after winning a game
is lower than his previous score
-[score] is the number of points the player received from his last game
-[high_score] is the all-time high score for that player before this last game*)
let update_best_score score high_score = if score < high_score
  then score else high_score

let update_avg_score score avg_score wins losses =
  let total_games = float_of_int (wins+losses) in
  let total_points = avg_score *. total_games in
  (total_points +. (float_of_int score)) /. (total_games +. 1.0)
  (*AVG SCORE IS A FLOAT*)

let read_player_stats username =
  let json = file_name_to_json (username ^ ".json") in
  let n = json |> member "name" |> to_string in
  let w = json |> member "wins" |> to_int in
  let l = json |> member "losses" |> to_int in
  let win_per = json |> member "win_percentage" |> to_float in
  let bs = json |> member "best_score" |> to_int in
  let avg_s = json |> member "avg_score" |> to_float in
  {name=n;wins=w;losses=l;win_percentage=win_per;best_score=bs;avg_score=avg_s}

let display_player_stats p =
  print_endline ("Name: " ^ p.name);
  print_endline ("Wins: " ^ (string_of_int p.wins));
  print_endline ("Losses: " ^ (string_of_int p.losses));
  print_endline ("Win Percentage: " ^ (string_of_float p.win_percentage));
  print_endline ("Best Score: " ^ (string_of_int p.best_score);
  print_endline ("Average Score: " ^ (string_of_float p.avg_score)





(*let extract_json_info json_tree =
  let rooms1 = json_tree |> member "rooms" |> to_list
                          |> List.map extract_rooms in
  let start_room1 = extract_start_room json_tree in
  let items1 = json_tree |> member "items" |> to_list
                          |> List.map extract_items in
  let start_inv1 = extract_start_inv json_tree in
  let start_locations1 = json_tree |> member "start_locations"
                          |> to_list |> List.map extract_start_locations in
  {rooms=rooms1;start_room=start_room1;items=items1;
  start_inv=start_inv1;start_locations=start_locations1}*)
