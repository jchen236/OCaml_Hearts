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
let hs_to_string (p:hs_player) =
  p.name ^ ": " ^ (string_of_int p.score)

(*[display_high_score hs] prints out the leaderboard containing the players
and their corresponding scores
-[hs] is a list of hs_players*)
let rec display_high_score hs = match hs with
  | [] -> ()
  | h::t -> let p = hs_to_string h in print_endline p; display_high_score t

(*[update_player_stats username win]
  will update an individual player's statistics
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
let avg_win_percentage wins losses =
  (float_of_int wins) /. (float_of_int (wins + losses))

(*[update_best_score score high_score] returns the player's new best score
if the score he has earned after winning a game
is lower than his previous score
-[score] is the number of points the player received from his last game
-[high_score] is the all-time high score for that player before this last game*)
let update_best_score score high_score = if score < high_score
  then score else high_score

(*[update_avg_score score avg_score wins losses]
returns the player's new average score*)
let update_avg_score score avg_score wins losses =
  let total_games = float_of_int (wins+losses) in
  let total_points = avg_score *. total_games in
  (total_points +. (float_of_int score)) /. (total_games +. 1.0)

(*[read_player_stats username] returns a record of type player_stats
from a json_file*)
let read_player_stats username =
  let json = file_name_to_json (username ^ ".json") in
  let n = json |> member "name" |> to_string in
  let w = json |> member "wins" |> to_int in
  let l = json |> member "losses" |> to_int in
  let win_per = json |> member "win_percentage" |> to_float in
  let bs = json |> member "best_score" |> to_int in
  let avg_s = json |> member "avg_score" |> to_float in
  {name=n;wins=w;losses=l;win_percentage=win_per;best_score=bs;avg_score=avg_s}

(*[display_player_stats p] prints the player's statistics
-[p] is of type player_stats *)
let display_player_stats p =
  print_endline ("Name: " ^ p.name);
  print_endline ("Wins: " ^ (string_of_int p.wins));
  print_endline ("Losses: " ^ (string_of_int p.losses));
  print_endline ("Win Percentage: " ^ (string_of_float p.win_percentage));
  print_endline ("Best Score: " ^ (string_of_int p.best_score));
  print_endline ("Average Score: " ^ (string_of_float p.avg_score))

(*[create_new_json_file username] creates a new json file for a new player*)
let create_new_json_file username =
  let stats:Yojson.Basic.json = `Assoc [("name", `String username);
  ("wins", `Int 0);("losses", `Int 0); ("win_percentage", `Float 0.0);
  ("best_score", `Int 100); ("avg_score", `Float 0.0)] in
  Yojson.Basic.to_file (username ^ ".json") stats

(*[reset_existing_json username] resets the existing json file
for [username] to the initial stats*)
let reset_existing_json username =
  create_new_json_file username

(*update_existing_json username won score] updates an existing json
that stores an individual player's statistics
-[username] is a string of the player's name
-[won] is a boolean whether or not the player won the most recent game
-[score] is the player's most recent game's score*)
let update_existing_json username won score =
  let p = read_player_stats username in
  let wins = if won then p.wins + 1 else p.wins in
  let losses = if not won then p.losses + 1 else p.losses in
  let win_percent = avg_win_percentage wins losses in
  let bs = update_best_score score p.best_score in
  let a_score = update_avg_score score p.avg_score p.wins p.losses in
  let new_stats:Yojson.Basic.json = `Assoc [("name", `String username);
  ("wins", `Int wins);("losses", `Int losses);
  ("win_percentage", `Float win_percent); ("best_score", `Int bs);
  ("avg_score", `Float a_score)] in
  Yojson.Basic.to_file (username ^ ".json") new_stats

(*[is_suit s] returns a boolean if the string is a valid suit (C,D,S,H)*)
let is_suit s =
  match s with
  |"C" |"D" |"S" |"H" -> true
  | _ -> false

(*[is_rank r] returns a boolean if the string is a valid rank (2-10,J,Q,K,A)*)
let is_rank r =
  match r with
  |"2" |"3" |"4" |"5" |"6" |"7" |"8" |"9" |"10" |"J" |"Q" |"K" |"A" -> true
  | _ -> false

(*[is_card card] returns a boolean
if the string is a valid representation of a card*)
let is_card card =
  if String.length card <> 2 && String.length card <> 3 then false
  else
    let suit = String.sub card 0 1 in
    let rank =  String.sub card 1 (String.length card - 1) in
    is_suit suit && is_rank rank

(*[input_player_card player_name] returns a tuple of the [player_name]*)
let rec input_player_card player_name =
  let () = print_endline "Play a card:" in
  let c = String.trim (read_line ()) in
  let len = String.length c in
  let card = String.sub c 0 1 ^ (String.trim (String.sub c 1 (len-1))) in
  if is_card card then (player_name, card)
  else (print_endline "You didn't enter a card!";input_player_card player_name)

(*[get_names n lst] returns a list of unique names of the human players
-[n] is an int that represents how many more people to ask for their names
-[lst] is the list of player names*)
let rec get_names n lst =
  if n <= 0 then lst
  else
    let () = print_endline
    "Enter your name (letters,numbers,and underscores/spaces only please):" in
    let name = String.trim (read_line ()) in
      if (List.mem name lst)
      then (let () = print_endline "Please enter a different name: " in
        get_names n lst)
      else get_names (n-1) (name::lst)

(*[get_human_players] asks how many human players there will be for the game
and returns a list of human player names*)
let rec get_human_players () =
  let () = print_endline "How many players? (enter an int between 1-4): " in
  try (
    let num_players = int_of_string (String.trim (read_line ())) in
    if (num_players >= 1 && num_players <= 4) then get_names num_players []
    else (print_endline "Enter a valid int"; get_human_players ()))
    with |_ -> let ()=print_endline "Enter a valid int" in get_human_players ()

(*[cards_to_exchange ()] prompts the user to enter 3 cards to exchange,
and if they are valid (they are all unique
and are cards of a standard 52 card deck),
returns a list of 3 strings representing valid cards*)
let rec cards_to_exchange () =
  let () =
    print_endline "Pick Three Cards to Exchange: (separate cards by commas)" in
  let input = String.trim (read_line ()) in
  let first_comma = String.index input ',' in
  let second_comma = String.index_from input (first_comma+1) ',' in
  let card1 = String.sub input 0 1 ^ (String.trim
                                      (String.sub input 1 (first_comma-1))) in
  let card2_input = String.trim
            (String.sub input (first_comma+1) (second_comma-first_comma-1)) in
  let card2 = String.sub card2_input 0 1 ^
    (String.trim (String.sub card2_input 1 (String.length card2_input - 1))) in
  let card3_input = String.trim
    (String.sub input (second_comma+1) (String.length input-second_comma-1)) in
  let card3 = String.sub card3_input 0 1 ^
  (String.trim (String.sub card3_input 1 (String.length card3_input - 1))) in
  if (is_card card1 && is_card card2 && is_card card3 &&
    card1 <> card2 && card2 <> card3) then [card1;card2;card3]
  else let () = print_endline "You didn't enter valid cards" in
  cards_to_exchange ()

(* representation of a card
 * cards will be represented by
 * 1-13 is Diamonds
 * - 1 -> 2 of Diamonds
 * - 13 -> Ace of Diamonds
 * 14-26 is Clubs
 * 27-39 is Hearts
 * 40-52 is Spades *)
(*[rep_card_as_string card] returns the string representation of a card.
e.g. if card = 3, "D4" would be returned*)
let rep_card_as_string card =
  let suit = if card <= 13 then "D"
              else if card <= 26 then "C"
              else if card <= 39 then "H"
              else "S" in
  let num = ((card-1) mod 13) in
  let rank = (match num with
  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 -> string_of_int (num + 2)
  | 9 -> "J"
  | 10 -> "Q"
  | 11 -> "K"
  | 12 -> "A") in (suit ^ rank)

(*[convert_hand_to_string_list cards]
returns a list of cards represented as strings
-[cards] is a "hand" containing a list of ints (representing cards) *)
let rec convert_hand_to_string_list cards =
  match cards with
  | [] -> []
  | h::t -> (rep_card_as_string h)::(convert_hand_to_string_list t)