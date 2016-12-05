open Card
open Player
open Save_game
open AI
(*CARD REPRESENTATION*)

let unwrap_optional_int optional =
	match optional with
	| Some i -> i
	| None -> -1

		(*********************************************)
(* Calculates the number of points a player receives from "winning" the hand*)
let rec score_of_turn (plays: (Player.player_id * Card.card) list) : int =
	match plays with
	| [] -> 0
	| h::t -> Card.point_of_card (snd h) + score_of_turn t

(* Compares two plays of the same suit and returns the (player,card) tuple that is the max*)
let max_play (play1: (Player.player_id * Card.card)) (play2: (Player.player_id * Card.card)) =
	if snd play1 > snd play2 then play1 else play2

(* Given a list of plays of the same suit, return the player that won the turn*)
let rec winner_of_turn (plays: (Player.player_id * Card.card) list) (curr_winner: (Player.player_id * Card.card)) : Player.player_id =
	match plays with
	| [] -> fst curr_winner
	| h::t -> winner_of_turn t (max_play h curr_winner)


(*Returns None if no card has been played this turn or Some x, the first play, otherwise
 * to be used for get_legal_moves*)
let get_first_play (card_lst : Card.card list) : Card.card option =
	match card_lst with
	| [] -> None
	| h::_ -> Some h

(* Returns the legal moves given a card, whether hearts have been broken, and a list of cards
 * If c is None, that means that it's the first card played that turn and the player can play
 * anything if hearts are broken, or any non-heart card if hearts are not broken.
 * If c is Some x, that means this is not the first card played that turn. If the current player
 * has a card that is of x's suit, he must play it. Otherwise, he can play anything if hearts
 * are broken, or any non-heart cart if hearts are not broken.
*)
let get_legal_moves (c : Card.card option) (card_lst : Card.card list) (hearts_broken: bool ref): Card.card list =
	match c with
	| None ->
		if !hearts_broken then card_lst
	else Card.filter_hearts card_lst
	| Some x ->
		(let same_suit_as_c = Card.only_suit x card_lst in
		match same_suit_as_c with
		| [] -> card_lst
		| h::t -> same_suit_as_c)

(*Returns the player who "won" the hand and the number of points received*)
let calculate_turn_result (plays: (Player.player_id * Card.card) list) : (Player.player_id * int) =
	let first_play = List.hd plays in
	let first_card = snd first_play in
	let score = score_of_turn plays in
	let considered_plays = List.filter (fun x -> Card.is_same_suit first_card (snd x)) plays in
	let winner = winner_of_turn considered_plays first_play in
	(winner,score)

let (ai_names: Player.player_id list)= ["Professor Clarkson"; "Professor Oak"; "Professor Constable"; "Professor George"]

(*Creates a record for a human player*)
let initialize_human_players acc (player_name: Player.player_id) =
	let new_player = Player.init in
	let new_player_with_name = Player.set_id new_player player_name in
	acc @ [new_player_with_name]

(*Creates a record for a AI *)
let initialize_ai acc (ai_name: Player.player_id) =
	let new_ai = Player.init_AI in
	let new_ai_with_name = Player.set_id new_ai ai_name in
	acc @ [new_ai_with_name]

(*Creates a sub-list containing the first n elements in lst*)
let rec partial_lst lst (n:int) acc=
	match lst with
	| [] -> List.rev acc
	| h::t -> if n = 0 then List.rev acc
else partial_lst t (n-1) (h::acc)

(*Splits lst into two lists, the first of which is length n*)
let split_lst lst n =
	let rec split_lst_helper i acc = function
	| [] -> List.rev acc, []
	| h::t as l -> if i = 0 then List.rev acc, l
else split_lst_helper (i-1) (h::acc) t
	in
	split_lst_helper n [] lst


(*Creates a shuffled deck of 52 cards*)
let initialize_deck () : Card.card list=
	let rec generate_list_cards x y acc =
		if x > y then acc
		else generate_list_cards (x+1) y (acc @ [x])
	in
	let deck = generate_list_cards 1 52 [] in
	let shuffle d =
		let nd = List.map (fun c -> (Random.bits (), c)) d in
		let sond = List.sort compare nd in
		List.map snd sond
	in
	shuffle deck


(*Given a deck of 52 cards, splits the deck into four 13-card hands*)
let distribute_deck (deck: Card.card list ) : (Card.card list list ) =
	let splitted_deck = split_lst deck 26 in
	let first_half = fst splitted_deck in
	let second_half = snd splitted_deck in

	let first_half_splitted = split_lst first_half 13 in
	let first_hand = fst first_half_splitted in
	let second_hand = snd first_half_splitted in

	let second_half_splitted = split_lst second_half 13 in
	let third_hand = fst second_half_splitted in
	let fourth_hand = snd second_half_splitted in
	first_hand::second_hand::third_hand::fourth_hand::[]


(*Given a list of 4 players/ais and a list of 4 hands, give a hand to each player*)
let rec distribute_hands (players: Player.player list) (hand_list : Card.card list list) (acc: Player.player list) (n: int) : Player.player list =
	match players with
	| [] -> List.rev acc
	| h::t -> (
		match hand_list with
		| [] -> List.rev acc
		| curr::others ->
			let new_player = 
			Player.create_player (Player.get_id h) curr (Player.get_total_score h) (Player.get_round_score h) (Player.get_is_AI h) n
		in distribute_hands t others (new_player::acc) (n+1)
	)

(*Call this method*)
(*Given a list of players and ai names, create player records for each of them*)
let initialize_all_players (player_id_lst: Player.player_id list) (ai_lst: Player.player_id list) : Player.player list =
	let num_players = List.length player_id_lst in
	let num_ais = 4 - num_players in
	let player_lst= List.fold_left initialize_human_players [] player_id_lst in
	let ais = partial_lst ai_lst num_ais [] in
	let ai_lst = List.fold_left initialize_ai [] ais in
	let players_and_ais = player_lst@ai_lst in
	let shuffled_deck = initialize_deck () in
	let four_hands = distribute_deck shuffled_deck in
	distribute_hands players_and_ais four_hands [] 0

(* Position 0 will give cards to position 1
 * position 1 will give cards to position 2
 * position 2 will give cards to position 3
 * position 3 will give cards to position 0*)



(* find_player returns the player record that has this position*)
 let rec find_player (position: int) (player_lst: Player.player list) : Player.player =
 	match player_lst with
 	| [] -> failwith "This position doesn't exist"
 	| h::tl ->
 		if Player.get_position h = position then h
 	else find_player position tl


(* Given a p_id and exchanges, return the list of cards that is associated with that p_id*)
let rec find_exchange (p_id: Player.player_id) (exchanges: (Player.player_id * Card.card list) list ) : Card.card list =
	match exchanges with
	| [] -> failwith "No such doner made a exchange"
	| h::tl -> if fst h = p_id then snd h
	else find_exchange p_id tl

let rec update_hand (p: Player.player) (cards: Card.card list) (player_lst: Player.player list) (res: Player.player list): Player.player list=
	 match player_lst with
	 | [] -> res
	 | h::tl ->
	 	if Player.get_id h = Player.get_id p then
	 	let new_player =
	 	Player.create_player (Player.get_id p) cards (Player.get_total_score p) (Player.get_round_score p) (Player.get_is_AI p) (Player.get_position p)
	 in
	 	update_hand p cards tl (res @ [new_player])
	 else
	 update_hand p cards tl (res @ [h])


(*exchange will take a player, a list of players,  a list of (player_id, card list) tuples, (and a "rule")
 * and return the card list that this player should receive. Returns the new updated player_lst *)
 let single_exchange (p: Player.player) (player_lst: Player.player list) (exchanges: (Player.player_id * Card.card list ) list) : Player.player list =

 	let doner_position = (Player.get_position p + 3) mod 4 in
 	let doner = find_player doner_position player_lst in
 	let doner_id = Player.get_id doner in
 	let doner_donation = find_exchange doner_id exchanges in
 	let receiver_cards = Player.get_cards p in
 	let receiver_plus_new_cards = receiver_cards @ doner_donation in
 	let receiver_donation = find_exchange (Player.get_id p) exchanges in
 	let receiver_minus_donation = Card.remove_cards_from_hand receiver_plus_new_cards receiver_donation in
 	update_hand p receiver_minus_donation player_lst []


(*TEST*)
(* Example test case
player_lst =[
 {cards = [22; 23; 24; 25; 26]; total_score = 0; round_score = 0; player_id = "ellie"; is_AI = false; position = 0};
 {cards = [1; 2; 3; 4; 5; 6; 7]; total_score = 0; round_score = 0; player_id = "bob"; is_AI = false; position = 1};
 {cards = [8; 9; 10; 11; 12; 13; 14; 15]; total_score = 0; round_score = 0; player_id = "charlie"; is_AI = false; position = 2};
 {cards = [16; 17; 18; 19; 20; 21]; total_score = 0; round_score = 0; player_id = "drake"; is_AI = false; position = 3}]

 exchanges = (player_id * card list) list = [("bob", [1; 2; 3]); ("charlie", [8; 9; 10]); ("drake", [16; 17; 18]); ("ellie", [22; 23; 24])]

for the res argument, pass in an identical player_lst


(* Position 0 will give cards to position 1
 * position 1 will give cards to position 2
 * position 2 will give cards to position 3
 * position 3 will give cards to position 0*)

*)
(*Given a list of players and their exchanges, return the new list of players*)
let rec exchange_cards (player_lst: Player.player list ) (exchanges: (Player.player_id * Card.card list ) list ) (res: Player.player list) : Player.player list =
	match player_lst with
	| [] -> res
	| h::tl ->
	exchange_cards tl exchanges (single_exchange h res exchanges)


(* let play_card (p:player) (c:card) =
	if List.mem c get_legal_moves *)

let rec is_valid_exchange (card_lst: Card.card list) (exchange: Card.card list) = 
	match exchange with
	| [] -> true
	| h::tl ->
		(List.mem h card_lst) && is_valid_exchange card_lst tl 
let print_card card = 
	print_endline (" " ^ card)

(*Returns a list of exchanges for the exchange phase*)
let rec exchange_phase (player_lst: Player.player list) (res: (Player.player_id * Card.card list) list ) : ((Player.player_id * Card.card list) list ) = 
	match player_lst with 
	| [] -> res
	| p::tl ->
		if (Player.get_is_AI p) then
		let ai_cards= AI.ai_exchange p in
		exchange_phase tl res @ [ai_cards]
	else (
		let () =  print_endline ("Exchange for: " ^ Player.get_id p  ^ "\n") in
		let currcards = SaveGame.convert_hand_to_string_list (Player.get_cards p) in
		let () = List.iter print_card currcards in 
		let tentative_exchange = SaveGame.cards_to_exchange () in
		if is_valid_exchange (Player.get_cards p) tentative_exchange then
			let p_exchange = (Player.get_id p, tentative_exchange) in
			exchange_phase tl res @ [p_exchange]
		else 
			let () = print_endline "These cards are not in your hand" in
			exchange_phase player_lst res
	)




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

(*[done_with_turn username] prints a bunch of hearts to block the
previous player's hand from sight from the next player*)
let rec done_with_turn username =
    let rec heart_string str num =
      (if num = 0 then str
      else heart_string (str ^ " <3") (num-1)) in
    print_endline (heart_string "" 10000);
    print_endline ""


(*[ready_to_play username] returns true once the player whose turn it is
says he is ready to play*)
let rec ready_to_play username =
  Printf.printf "Enter READY to signal ready to play, %s: " username;
  let input = read_line () in
  if (String.trim (String.uppercase_ascii input)) = "READY" then ()
  else ready_to_play username

(*[input_player_card player_name] returns a tuple of the [player_name]*)
let rec input_player_card (player_name: string) : (Player.player_id * string) =
  Printf.printf "Play a card: ";
  let c = String.trim (read_line ()) in
  let len = String.length c in
  let card = String.sub c 0 1 ^ (String.trim (String.sub c 1 (len-1))) in
  if is_card card then (player_name, card)
  else (print_endline "You didn't enter a real card!"; input_player_card player_name)

(* Asks player for his/her move, continues to call itself until a legal move is entered *)
let rec move_repl (player: Player.player) (played_cards: Card.card option array) (hearts_broken: bool ref) : Card.card =
	let string_card = snd (input_player_card (Player.get_id player)) in
	let card = SaveGame.convert_string_card_to_int string_card in
	let legal_moves = get_legal_moves played_cards.(0) (Player.get_cards player) hearts_broken in
	if (List.mem card legal_moves) then begin
		done_with_turn (Player.get_id player);
		(hearts_broken := if card >= 27 && card <= 39 then true else !hearts_broken);
		card
	end
	else begin
		print_string "Invalid -- ";
		move_repl player played_cards hearts_broken
	end

(* Plays out a turn. It takes in a list of players and goes through each one asking for
	them to input a card. It checks to see if the card the player plays is in his/her hand
	and is a valid move.
*)
let play_turn (player_lst: Player.player list) (round_cards: Card.card list) (hearts_broken: bool ref) : (Player.player * Card.card) list =
	let played_cards = [| None; Some(-1); Some(-1); Some(-1) |] in
	let counter = 0 in
	let play_move player played_cards =
		ready_to_play (Player.get_id player);

		print_string "\nPlayed cards: ";
		(if played_cards.(0) = None then
			print_string "None"
		else
			Array.iter (fun card -> if card = Some(-1) then () else Printf.printf "%s " (Card.rep_card_as_string (unwrap_optional_int card))) played_cards
		);
		print_string "\nHand: ";
		List.iter (fun card -> Printf.printf "%s " (Card.rep_card_as_string card) ) (Player.get_cards player);
		print_endline "\n";

		move_repl player played_cards hearts_broken
	in

	let rec helper lst played_cards counter=
		match lst with
		| [] -> []
		| (p, c)::t -> (* let move = (if p.is_AI then ___ else play_move p played_cards) in *)
						let move = play_move p played_cards in
				  				played_cards.(counter) <- Some move;
				  				let updated_player = Player.set_cards p (Card.remove_card_from_hand (Player.get_cards p) move) in
								[(updated_player, move)] @ (helper t played_cards (counter+1))
	in

let tuple_list = List.map (fun player -> (player, -1)) player_lst in
helper tuple_list played_cards counter



let rec get_first_tuple lst = 
	match lst with
	| [] -> []
	| h::tl -> 
		(fst h ) :: get_first_tuple tl

let rec player_card_to_playerid_card (turn_res: (Player.player * Card.card) list) (res: (Player.player_id * Card.card) list ): (Player.player_id * Card.card) list = 
	match turn_res with
	| [] -> res
	| h::tl ->
		player_card_to_playerid_card tl res @ [( Player.get_id (fst h), snd h)]

let rec remove_card (player_lst: Player.player list) (move: (Player.player_id * Card.card)) (res: Player.player list) : Player.player list =
	match player_lst with
	| [] -> res
	| p::tl ->
		if Player.get_id p = fst move
		then let new_player =
	    Player.create_player (Player.get_id p) (Card.remove_card_from_hand (Player.get_cards p) (snd move) ) (Player.get_total_score p) (Player.get_round_score p) (Player.get_is_AI p) (Player.get_position p)
		in remove_card tl move (res@[new_player])
	else remove_card tl move res @ [p]

let rec update_score_after_turn (player_lst: Player.player list) (turn_result: (Player.player_id * int)) (res: Player.player list) : Player.player list =
	match player_lst with
	| [] -> res
	| p::tl ->
		if (Player.get_id p) = fst turn_result
		then let new_player =

		Player.create_player (Player.get_id p) (Player.get_cards p) (Player.get_total_score p+ (snd turn_result)) (Player.get_round_score p+ (snd turn_result)) (Player.get_is_AI p) (Player.get_position p)
		in update_score_after_turn tl turn_result res @ [new_player]
	else update_score_after_turn tl turn_result res @ [p]

let rec get_position_given_id (player_lst: Player.player list) (id: Player.player_id) : int =
	match player_lst with
	| [] -> failwith "No player has this id"
	| h::tl ->
		if (Player.get_id h) = id
		then (Player.get_position h)
	else get_position_given_id tl id


let rec rearrange_player_list (player_lst: Player.player list) (turn_result: (Player.player_id *int)) : Player.player list =
	let winner_position = get_position_given_id player_lst (fst turn_result) in
	let res = ref [] in
	let counter = ref winner_position in
	for i = 1 to 4 do
	res := !res @ [find_player !counter player_lst]; (counter:= (!counter + 1) mod 4)
	done;
	!res

let rec is_winner (player_lst: Player.player list) : bool = 
	match player_lst with
	| [] -> false
	| h::tl ->
		if (Player.get_total_score h) > 25 then true
	else is_winner tl

let rec extract_playerid_and_score (player_lst: Player.player list) (res: (Player.player_id * int) list) = 
	match player_lst with
	| [] -> res
	| h::tl ->
		extract_playerid_and_score tl res @ [(Player.get_id h, Player.get_total_score h)]

let get_winner (player_lst: Player.player list) : Player.player_id =

	let player_id_and_score = extract_playerid_and_score player_lst [] in
	let sorted_player_id_and_score = List.sort (fun x y -> Pervasives.compare (snd x) (snd y)) player_id_and_score in
	(fst (List.hd sorted_player_id_and_score))




