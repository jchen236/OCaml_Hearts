(*CARD REPRESENTATION*)

(* representation of a card
 * cards will be represented by 
 * 1-13 is Diamonds
 * - 1 -> 2 of Diamonds
 * - 13 -> Ace of Diamonds
 * 14-26 is Clubs
 * 27-39 is Hearts
 * 40-52 is Spades *)

 type card = int
 type hand = card list 
 type player_id = string

(* Calculates the bounds for the suit of this card. The bounds are inclusive*)
let suit_bounds c : (int * int)= 
	let (lowerbound:int) = (c-1)/13 in
	let (upperbound:int) = (c+12) / 13 in
	(lowerbound*13+1, upperbound*13)

(* Returns a hand without any hearts *)
let filter_hearts (card_lst: hand) : card list= 
	List.filter (fun x -> x < 27 || x > 39) card_lst

(*Returns if c2 is of the same suit as c1*)
let is_same_suit (c1:card) (c2:card) = 
	let lowerbound = fst (suit_bounds c1) in
	let upperbound = snd (suit_bounds c1) in
	(c2 >= lowerbound && c2 <= upperbound)


(* Returns a hand that only contains cards of the same suit as c*)
let only_suit (c: card) (card_lst: hand) : card list = 
	List.filter( fun x -> is_same_suit c x) card_lst

let point_of_card (c:card) = 
	if c >=27 && c <=39 then 1
	else
		(if c = 50 then 13
		else 0)
(* Calculates the number of points a player receives from "winning" the hand*)
let rec score_of_turn (plays: (player_id * card) list) : int = 
	match plays with 
	| [] -> 0
	| h::t -> point_of_card (snd h) + score_of_turn t

(* Compares two plays of the same suit and returns the (player,card) tuple that is the max*)
let max_play (play1: (player_id * card)) (play2: (player_id * card)) = 
	if snd play1 > snd play2 then play1 else play2

(* Given a list of plays of the same suit, return the player that won the turn*)
let winner_of_turn (plays: (player_id * card) list) (curr_winner: (player_id * card)) : player = 
	match plays with
	| [] -> fst curr_winner
	| h::t -> winner_of_turn t (max_play h curr_winner)


(*Returns None if no card has been played this turn or Some x, the first play, otherwise
 * to be used for get_legal_moves*)
let get_first_play (card_lst:card list) : card option = 
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
let get_legal_moves (c:card option) (hearts_broken:bool) (card_lst:card list) : card list = 
	match c with
	| None ->
		if hearts_broken then card_lst
	else filter_hearts card_lst
	| Some x -> 
		(let same_suit_as_c = only_suit x card_lst in
		match same_suit_as_c with
		| [] -> card_lst
		| h::t -> same_suit_as_c)

(*Returns the player who "won" the hand and the number of points received*)
let calculate_turn_result (plays: (player_id * card) list) : (player_id * int) = 
	let first_play = List.hd plays in
	let first_card = snd first_play in
	let score = score_of_turn plays in
	let considered_plays = List.filter (fun x -> is_same_suit first_card (snd x)) plays in
	let winner = winner_of_turn considered_plays first_play in 
	(winner,score)

let (ai_names: player_id list)= ["Professor Clarkson"; "Professor Oak"; "Professor Constable"; "Professor George"]

 type player = {
    cards : hand;
    total_score: int;
    round_score: int;
    player_id: string;
    is_AI: bool;
    position: int;
  }

(*Creates a record for a human player*)
let initialize_human_players acc (player_name:player_id) = 
	let new_player = 
	{cards = [];
	total_score = 0;
	round_score = 0;
	player_id = player_name;
	is_AI = false;
	position = 0;} in
	acc @ [new_player]

(*Creates a record for a AI *)
let initialize_ai acc (ai_name: player_id) = 
	let new_ai = 
	{ cards = [];
	total_score = 0;
	round_score = 0;
	player_id = ai_name;
	is_AI = true;
	position = 0;} in
	acc @ [new_ai]

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
let initialize_deck () : card list= 
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
let distribute_deck (deck: card list ) : (card list list ) = 
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
let rec distribute_hands (players: player list) (hand_list : hand list) (acc: player list) (n: int) : player list = 
	match players with 
	| [] -> List.rev acc
	| h::t -> (
		match hand_list with
		| [] -> List.rev acc 
		| curr::others -> 
			let new_player = {
			cards =  curr;
			total_score = h.total_score;
			round_score = h.round_score;
			player_id = h.player_id;
			is_AI = h.is_AI;
			position = n;
			}
		in distribute_hands t others (new_player::acc) n+1
	)

(*Call this method*)
(*Given a list of players and ai names, create player records for each of them*)
let initialize_all_players (player_id_lst: player_id list) (ai_lst:player_id list) : player list =
	let num_players = List.length player_id_lst in
	let num_ais = 4 - num_players in
	let player_id_lst = List.fold_left initialize_human_players [] player_id_lst in 
	let ais = partial_lst ai_lst num_ais [] in
	let ai_lst = List.fold_left initialize_ai [] ais in
	let players_and_ais = player_id_lst@ai_lst in 
	let shuffled_deck = initialize_deck () in
	let four_hands = distribute_deck shuffled_deck in hh 
	distribute_hands players_and_ais four_hands [] 0

(* Position 0 will give cards to position 1
 * position 1 will give cards to position 2
 * position 2 will give cards to position 3
 * position 3 will give cards to position 0*)



(* find_player returns the player record that has this position*)
 let rec find_player (position: int) (player_lst: player list) : player = 
 	match player_lst with
 	| [] -> failwith "This position doesn't exist"
 	| h::tl -> 
 		if h.position = position then h
 	else find_player position tl 


let remove_card_from_hand (p_cards: card list) (c: card) = 
	List.filter (fun x -> x <> c) p_cards 

(*Removes [cards] from [p_cards]*)
let rec remove_cards_from_hand (p_cards: card list) (cards: card list) = 
	match cards with
	| [] -> p_cards 
	| h::tl -> remove_cards_from_hand (remove_card_from_hand p_cards h) tl


(* Given a p_id and exchanges, return the list of cards that is associated with that p_id*)
let rec find_exchange (p_id: player_id) (exchanges: (player_id * card list) list ) : card list = 
	match exchanges with 
	| [] -> failwith "No such doner made a exchange"
	| h::tl -> if fst h = p_id then snd h 
	else find_exchange p_id tl  

let rec update_hand (p:player) (cards: card list) (player_lst: player list) (res: player list): player list=
	 match player_lst with
	 | [] -> res
	 | h::tl ->
	 	if h.player_id = p.player_id then
	 	let new_player = {
	 	cards = cards;
	 	total_score = p.total_score;
	 	round_score = p.round_score;
	 	player_id = p.player_id;
	 	is_AI = p.is_AI;
	 	position = p.position;
	 	} in 
	 	update_hand p cards tl (res @ [new_player])
	 else 
	 update_hand p cards tl (res @ [h])


(*exchange will take a player, a list of players,  a list of (player_id, card list) tuples, (and a "rule") 
 * and return the card list that this player should receive. Returns the new updated player_lst *)
 let single_exchange (p:player) (player_lst: player list) (exchanges: (player_id * card list ) list) : player list = 

 	let doner_position = (p.position + 3) mod 4 in
 	let doner = find_player doner_position player_lst in
 	let doner_id = doner.player_id in
 	let doner_donation = find_exchange doner_id exchanges in
 	let receiver_cards = p.cards in
 	let receiver_plus_new_cards = receiver_cards @ doner_donation in
 	let receiver_donation = find_exchange p.player_id exchanges in
 	let receiver_minus_donation = remove_cards_from_hand receiver_plus_new_cards receiver_donation in
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
let rec exchange_cards (player_lst: player list ) (exchanges: (player_id * card list ) list ) (res: player list) : player list =
	match player_lst with
	| [] -> res
	| h::tl -> 
	exchange_cards tl exchanges (single_exchange h res exchanges)


let rec remove_card (player_lst: player list) (move: (player_id *  card)) (res: player list) : player list = 
	match player_lst with
	| [] -> res
	| p::tl ->
		if p.player_id = fst move 
		then let new_player = 
		{  cards = remove_card_from_hand p.cards (snd move);
    	total_score =  p.total_score;
	    round_score =  p.round_score;
	    player_id = p.player_id;
	    is_AI = p.is_AI;
	    position = p.position;
	    }
		in remove_card tl move (res@[new_player])
	else remove_card tl move res @ [p]

let rec update_score_after_turn (player_lst: player list) (turn_result: (player_id * int)) (res: player list) : player list = 
	match player_lst with
	| [] -> res
	| p::tl ->
		if p.player_id = fst turn_result 
		then let new_player =
		{
		cards = p.cards;
		total_score = p.total_score + (snd turn_result);
		round_score = p.round_score + (snd turn_result);
		player_id = p.player_id;
		is_AI = p.is_AI;
		position = p.position;
		}
		in update_score_after_turn tl turn_result res @ [new_player]
	else update_score_after_turn tl turn_result res @ [p]

let rec get_position_given_id (player_lst: player list) (id: player_id) : int = 
	match player_lst with
	| [] -> failwith "No player has this id"
	| h::tl ->
		if h.player_id = id
		then h.position
	else get_position_given_id tl id


let rec rearrange_player_list (player_lst: player list) (turn_result: (player_id *int)) : player list = 
	let winner_position = get_position_given_id player_lst (fst turn_result) in
	let res = ref [] in
	let counter = ref winner_position in 
	for i = 1 to 4 do
	res := !res @ [find_player !counter player_lst]; (counter:= (!counter + 1) mod 4)
	done;
	!res 

