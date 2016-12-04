include whoknows.ml

let run_game () = 
	let (player_name_lst: player_id list) = get_human_players() in
	let (ai_names: player_id list)= ["Professor Clarkson"; "Professor Oak"; "Professor Constable"; "Professor George"] in
	let (initialized_players: player list) = initialize_all_players player_name_lst ai_names in
	let (exchanges: (player_id * card list) list)  = exchange_phase initialized_players [] in
	let (post_exchange_players: player list) = exchange_cards initialized_players exchanges initialized_players in

	(*Get the plays for the 4 players*)


	let player_list = ref post_exchange_players in
	let (play_turn_res : (player * card) list) = play_turn player_list in
	let (turn_plays: (player_id * card) list) = player_card_to_playerid_card play_turn_res in

	(*Calculate the winner of that turn and how many points to receive*)
	let (turn_result: (player_id * int)) = calculate_turn_result turn_plays in

	(*update that player's score after the turn*)
	let (updated_players_after_turn: player list) = update_score_after_turn post_exchange_players turn_result [] in

	(* list is rearranged for the next turn*)
	let (post_turn_player_list: player list) = rearrange_player_list updated_players_after_turn turn_result in 





	


let run_game () = 
	1. Exchange
	2. For loop for each game
		- For loop for each round (1)
		- For loop for each turn (13)