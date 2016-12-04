include whoknows.ml

let run_game () = 

	let hearts_broken = ref false
	let (player_name_lst: player_id list) = get_human_players() in
	let (ai_names: player_id list)= ["Professor Clarkson"; "Professor Oak"; "Professor Constable"; "Professor George"] in
	let (initialized_players: player list) = initialize_all_players player_name_lst ai_names in
	let (exchanges: (player_id * card list) list)  = exchange_phase initialized_players [] in
	let (post_exchange_players: player list) = exchange_cards initialized_players exchanges initialized_players in



	(*Get the plays for the 4 players*)

	let player_list = ref post_exchange_players in

	while (not is_winner player_list) do
		let round_cards = ref [] in

		for i = 1 to 13 do
			let turn_cards = ref [] in (*NOT SURE IF THIS GOES HERE, ( i think it should go inside play_turn), but
		albert needs access to the cards played so far in a turn for his AI, so whether his AI is going first or fourth,
		it needs access to those cards*)
			let (play_turn_res : (player * card) list) = play_turn !player_list in
			let (turn_plays: (player_id * card) list) = player_card_to_playerid_card play_turn_res in

			(*Calculate the winner of that turn and how many points to receive*)
			let (turn_result: (player_id * int)) = calculate_turn_result turn_plays in

			(*update that player's score after the turn*)
			let (updated_players_after_turn: player list) = update_score_after_turn post_exchange_players turn_result [] in

			(* list is rearranged for the next turn*)
			let (post_turn_player_list: player list) = rearrange_player_list updated_players_after_turn turn_result in 
			player_list := post_turn_player_list;
			round_cards := !round_cards @ !turn_cards
		done

	done

	print_endline ("Congrats " ^ get_winner player_list ^ "\nYou have won!" ) in 

	update_player_json player_list (get_winner player_list)
