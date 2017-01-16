open Card
open Player
open AI
open IOops
open Game_logic



let rec run_game () =
	let init_command = String.uppercase_ascii (String.trim (read_line ())) in
	let com =
		(if ((String.contains init_command ' ') &&
					(String.sub init_command 0 (String.index init_command ' ') = "STATS"))
		then "STATS" else init_command) in

	match com with
	| "READY" -> (let hearts_broken = ref false in
	let (player_name_lst: Player.player_id list) = SaveGame.get_human_players() in
	let (ai_names: Player.player_id list)= ["Professor Clarkson"; "Professor Oak"; "Professor Constable"; "Professor George"] in
	let (initialized_players: Player.player list) = initialize_all_players player_name_lst ai_names in
	let (exchanges: (Player.player_id * Card.card list) list)  = exchange_phase initialized_players [] in
	let (post_exchange_players: Player.player list) = exchange_cards initialized_players exchanges initialized_players in

	

	(*Get the plays for the 4 players*)

	let player_list = ref post_exchange_players in

	while (not (is_winner !player_list)) do
		let round_cards = ref [] in
		let hearts_broken = ref false in 
		for i = 1 to 13 do
			let turn_cards = ref [] in (*NOT SURE IF THIS GOES HERE, ( i think it should go inside play_turn), but
		albert needs access to the cards played so far in a turn for his AI, so whether his AI is going first or fourth,
		it needs access to those cards*)
			let (play_turn_res : (Player.player * Card.card) list) = (play_turn !player_list !round_cards hearts_broken) in
			let (turn_plays: (Player.player_id * Card.card) list) = player_card_to_playerid_card play_turn_res [] in
			let (new_players: Player.player list) = get_first_tuple play_turn_res in 

			(*Calculate the winner of that turn and how many points to receive*)
			let (turn_result: (Player.player_id * int)) = calculate_turn_result turn_plays in

			(*update that player's score after the turn*)
			let (updated_players_after_turn: Player.player list) = update_score_after_turn new_players turn_result [] in

			(* list is rearranged for the next turn*)
			let (post_turn_player_list: Player.player list) = rearrange_player_list updated_players_after_turn turn_result in 
			player_list := post_turn_player_list;
			round_cards := !round_cards @ !turn_cards
		done;
		let () = print_endline ("Round is over") in
		let () = display_player_scores !player_list in 
		let shuffled_deck = initialize_deck () in
		let four_hands = distribute_deck shuffled_deck in
		let fresh_players = distribute_hands !player_list four_hands [] 0 in
		player_list := fresh_players;

	done;

	let () = print_endline ("Congrats " ^ (get_winner !player_list) ^ "\nYou have won!" ) in
	let () = print_endline "\nType YES to play another game, NO to end" in
	let replay = String.uppercase_ascii (String.trim (read_line ())) in
	(if replay = "YES"
		then (SaveGame.update_player_json !player_list (get_winner !player_list);run_game ())
else SaveGame.update_player_json !player_list (get_winner !player_list)))
	| "HELP" -> (SaveGame.print_help (); run_game())
	| "QUIT" -> print_endline "Quitting Game..."
	| "LEADERBOARD" -> (SaveGame.display_high_score (); run_game ())
	| "STATS" -> (let last_space = (String.rindex init_command ' ') + 1 in
		let name_len = String.length init_command
										- ((String.rindex init_command ' ') +1) in
		let username = String.sub init_command last_space name_len in
		(SaveGame.display_player_stats username; run_game ()))
	| _ -> (print_endline "\nPlease enter a valid command"; run_game ())
