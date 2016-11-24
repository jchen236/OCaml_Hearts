module Player : Player = struct

	let get_legal_moves card_desired card_list =
		if card_desired = None then get_hand
		else List.fold_left (fun playable_list card -> 
								if card.suit = card_desired.suit then card::playable_list else playable_list) [] card_list

	let play_card card hand =
		List.fold_left (fun new_hand cur_card -> if cur_card = card then new_hand else cur_card::new_hand) [] hand
end