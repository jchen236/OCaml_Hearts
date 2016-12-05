module Player = struct
include Card
	type player =
	{
		player_id : string;
		cards : Card.card list;
		total_score : int;
		round_score : int;
		is_AI : bool;
		position : int;
	}

	let init = 
	{
		player_id = "PLAYER";
		cards = [];
		total_score = -1;
		round_score = -1;
		is_AI = false;
		position = -1; 
	}

	let get_id p = p.player_id
	let get_cards p = p.cards
	let get_total_score p = p.total_score
	let get_round_score p = p.round_score
	let get_is_AI p = p.is_AI
	let get_position p = p.position


	let set_id p id = {p with player_id = id}
	let set_cards p cards = {p with cards = cards}
	let set_total_score p total_score = {p with total_score = total_score}
	let set_round_score p round_score = {p with round_score = round_score}
	let set_is_AI p is_AI = {p with is_AI = is_AI}
	let set_position p position = {p with position = position}

	let create_player id c ts rs is_AI p = 
	{
		player_id = id;
		cards = c;
		total_score  = ts;
		round_score  = rs;
		is_AI  = is_AI;
		position  = p;
	}




end
