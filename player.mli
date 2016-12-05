open Card
module Player : sig
type player =
{
	player_id : string;
	cards : Card.card list;
	total_score : int;
	round_score : int;
	is_AI : bool;
	position : int;
}

type player_id = string
val init : player

val get_id : player -> string
val get_cards : player -> Card.card list
val get_total_score : player -> int
val get_round_score : player -> int
val get_is_AI : player -> bool
val get_position : player -> int


val set_id : player -> string -> player
val set_cards : player -> Card.card list -> player
val set_total_score : player -> int -> player
val set_round_score :  player -> int -> player
val set_is_AI : player -> bool -> player 
val set_position : player -> int -> player
val create_player : string -> Card.card list -> int -> int -> bool -> int -> player 

end