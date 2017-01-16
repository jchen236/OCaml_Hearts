open Card
open Player
module AI : sig
	

(* AI Methods *)
(* Given a player record, the method returns a card list of length 3 containing
 * the cards that the AI chooses to exchange with another player. *)
val ai_exchange : Player.player ->  ( Player.player_id * (Card.card list) )

(*Chooses a card randomly based on legal cards to play. *)
val random_play : int option array -> Player.player list -> bool ref -> (Player.player * Card.card)

(* Follows a greedy approach when choosing which card to play. *)
val greedy_play : int option array -> Player.player list -> bool ref -> (Player.player * Card.card)

(* Simulates a game for every card available in a legal hand and chooses
 * the card that the AI predicts will minimize its score. *)
(*val look_ahead_play : int option array -> Player.player list -> Card.card list -> bool ref -> (Player.player * Card.card)*)

end 