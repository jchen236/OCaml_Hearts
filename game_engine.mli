(* The engine intializes the game and runs the game loop *)
module type Engine = sig

  (* an up-to-date screenshot of the game at any point in time. keeps track of
   * information about the players, current hand(s), turns, and rounds  *)
  type state

  (* integer representation of a card *)
  type card

  (* intialize generates, shuffles, and distribute the cards accordingly
   * and returns a game state with attributes required to start the game *)
  val intialize : state -> state

  (* takes the initial state and intitates a card exchange phase
   * before starting the game loop *)
  val exchange : state -> state

  (* takes the current state and plays the current selected card *)
  val play_card : state -> state

  (* takes in current state and sum the score for cards played for losing player
   * as well as update the state variable *)
  val calculate_turn_score : state -> (player, int) -> state

  (* turns the integer representation of the card to the matching card for
   * the player *)
  val int_to_card : int -> card

  (* returns the player with the lowest score after a game of hearts is
   * completed *)
  val isWinner : state -> player

  (* resets the current state to new instance of the game *)
  val tearDown : state -> state
end