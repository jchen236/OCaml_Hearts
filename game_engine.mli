(* The engine intializes the game and runs the game loop *)
module type Engine = sig
  (* an up-to-date screenshot of the game at any point in time. keeps track of
   * information about the players, current hand(s), turns, and rounds  *)
  type state

  (* intialize generates, shuffles, and distribute the cards accordingly
   * and returns a game state with attributes required to start the game *)
  val intialize : state -> state

  (* takes the initial state and intitates a card exchange phase
   * before starting the game loop *)
  val exchange : state -> state

  (* looks at the current state for the given player/AI and returns a list of
   * available moves. (i.e. if player has card(s) with the same suit as
   * the first card played in a turn, those are the available cards *)
  val get_legal_moves : state -> card list

  (* takes the current state and plays the current selected card *)
  val play_card : state -> state

  (* takes in current state and sum the score for cards played for losing player
   * as well as update the state variable *)
  val calculate_turn_score : state -> (player, int) -> state

  (* returns the player with the lowest score after a game of hearts is
   * completed *)
  val isWinner : state -> player

  (* resets the current state to new instance of the game *)
  val tearDown : state -> state
end