module type AI sig =

  (* returns card list for AI *)
  val get_hand : unit

  (* AI's total score for the game*)
  type total_score : int

  (* AI's score for the round*)
  type round_score : int

  (* AI's unique id*)
  type ai_id : string

  (* if a card has been played, look at the suit of the card and return
   * the list of cards that have the same suit. if there are no cards with
   * the same suit, then return the currrent hand *)
  val get_legal_moves : card option -> card list -> card list

  (*calculates which card to play based off the algorithm*)
  val calc_card_to_play : card option -> card list -> card

  (* pass in player's hand and desired card to be played. removes card from
   * hand and returns binding of card to player *)
  val play_card : card -> hand -> (player_id, card)




end