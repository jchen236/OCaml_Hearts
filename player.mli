module type Player sig =

  (* returns card list for player *)
  val get_hand : unit

  (* if a card has been played, look at the suit of the card and return
   * the list of cards that have the same suit. if there are no cards with
   * the same suit, then return the currrent hand *)
  val get_legal_moves : card option -> card list -> card list

  (* pass in player's hand and desired card to be played. removes card from
   * hand and returns binding of card to player *)
  val play_card : card -> hand -> (player_id, card)


end