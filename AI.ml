(* AI for Hearts Card Game *)
module AI : AI = struct

  type ai_id = string

  (* Retrieves card list for AI according to id *)
  let get_hand ai_id =
    failwith "Unimplemented"

  (* Looks for legal move based on whether or not a card was played *)
  let get_legal_moves (ai_id: string) (c: card option) (card_list: card list) =
    match c with
    | None   -> card_list
    | Some x -> failwith "Unimplemented"

  (* Plays desired card to the game. *)
  let play_card card hand =
    failwith "Unimplemented"

  (* Generates a random number from 0 to n given n. *)
  let randomizer (n:int) = Random.int n

  (* Random Selection AI:
   * Returns any available card that can be played. This AI does not care about
   * the score and just looks to play any card that is avaiable in its hand. *)
  let calc_card_to_play (ai_id: string) (c:card option) (card_list : card list) =
    let hand = get_hand ai_id in
    match c with
    | None   -> List.nth (randomizer (List.length card_list)) card_list
    | Some x -> let moves = get_legal_moves c card_list in
                List.nth (randomizer (List.length moves)) moves

  (* Checks if current hand has any cards that are *)
  let need_same_suit (c : card) (card_list : card list) : bool =
    failwith "Unimplemented"

  (* For a given card list, the function returns the smallest card avaiable
   * in the list. The call to the helper function is hard-coded with a 53
   * because if the function is called, there is bound to be a card in the hand
   * and it guarantees a card is chosen. *)
  let smallest_card (card_list : card list) =
    let rec smallest_card_helper acc (card_list : card list) =
      match card_list with
      | []   -> acc
      | h::t -> if h < acc
                  then smallest_card_helper h t
                else smallest_card_helper acc t
    in
    smallest_card_helper 53 card_list

  (* Checks if current hand contains the Queen of Spades *)
  let rec has_queen_spades (card_list : card list) : bool =
    match card_list with
    | []   -> False
    | h::t -> if h = 51
                then True
              else has_queen_spades t

  (* Checks if hand has hearts *)
  let has_hearts (card_list : card list) : bool =
    failwith "Unimplemented"

  (* Returns card list with hearts only *)
  let heart_hand (card_list: card list) : card_list =
    failwith "Unimplemented"

  (* Finds largest card *)
  let largest_card (card_list : card list) =
    let rec largest_card_helper acc (card_list : card list) =
      match card_list with
      | []   -> acc
      | h::t -> if h > acc
                  then largest_card_helper h t
                else largest_card_helper acc t
    in
    largest_card_helper 0 card_list

  (* Find next smallest card *)
  let next_smallest (c: card) (card_list : card list) =
    failwith "Unimplemented"

  (* Implements the card choice for score minimization portion of the greedy
   * algorithm. *)
  let score_minimizer (ai_id: string) (c: card) (card_list : card list) =
    if(need_same_suit c card_list)
      then next_smallest c card_list
    else (if(has_queen_spades)
              then 51
           else if (has_hearts)
              then let hand = heart_hand card_list
                   in
                   largest_card hand
           else largest_card card_list)

  (* Greedy Algorithm AI:
   * The algorithm returns a card that minimizes the score for the player.
   * Specifically, it will play the lowest card avaiable if the suit has to be
   * the same. It will play the Queen of Spades when possible. *)
  let greedy_calc_card_to_play (ai_id: string) (c:card option) (card_list : card list) =
    let hand = get_hand ai_id  in
    match c with
    | None      -> smallest_card card_list
    | Some card -> let moves = get_legal_moves c card_list in
                   score_minimizer card moves

  (* Finite Look-Ahead AI *)
end