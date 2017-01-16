open Card
open Player

module AI = struct
(* AI for Hearts Card Game *)

  type ai_id = string

  (* Contains all the hands of the four players and the list of cards that
   * have been played. There will be a 5 length list. Elements 1-4 will be the
   * player id and their associated hands. The AI will always be the first
   * element in the (id, card list) tuple. The second to last element will be a string
   * associated with the winner of a single round/next person to start. The last
   * element will be a boolean to keep track of whether or not hearts has been
   * broken. *)
  type final_state = {
    mutable player_and_cards : (Player.player * Card.card list) list;
    mutable total_cards_played : Card.card list;
    mutable current_cards_played : int option array;
    mutable ordered_players : Player.player list;
    mutable hearts_broken : bool ref;
  }

(* ========================================================================== *)
(* General AI Methods *)

(* Calculates the bounds for the suit of this card. The bounds are inclusive*)
let suit_bounds c : (int * int)=
  let (lowerbound:int) = (c-1)/13 in
  let (upperbound:int) = (c+12) / 13 in
  (lowerbound*13+1, upperbound*13)

(* Returns a hand without any hearts *)
let filter_hearts (card_lst: Card.card list) : Card.card list=
  List.filter (fun x -> x < 27 || x > 39) card_lst

(*Returns if c2 is of the same suit as c1*)
let is_same_suit (c1:Card.card) (c2:Card.card) =
  let lowerbound = fst (suit_bounds c1) in
  let upperbound = snd (suit_bounds c1) in
  (c2 >= lowerbound && c2 <= upperbound)

(* Returns the legal moves given a card, whether hearts have been broken, and a list of cards
 * If c is None, that means that it's the first card played that turn and the player can play
 * anything if hearts are broken, or any non-heart card if hearts are not broken.
 * If c is Some x, that means this is not the first card played that turn. If the current player
 * has a card that is of x's suit, he must play it. Otherwise, he can play anything if hearts
 * are broken, or any non-heart cart if hearts are not broken.
*)
let get_legal_moves (c:Card.card option)  (card_lst:Card.card list) (hearts_broken:bool ref) : Card.card list =
  match c with
  | None ->
    if !hearts_broken then card_lst
  else filter_hearts card_lst
  | Some x ->
    (let same_suit_as_c = Card.only_suit x card_lst in
    match same_suit_as_c with
    | [] -> card_lst
    | h::t -> same_suit_as_c)

(* Calculates the number of points a player receives from "winning" the hand*)
let rec score_of_turn (plays: (Player.player_id * Card.card) list) : int =
  match plays with
  | [] -> 0
  | h::t -> Card.point_of_card (snd h) + score_of_turn t

(* Compares two plays of the same suit and returns the (player,card) tuple that is the max*)
let max_play (play1: (Player.player_id * Card.card)) (play2: (Player.player_id * Card.card)) =
  if snd play1 > snd play2 then play1 else play2

(* Given a list of plays of the same suit, return the player that won the turn*)
let rec winner_of_turn (plays: (Player.player_id * Card.card) list) (curr_winner: (Player.player_id * Card.card)) : Player.player_id =
  match plays with
  | [] -> fst curr_winner
  | h::t -> winner_of_turn t (max_play h curr_winner)

  (* Plays desired card to the game. *)
  let play_card card hand =
    failwith "Unimplemented"

let rec choose_random_three lst acc =
  match lst with
  | h1::h2::h3::t -> h1::h2::h3::acc

(* Exchange method for the AI:
 * Follows a greedy approach to eliminate diamonds or clubs. Always keeps spades
 * of Jack or lower. Returns a list of three cards that the AI wishes to
 * exchange. *)
let ai_exchange (ai : Player.player) : (Player.player_id * Card.card list) =
  let ai_id = Player.get_id ai in
  let ai_hand = Player.get_cards ai in
  (ai_id, choose_random_three ai_hand [])

(* Generates a random number from 0 to n given n. *)
let randomizer (n:int) = Random.int n

(* Returns the first play (None) or not *)
let find_first_play (turn_array : int option array) =
  match turn_array with
  | [|a;b;c;d|] -> a

(* Given a list with option types, the function removes the option.
 * Precondition: List with Some x only.
 * Postcondition: List containing x and no option types. *)
let eliminate_option lst : Card.card list =
  let rec eliminate_helper lst acc =
    match lst with
    | [] -> acc
    | (Some x)::t -> x :: eliminate_helper t acc
  in
  eliminate_helper lst []

(* Converts current moves in the array to list form for the greedy approach.
 * If there is a None field, then the element is cut off from the list. *)
let array_to_list (turn_array) : Card.card list =
  match turn_array with
  | [|a;b;c;d|] -> eliminate_option (List.filter (fun x -> x != None) [a;b;c;d])

(* Finds the index of the first appearance of a variable in a list *)
let find_first_idx lst var =
  let rec find_first_idx_helper lst var acc =
  match lst with
  | []   -> failwith "Not possible"
  | h::t -> if h = var
              then acc
            else find_first_idx_helper t var (acc + 1)
  in find_first_idx_helper lst var 0

(* Turns an array of length 4 into a list of length 4 *)
let original_array_to_list (turn_array : int option array) : int option list =
  match turn_array with
  | [|a;b;c;d|] -> [a;b;c;d]

(* ========================================================================== *)
(* Methods for Random Selection AI: *)

 (* Returns any available card that can be played. This AI does not care about
  * the score and just looks to play any card that is avaiable in its hand. *)
let random_play (turn_array: int option array) (player_list : Player.player list) (hearts_broken : bool ref) : (Player.player * Card.card) =
  let first_play = find_first_play turn_array in
  match first_play with
  | None   -> let ai = List.nth player_list 0 in
             let card_list = get_legal_moves None ai.cards hearts_broken in
             (ai, List.nth card_list (randomizer (List.length card_list)))
  | Some c -> let turn_list = original_array_to_list turn_array in
              let ai_indx = find_first_idx turn_list (Some(-1)) in
              let ai = List.nth player_list ai_indx in
              let ai_hand = Player.get_cards ai in
              let card_list = get_legal_moves (Some c) ai_hand hearts_broken in
              (ai, List.nth card_list (randomizer (List.length card_list)))

(* ========================================================================== *)
(* Methods for Greedy Selection AI: *)

  (* Finds the number of cards that a person has for a specific suit *)
  let cards_in_suit (card_list: Card.card list) =
    failwith "Unimplemented"

  (* Checks if current hand has any cards that are *)
  let need_same_suit (c : Card.card) (card_list : Card.card list) : bool =
    failwith "Unimplemented"

  (* For a given card list, the function returns the smallest card avaiable
   * in the list. The call to the helper function is hard-coded with a 53
   * because if the function is called, there is bound to be a card in the hand
   * and it guarantees a card is chosen. *)
  let smallest_card (card_list : Card.card list) =
    let rec smallest_card_helper acc (card_list : Card.card list) =
      match card_list with
      | []   -> acc
      | h::t -> if h < acc
                  then smallest_card_helper h t
                else smallest_card_helper acc t
    in
    smallest_card_helper 53 card_list

  (* Checks if current hand contains the Queen of Spades *)
  let rec has_queen_spades (card_list : Card.card list) : bool =
    match card_list with
    | []   -> false
    | h::t -> if h = 50
                then true
              else has_queen_spades t

  (* Checks if hand has hearts *)
  let has_hearts (card_list : Card.card list) : bool =
    if(List.length (List.filter (fun x -> x >= 27 || x <= 39) card_list)) >= 1
      then true
    else false

  (* Returns card list with hearts only *)
  let heart_hand (card_list: Card.card list) : Card.card list =
    List.filter (fun x -> x >= 27 || x <= 39) card_list

  (* Finds largest card *)
  let largest_card (card_list : Card.card list) =
    let rec largest_card_helper acc (card_list : Card.card list) =
      match card_list with
      | []   -> acc
      | h::t -> if h > acc
                  then largest_card_helper h t
                else largest_card_helper acc t
    in
    largest_card_helper 0 card_list

  (* Find next smallest card in given hand *)
  let next_smallest (c: Card.card) (card_list : Card.card list) =
    let suit_list = Card.only_suit c card_list in
    let sorted_suit_list = List.sort compare suit_list in
    let rec next_smallest_helper (c: Card.card) (card_list : Card.card list) (acc : Card.card) =
      match card_list with
      | [] -> acc
      | h::t -> if(h < c && h > acc)
                  then next_smallest_helper c t h
                else
                  next_smallest_helper c t acc
  in
    next_smallest_helper c card_list 0


  (* Implements the card choice for score minimization portion of the greedy
   * algorithm. *)
  let score_minimizer (c: Card.card) (card_list : Card.card list) (cards_played : Card.card list) : Card.card =
    let same_suit_list = Card.only_suit c card_list in
      match same_suit_list with
      | [] -> if(has_queen_spades card_list)
                  then 50
                else if(has_hearts card_list)
                  then largest_card (heart_hand card_list)
                else
                   largest_card card_list
      | h::t -> next_smallest c same_suit_list

  (* Greedy Algorithm AI:
   * The algorithm returns a card that minimizes the score for the player.
   * Specifically, it will play the lowest card avaiable if the suit has to be
   * the same. It will play the Queen of Spades when possible. *)
  let greedy_play (turn_array : int option array) (player_list : Player.player list) (hearts_broken : bool ref) : (Player.player * Card.card) =
    let first_play = find_first_play turn_array in
    match first_play with
    | None   -> let ai = List.nth player_list 0 in
                (ai, smallest_card (get_legal_moves None ai.cards hearts_broken))
    | Some c -> let cards_played = array_to_list turn_array in
                let turn_list = original_array_to_list turn_array in
                let ai_indx = find_first_idx turn_list (Some(-1)) in
                let ai = List.nth player_list ai_indx in
                let ai_hand = ai.cards in
                let legal_moves = get_legal_moves (Some c) ai_hand hearts_broken in
                (ai, score_minimizer c legal_moves cards_played)

(* ========================================================================== *)
(* Methods for Look-Ahead AI: *)

  (* Removes an element from a list *)
  let remove_from_list elt lst =
    List.filter (fun x -> x <> elt) lst

  (* Given two lists, remove elements of list 1 from list 2 *)
  let remove_list_from_list lst1 lst2 =
    let rec remove_list_from_list_helper lst1 lst2 acc =
      match lst1 with
      | []   -> acc
      | h::t -> let new_list = remove_from_list h lst2 in
                remove_list_from_list_helper t lst2 new_list
    in
    remove_list_from_list_helper lst1 lst2 []

  (* Given a list lst and a number n, returns a list lst2 that contains
   * n distinct elements from lst. *)
  let randomized_subset_list lst n =
    let rec randomized_subset_list_helper lst n acc counter =
      if counter != n
        then let elt = List.nth lst (randomizer (List.length lst)) in
             let new_list = remove_from_list elt lst in
             randomized_subset_list_helper new_list n (elt::acc) (counter + 1)
        else acc
    in randomized_subset_list_helper lst n [] 0

(* Required for deck distribution*)
(* Creates a shuffled deck of 52 cards *)
let initialize_deck () : Card.card list=
  let rec generate_list_cards x y acc =
    if x > y then acc
    else generate_list_cards (x+1) y (acc @ [x])
  in
  let deck = generate_list_cards 1 52 [] in
  let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  shuffle deck

  (* Given the cards played, find the remaining cards in the game and assign
   * them to three other lists. *)
  let distribute_cards (new_player_list : Player.player list) (turn_list : int option list) (total_cards_played : Card.card list) (ai_cards : Card.card list)  =
    let deck = initialize_deck () in
    let deck_no_ai = remove_list_from_list ai_cards deck in
    let number_cards_left = remove_list_from_list total_cards_played deck_no_ai in
      let rec distribute_cards_helper (new_player_list) (turn_list) (cards_left) (acc) (counter) =
        match turn_list with
        | []   -> acc
        | h::t -> if h = None
                    then let num_cards_needed = (List.length cards_left) / (List.length new_player_list) in
                         let hand = randomized_subset_list cards_left num_cards_needed in
                         let remaining_cards = remove_list_from_list hand cards_left in
                         let player = List.nth new_player_list counter in
                         List.rev(distribute_cards_helper new_player_list t remaining_cards (hand::acc) (counter + 1))
                  else
                    let num_cards_needed = ((List.length cards_left) / (List.length new_player_list)) - 1 in
                    let hand = randomized_subset_list cards_left num_cards_needed in
                    let remaining_cards = remove_list_from_list hand cards_left in
                    let player = List.nth new_player_list counter in
                    List.rev(distribute_cards_helper new_player_list t remaining_cards (hand::acc) (counter + 1))

      in distribute_cards_helper new_player_list turn_list number_cards_left [] 0

  (*Returns the player who "won" the hand and the number of points received*)
  let calculate_turn_result (plays: (Player.player_id * Card.card) list) : (Player.player_id * int) =
    let first_play = List.hd plays in
    let first_card = snd first_play in
    let score = score_of_turn plays in
    let considered_plays = List.filter (fun x -> is_same_suit first_card (snd x)) plays in
    let winner = winner_of_turn considered_plays first_play in
    (winner,score)

  (* Given a turn array, removes all the options for use in calculating the
   * play tuples. *)
  let no_option_array (turn_array : int option array) =
    match turn_array with
    |[|Some a; Some b; Some c; Some d|] -> [|a;b;c;d|]

  (* Given a player list and the turn array, the function produces a
   * player, card tuple that can then be used to calculate the score
   * and the winner of the current round. Order of player list is the
   * order in which they play. Position field is static. Turn array has
   * the same ordering. *)
  let convert_plays_to_tuple (turn_array : int array) (player_list : Player.player list) =
    let start_idx = -1 in
    let rec convert_helper (idx) (t_array) (p_list) (acc) =
      match p_list with
      | []   -> acc
      | h::t -> let index = idx + 1 in
                [(Player.get_id h, t_array.(index))] @ (convert_helper index t_array t acc)
    in convert_helper start_idx turn_array player_list []

  (* Given two lists, create a list that returns a tuple of the elements
   * in their respective order *)
  let lists_to_tuple lst1 lst2 =
    List.map2 (fun elt1 elt2 -> (elt1,elt2)) lst1 lst2

  (* Given a list, an element, and idx n, create a new list with n at idx n *)
  let list_add_at_idx lst elt n =
    if n = List.length lst
      then lst @ [elt]
    else
      match lst with
      | elt1::elt2::elt3::[] -> if n = 0
                                  then elt::elt1::elt2::elt3::[]
                                else if n = 1
                                  then elt1::elt::elt2::elt3::[]
                                else
                                  elt1::elt2::elt::elt3::[]

  (* Creates a state representation that will be used for game simulation
   * for the look-ahead AI. *)
  let create_state (turn_array : int option array) (ord_players : Player.player list) (t_cards_played : Card.card list) (hrts_broken : bool ref) : final_state =
    let turn_list = original_array_to_list turn_array in
    let ai_idx = find_first_idx turn_list None in
    let ai_elt = List.nth turn_list ai_idx in
    let ai = List.nth ord_players ai_idx in
    let ai_cards = Player.get_cards ai in
    let new_player_list = remove_from_list ai ord_players in
    let new_turn_list = remove_from_list ai_elt turn_list in
    let generated_lists = distribute_cards new_player_list new_turn_list t_cards_played ai_cards in
    let list_of_tuples = lists_to_tuple new_player_list generated_lists in
    let final_list = list_add_at_idx list_of_tuples (ai,ai_cards) ai_idx in
    {
      player_and_cards = final_list;
      total_cards_played = t_cards_played;
      current_cards_played = turn_array;
      hearts_broken = hrts_broken;
      ordered_players = ord_players;
    }

  (* Adds played cards to the total_cards_played list *)
  let update_total_cards_played (st: final_state) (card_list : Card.card list) : unit =
    match card_list with
    | []   -> ()
    | h::t -> st.total_cards_played <- (st.total_cards_played @ card_list)

  (* Given a card option, removes the option data type*)
  let remove_card_option (card : Card.card option) =
    match card with
    | Some c -> c

  (* Remove a card from a card list *)
  let remove_card_from_hand (p_cards: Card.card list) (c: Card.card) =
   List.filter (fun x -> x <> c) p_cards

  (* Removes the played card from players hands after a round is simulated *)
  let update_player_cards (st: final_state) : unit =
    let turn_array = st.current_cards_played in
    let player_card_tuple_list = st.player_and_cards in
    let rec update_player_cards_helper (st) (t_array) (acc) (counter) =
      match player_card_tuple_list with
      | []   -> st.player_and_cards <- (List.rev acc)
      | h::t -> let player = fst h in
                let player_hand = snd h in
                let play = remove_card_option (t_array.(counter)) in
                update_player_cards_helper st t_array ((player, remove_card_from_hand player_hand play)::acc) (counter + 1)
    in
    update_player_cards_helper st turn_array [] 0


(* find_player returns the player record that has this position*)
 let rec find_player (position: int) (player_lst: Player.player list) : Player.player =
  match player_lst with
  | [] -> failwith "This position doesn't exist"
  | h::tl ->
    if Player.get_position h = position then h
  else find_player position tl

  let rec get_position_given_id (player_lst: Player.player list) (id: Player.player_id) : int =
    match player_lst with
    | [] -> failwith "No player has this id"
    | h::tl ->
      if (Player.get_id h) = id
      then (Player.get_position h)
    else get_position_given_id tl id

  (* Given a player list and a winner from a round, reconstruct a new
   * ordered player list to determine who goes first. *)
  let rec rearrange_player_list (player_lst: Player.player list) (turn_result: (Player.player_id *int)) : Player.player list =
    let winner_position = get_position_given_id player_lst (fst turn_result) in
    let res = ref [] in
    let counter = ref winner_position in
    for i = 1 to 4 do
    res := !res @ [find_player !counter player_lst]; (counter:= (!counter + 1) mod 4)
    done;
    !res

  (* If a hearts is played, then hearts is broken. *)
  let check_hearts_broken (st: final_state) (card : Card.card) : unit =
    if(is_same_suit card 28)
      then st.hearts_broken <- (ref true)
    else
      ()

  (* Once the AI chooses the card, this method finishes the turn if the AI
   * is not the last player. This returns an updated state with all the cards
   * and hands of a simulation. Fills in the rest of the array *)
  let rec finish_turn (st : final_state) (first_card : Card.card option) =
    let ordered_players = st.ordered_players in
    let turn_array = st.current_cards_played in
    let hearts_broken = st.hearts_broken in
    match turn_array with
    | [|a;b;c;d|] -> if b = None
                      then let player = List.nth ordered_players 1 in
                           let player_hand = player.cards in
                           let cards_played = array_to_list turn_array in
                           let card = greedy_play turn_array ordered_players hearts_broken in
                           let () = turn_array.(1) <- Some (snd card) in
                           let () = check_hearts_broken st (snd card)
                         in finish_turn st first_card
                     else if c = None
                      then let player = List.nth ordered_players 2 in
                           let player_hand = player.cards in
                           let cards_played = array_to_list turn_array in
                           let card = greedy_play turn_array ordered_players hearts_broken in
                           let () = turn_array.(2) <- Some (snd card) in
                           let () = check_hearts_broken st (snd card)
                         in finish_turn st first_card
                     else
                      let player = List.nth ordered_players 3 in
                      let player_hand = player.cards in
                      let cards_played = array_to_list turn_array in
                      let card = greedy_play turn_array ordered_players hearts_broken in
                      let () = turn_array.(3) <- Some (snd card) in
                      let () = check_hearts_broken st (snd card) in
                      let () = st.current_cards_played <- turn_array in
                      let () = update_player_cards st in
                      let turn_list = array_to_list turn_array in
                      let () = update_total_cards_played st turn_list in
                      let result_tuples = convert_plays_to_tuple (no_option_array turn_array) ordered_players in
                      let winner_tuple = calculate_turn_result result_tuples in
                      let new_player_list = rearrange_player_list ordered_players winner_tuple
                    in
                      st.ordered_players <- new_player_list

  (* If the winner is the ai, return the number of points taken for the round.
   * Else return 0, meaning the AI does not take the trick. *)
  let winner_ai (turn_array : int option array) (ai_id : Player.player_id) (ordered_players : Player.player list): int =
    let result_tuples = convert_plays_to_tuple (no_option_array turn_array) ordered_players in
    let winner_tuple = calculate_turn_result result_tuples in
    if fst winner_tuple = ai_id
      then snd winner_tuple
    else 0

  (* Looks at all valid cards for the current hand and completes one round (4
   * turns). Returns the number of points that the AI gains for one round. If
   * AI does not win, there there is no pts recieved. *)
  let advance_round (card: Card.card) (st : final_state) : int =
    let () = check_hearts_broken st card in
    let turn_array = st.current_cards_played in
    let ordered_players = st.ordered_players in
    match turn_array with
    | [|a;b;c;d|] ->
                      if a = None
                        then (let ai = List.nth ordered_players 0 in
                              let ai_id = ai.player_id in
                              let () = turn_array.(0) <- Some card in
                              let () = finish_turn st (Some card)  in
                              winner_ai turn_array ai_id ordered_players)
                      else if b = None
                        then (let ai = List.nth ordered_players 1 in
                              let ai_id = ai.player_id in
                              let () = turn_array.(1) <- Some card in
                              let () = finish_turn st (Some card)  in
                              winner_ai turn_array ai_id ordered_players)
                      else if c = None
                        then (let ai = List.nth ordered_players 2 in
                              let ai_id = ai.player_id in
                              let () = turn_array.(2) <- Some card in
                              let () = finish_turn st (Some card)  in
                              winner_ai turn_array ai_id ordered_players)
                      else
                        let ai = List.nth ordered_players 3 in
                        let ai_id = ai.player_id in
                        let () = turn_array.(3) <- Some card in
                        let () = st.current_cards_played <- turn_array in
                        let result_tuples = convert_plays_to_tuple (no_option_array turn_array) ordered_players in
                        let winner_tuple = calculate_turn_result result_tuples in
                        let new_player_list = rearrange_player_list ordered_players winner_tuple in
                        let () = st.ordered_players <- new_player_list
                      in
                        winner_ai turn_array ai_id ordered_players
(*
  let rec simulate_up_to_ai (st : final_state) (ai : player) : unit =
    let turn_array = st.current_cards_played in
    let pos_list = original_array_to_list turn_array in
    let first_play = find_first_play pos_list in
    let new_order = st.ordered_players in
    match new_order with
    | [] -> failwith "Impossible"
    | h::t -> if h = ai
                then ()
              else
                let player = List.nth ordered_players 2 in
                let player_hand = player.cards in
                let cards_played = array_to_list turn_array in
                let card = greedy_play first_card player_hand cards_played in
                let () = turn_array.(2) <- Some card

              *)


  (* Given that a game has started, this loop will simulate the entire game
   * for an intialized round and return the number of points that the AI
   * has amounted throughout the entire simulation *)
  let finish_game (st : final_state) (ai : Player.player) : int =
    let total_cards = st.total_cards_played in
    let total_points = ref 0 in
    let turn_array = st.current_cards_played in
    let pos_list = original_array_to_list turn_array in
    let ai_idx = find_first_idx pos_list None in
    let player_card_tuple_list = st.player_and_cards in
    let ai_tuple = List.nth player_card_tuple_list ai_idx in
    let ai = fst ai_tuple in
    let ai_cards = snd ai_tuple in
    let first_play = find_first_play turn_array in
    let hearts_broken = st.hearts_broken in
    let legal_moves = get_legal_moves first_play ai_cards hearts_broken in
    let () =
    while (List.length total_cards) != 52
      do
        let card = List.nth legal_moves (randomizer (List.length legal_moves)) in
        total_points := !total_points + advance_round card st
    done
  in
  !total_points

  (* Finite Look-Ahead AI:
   * Calculates the best card by running a simulation of a round on every
   * card available for play in a single turn. Game_points represents the
   * number of rounds that the AI has won. *)
  let look_ahead_play (turn_array : int option array) (ordered_players: Player.player list) (total_cards_played : Card.card list) (hearts_broken : bool ref) : (Player.player * Card.card) =
    let st = create_state turn_array ordered_players total_cards_played hearts_broken in
    let pos_list = original_array_to_list turn_array in
    let ai_idx = find_first_idx pos_list None in
    let player_card_tuple_list = st.player_and_cards in
    let ai_tuple = List.nth player_card_tuple_list ai_idx in
    let ai = fst ai_tuple in
    let ai_cards = snd ai_tuple in
    let first_play = find_first_play turn_array in
    let legal_moves = get_legal_moves first_play ai_cards hearts_broken in
    let best_points = ref 0 in
    let best_card = ref 0 in
    let rec future_calc_helper (legal_moves) (best_points) (best_card) =
        let game_points = ref 0 in
        match legal_moves with
        | []   -> !best_card
        | h::t -> let new_st = st in
                  let () = game_points := advance_round h new_st in
                  let () = game_points := !game_points + finish_game new_st ai in
                  if !game_points < !best_points
                    then (best_points := !game_points;
                         best_card := h;
                         future_calc_helper t best_points best_card)
                  else future_calc_helper t best_points best_card
    in
    (ai, future_calc_helper legal_moves best_points best_card)

  end
