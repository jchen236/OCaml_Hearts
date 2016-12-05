module Card = struct
	type card = int

	let create_card (n:int) : card= 
		(n:card)

	(* Calculates the bounds for the suit of this card. The bounds are inclusive*)
	let suit_bounds c : (int * int)=
		let (lowerbound:int) = (c-1)/13 in
		let (upperbound:int) = (c+12) / 13 in
		(lowerbound*13+1, upperbound*13)

	(* Returns a hand without any hearts *)
	let filter_hearts (card_lst: card list) : card list=
		List.filter (fun x -> x < 27 || x > 39) card_lst

	(*Returns if c2 is of the same suit as c1*)
	let is_same_suit (c1:card) (c2:card) =
		let lowerbound = fst (suit_bounds c1) in
		let upperbound = snd (suit_bounds c1) in
		(c2 >= lowerbound && c2 <= upperbound)

	(* Returns a hand that only contains cards of the same suit as c*)
	let only_suit (c: card) (card_lst: card list) : card list =
		List.filter( fun x -> is_same_suit c x) card_lst

	(* representation of a card
	 * cards will be represented by
	 * 1-13 is Diamonds
	 * - 1 -> 2 of Diamonds
	 * - 13 -> Ace of Diamonds
	 * 14-26 is Clubs
	 * 27-39 is Hearts
	 * 40-52 is Spades *)
	(*[rep_card_as_string card] returns the string representation of a card.
	e.g. if card = 3, "D4" would be returned*)
	let rep_card_as_string card =
	  let suit = if card <= 13 then "D"
	              else if card <= 26 then "C"
	              else if card <= 39 then "H"
	              else "S" in
	  let num = ((card-1) mod 13) in
	  let rank = (match num with
	  | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 -> string_of_int (num + 2)
	  | 9 -> "J"
	  | 10 -> "Q"
	  | 11 -> "K"
	  | 12 -> "A"
	  | _ -> "not a card"
		) in (suit ^ rank)

	 (*Returns the string representation of a card the user inputs to an int
	    e.g. C10 (10 of Clubs) = 23
	let string_to_int_card (str: string) : int =
		match (String.get str 0) with
		| 'D' -> int_of_string (String.sub str 1 (String.length str - 1))
		| 'C' -> int_of_string (String.sub str 1 (String.length str - 1)) + 13
		| 'H' -> int_of_string (String.sub str 1 (String.length str - 1)) + 26
		| 'S' -> int_of_string (String.sub str 1 (String.length str - 1)) + 39
		| _ -> failwith "invalid string card entered" *)

	(*[rank_repr_as_int rank] takes in a string [rank] and
	returns its corresponding int value as represented in the deck*)
	let rank_repr_as_int rank =
	  match rank with
	  |"2" |"3" |"4" |"5" |"6" |"7" |"8" |"9" |"10" -> (int_of_string rank - 1)
	  |"J" -> 10
	  |"Q" -> 11
	  |"K" -> 12
	  |"A" -> 13
	  | _ -> -1

	(*[convert_string_card_to_int card] returns an int represenation
	of a card's string representation*)
	let convert_string_card_to_int card =
	  let rank = rank_repr_as_int (String.sub card 1 (String.length card - 1)) in
	  match (String.sub card 0 1) with
	  |"D" -> rank
	  |"C" -> rank + 13
	  |"H" -> rank + 26
	  |"S" -> rank + 39
	  | _ -> -1

	(*[convert_hand_to_string_list cards]
	returns a list of cards represented as strings
	-[cards] is a "hand" containing a list of ints (representing cards) *)
	let rec convert_hand_to_string_list cards =
	  match cards with
	  | [] -> []
	  | h::t -> (rep_card_as_string h)::(convert_hand_to_string_list t)


	let point_of_card (c:card) =
		if c >=27 && c <=39 then 1
		else
			(if c = 50 then 13
			else 0)

	let remove_card_from_hand (p_cards: card list) (c: card) =
	List.filter (fun x -> x <> c) p_cards

	(*Removes [cards] from [p_cards]*)
	let rec remove_cards_from_hand (p_cards: card list) (cards: card list) =
		match cards with
		| [] -> p_cards
		| h::tl -> remove_cards_from_hand (remove_card_from_hand p_cards h) tl
end