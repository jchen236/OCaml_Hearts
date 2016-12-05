module Card : sig
	type card = int

	val create_card : int -> card
		
	(* Calculates the bounds for the suit of this card. The bounds are inclusive*)
	val suit_bounds : card -> (int * int)

	(* Returns a hand without any hearts *)
	val filter_hearts : card list -> card list 

	(*Returns if c2 is of the same suit as c1*)
	val is_same_suit : card -> card -> bool

	(* Returns a hand that only contains cards of the same suit as c*)
	val only_suit : card -> card list -> card list 

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
	val rep_card_as_string : card -> string 

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
	val rank_repr_as_int : string -> int 

	(*[convert_string_card_to_int card] returns an int representation
	of a card's string representation*)
	val convert_string_card_to_int : string -> int 
	 

	(*[convert_hand_to_string_list cards]
	returns a list of cards represented as strings
	-[cards] is a "hand" containing a list of ints (representing cards) *)
	val convert_hand_to_string_list : int list -> string list  

	val point_of_card : card -> int 


	val remove_card_from_hand : card list -> card -> card list 

	(*Removes [cards] from [p_cards]*)
	val remove_cards_from_hand : card list -> card list -> card list 
end