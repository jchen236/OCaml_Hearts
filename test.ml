open OUnit2
open Whoknows
open AI
open Card
open Player

let points_turn_test = [
	"all spades" >:: (fun _ -> assert_equal ("p4", 0) (calculate_turn_result [("p1", 40); ("p2", 41); ("p3", 42); ("p4", 43)]));
	"all spades, queen of spades" >:: (fun _ -> assert_equal ("p1", 13) (calculate_turn_result [("p1", 50); ("p2", 41); ("p3", 42); ("p4", 43)]));
	"all clubs" >:: (fun _ -> assert_equal ("p3", 0) (calculate_turn_result [("p1", 14); ("p2", 16); ("p3", 26); ("p4", 19)]));
	"all diamonds" >:: (fun _ -> assert_equal ("p2", 0) (calculate_turn_result [("p1", 2); ("p2", 13); ("p3", 1); ("p4", 6)]));
	"four non-heart cards (not queen of spades), start spades" >:: (fun _ -> assert_equal ("p1", 0) (calculate_turn_result [("p1", 40); ("p2", 1); ("p3", 15); ("p4", 3)]));
	"four non-heart cards (not queen of spades), start clovers" >:: (fun _ -> assert_equal ("p1", 0) (calculate_turn_result [("p1", 15); ("p2", 1); ("p3", 42); ("p4", 43)]));
	"four non-heart cards (not queen of spades), start diamonds" >:: (fun _ -> assert_equal ("p1", 0) (calculate_turn_result [("p1", 2); ("p2", 1); ("p3", 42); ("p4", 43)]));
	"four non-heart cards (queen of spades), start spades" >:: (fun _ -> assert_equal ("p3", 13) (calculate_turn_result [("p1", 40); ("p2", 1); ("p3", 50); ("p4", 3)]));
	"four non-heart cards (queen of spades), start clovers" >:: (fun _ -> assert_equal ("p1", 13) (calculate_turn_result [("p1", 15); ("p2", 1); ("p3", 50); ("p4", 43)]));
	"four non-heart cards (queen of spades), start diamonds" >:: (fun _ -> assert_equal ("p1", 13) (calculate_turn_result [("p1", 2); ("p2", 1); ("p3", 50); ("p4", 43)]));
	"all hearts" >:: (fun _ -> assert_equal ("p4", 4) (calculate_turn_result [("p1", 27); ("p2", 28); ("p3", 29); ("p4", 30)]));
	"One hearts, hearts doesn’t start, three others" >:: (fun _ -> assert_equal ("p1", 1) (calculate_turn_result [("p1", 26); ("p2", 27); ("p3", 14); ("p4", 15)]));
	"Two hearts, hearts doesn’t start, two others" >:: (fun _ -> assert_equal ("p1", 2) (calculate_turn_result [("p1", 26); ("p2", 27); ("p3", 28); ("p4", 15)]));
	"Three hearts, hearts doesn’t start, one other" >:: (fun _ -> assert_equal ("p1", 3) (calculate_turn_result [("p1", 26); ("p2", 27); ("p3", 28); ("p4", 39)]));
	"One hearts, hearts start, three others" >:: (fun _ -> assert_equal ("p1", 1) (calculate_turn_result [("p1", 27); ("p2", 40); ("p3", 41); ("p4", 42)]));
	"Two hearts, hearts start, two others" >:: (fun _ -> assert_equal ("p2", 2) (calculate_turn_result [("p1", 27); ("p2", 28); ("p3", 41); ("p4", 42)]));
	"Three hearts, hearts start, one other" >:: (fun _ -> assert_equal ("p3", 3) (calculate_turn_result [("p1", 27); ("p2", 28); ("p3", 29); ("p4", 42)] ));
]

let legal_moves_test = [
	"all spades, hearts not broken" >:: (fun _ -> assert_equal [42; 41] (get_legal_moves (Some 40) [1; 14; 27; 42; 41] (ref false) ));
	"all diamonds, hearts not broken" >:: (fun _ -> assert_equal [1; 2] (get_legal_moves (Some 3) [1; 2; 14; 27; 42; 41] (ref false)));
	"all clubs, hearts not broken" >:: (fun _ -> assert_equal [14; 15] (get_legal_moves (Some 16) [1; 2; 14; 15; 27; 42; 41] (ref false)));
	"all spades, HB" >:: (fun _ -> assert_equal [42; 41] (get_legal_moves (Some 40) [1; 14; 27; 42; 41] (ref true)));
	"all diamonds, HB" >:: (fun _ -> assert_equal [1; 2] (get_legal_moves (Some 3) [1; 2; 14; 27; 42; 41] (ref true)));
	"all clubs, HB" >:: (fun _ -> assert_equal [14; 15] (get_legal_moves (Some 16) [1; 2; 14; 15; 27; 42; 41] (ref true)));
	"all hearts" >:: (fun _ -> assert_equal [27; 28] (get_legal_moves (Some 29) [1; 2; 14; 15; 27; 28; 42; 41] (ref true)));
	"spades, but you have no spades" >:: (fun _ -> assert_equal [1; 14; 27] (get_legal_moves (Some 40) [1; 14; 27] (ref false)));
	"spades, but you have no spades HB" >:: (fun _ -> assert_equal [1; 14; 27] (get_legal_moves (Some 40) [1; 14; 27] (ref true)));
	"diamonds, but you have no diamonds" >:: (fun _ -> assert_equal [14; 15; 27; 28; 42; 41] (get_legal_moves (Some 3) [14; 15; 27; 28; 42; 41] (ref false)));
	"diamonds, but you have no diamonds HB" >:: (fun _ -> assert_equal [14; 15; 27; 28; 42; 41] (get_legal_moves (Some 3) [14; 15; 27; 28; 42; 41] (ref true)));
	"clubs, but you have no clubs" >:: (fun _ -> assert_equal [1; 2; 27; 28; 42; 41] (get_legal_moves (Some 16) [1; 2; 27; 28; 42; 41] (ref false)));
	"clubs, but you have no clubs HB" >:: (fun _ -> assert_equal [1; 2; 27; 28; 42; 41] (get_legal_moves (Some 16) [1; 2; 27; 28; 42; 41] (ref true)));
	"hearts, but you have no hearts" >:: (fun _ -> assert_equal [1; 2; 14; 15; 42; 41] (get_legal_moves (Some 29) [1; 2; 14; 15; 42; 41] (ref false)));
	"First player, !HB" >:: (fun _ -> assert_equal [1; 2; 14; 15; 42; 41] (get_legal_moves None [1; 2; 14; 15; 27; 28; 42; 41] (ref false)));
	"First player, HB" >:: (fun _ -> assert_equal [1; 2; 14; 15; 27; 28; 42; 41] (get_legal_moves None [1; 2; 14; 15; 27; 28; 42; 41] (ref true)));
	"First player, have only hearts but !HB" >:: (fun _ -> assert_equal [27; 28] (get_legal_moves None [27; 28] (ref false)));
]
(*
let player_lst1 =  [
	 {cards = [22; 23; 24; 25; 26]; total_score = 0; round_score = 0; player_id = "ellie"; is_AI = false; position = 0};
	 {cards = [1; 2; 3; 4; 5; 6; 7]; total_score = 0; round_score = 0; player_id = "bob"; is_AI = false; position = 1};
	 {cards = [8; 9; 10; 11; 12; 13; 14; 15]; total_score = 0; round_score = 0; player_id = "charlie"; is_AI = false; position = 2};
	 {cards = [16; 17; 18; 19; 20; 21]; total_score = 0; round_score = 0; player_id = "drake"; is_AI = false; position = 3}]
let exchange1 = [("bob", [1; 2; 3]); ("charlie", [8; 9; 10]); ("drake", [16; 17; 18]); ("ellie", [22; 23; 24])]
let result1 = exchange_cards player_lst1 exchange1 player_lst1

let player_lst2 = [
	 {cards = [1; 22; 23; 24; 25; 26; 40]; total_score = 0; round_score = 0; player_id = "ellie"; is_AI = false; position = 2};
	 {cards = [2; 5; 6; 7; 19; 37]; total_score = 0; round_score = 0; player_id = "bob"; is_AI = false; position = 1};
	 {cards = [3; 8; 9; 10; 11; 36; 12; 13; 14; 15]; total_score = 0; round_score = 0; player_id = "charlie"; is_AI = false; position = 0};
	 {cards = [4; 16; 17; 29; 18; 20; 38]; total_score = 0; round_score = 0; player_id = "drake"; is_AI = false; position = 3}]
let exchange2 = [("bob", [2; 19; 37]); ("charlie", [3; 14; 36]); ("drake", [4; 29; 38]); ("ellie", [1; 23; 40])]
let result2 = exchange_cards player_lst2 exchange2 player_lst2

let exchange_test = [
 	"ellie's cards 1" >:: (fun _ -> assert_equal [25; 26; 16; 17; 18] (List.hd result1).cards);
	"ellie's total score 1" >:: (fun _ -> assert_equal 0 (List.hd result1).total_score);
	"ellie's round score 1" >:: (fun _ -> assert_equal 0 (List.hd result1).round_score);
	"ellie's id 1" >:: (fun _ -> assert_equal "ellie" (List.hd result1).player_id);
	"ellie is_AI 1" >:: (fun _ -> assert_equal false (List.hd result1).is_AI);
	"ellie's position 1" >:: (fun _ -> assert_equal 0 (List.hd result1).position);

	"bob's cards 1" >:: (fun _ -> assert_equal [4; 5; 6; 7; 22; 23; 24] (List.nth result1 1).cards);
	"bob's total score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 1).total_score);
	"bob's round score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 1).round_score);
	"bob's id 1" >:: (fun _ -> assert_equal "bob" (List.nth result1 1).player_id);
	"bob is_AI 1" >:: (fun _ -> assert_equal false (List.nth result1 1).is_AI);
	"bob's position 1" >:: (fun _ -> assert_equal 1 (List.nth result1 1).position);

	"charlie's cards 1" >:: (fun _ -> assert_equal [11; 12; 13; 14; 15; 1; 2; 3] (List.nth result1 2).cards);
	"charlie's total score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 2).total_score);
	"charlie's round score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 2).round_score);
	"charlie's id 1" >:: (fun _ -> assert_equal "charlie" (List.nth result1 2).player_id);
	"charlie is_AI 1" >:: (fun _ -> assert_equal false (List.nth result1 2).is_AI);
	"charlie's position 1" >:: (fun _ -> assert_equal 2 (List.nth result1 2).position);

	"drake's cards 1" >:: (fun _ -> assert_equal [19; 20; 21; 8; 9; 10] (List.nth result1 3).cards);
	"drake's total score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 3).total_score);
	"drake's round score 1" >:: (fun _ -> assert_equal 0 (List.nth result1 3).round_score);
	"drake's id 1" >:: (fun _ -> assert_equal "drake" (List.nth result1 3).player_id);
	"drake is_AI 1" >:: (fun _ -> assert_equal false (List.nth result1 3).is_AI);
	"drake's position 1" >:: (fun _ -> assert_equal 3 (List.nth result1 3).position);


	"ellie's cards 2" >:: (fun _ -> assert_equal [22; 24; 25; 26; 2; 19; 37] (List.hd result2).cards);
	"ellie's total score 2" >:: (fun _ -> assert_equal 0 (List.hd result2).total_score);
	"ellie's round score 2" >:: (fun _ -> assert_equal 0 (List.hd result2).round_score);
	"ellie's id 2" >:: (fun _ -> assert_equal "ellie" (List.hd result2).player_id);
	"ellie is_AI 2" >:: (fun _ -> assert_equal false (List.hd result2).is_AI);
	"ellie's position 2" >:: (fun _ -> assert_equal 2 (List.hd result2).position);

	"bob's cards 2" >:: (fun _ -> assert_equal [5; 6; 7; 3; 14; 36] (List.nth result2 1).cards);
	"bob's total score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 1).total_score);
	"bob's round score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 1).round_score);
	"bob's id 2" >:: (fun _ -> assert_equal "bob" (List.nth result2 1).player_id);
	"bob is_AI 2" >:: (fun _ -> assert_equal false (List.nth result2 1).is_AI);
	"bob's position 2" >:: (fun _ -> assert_equal 1 (List.nth result2 1).position);

	"charlie's cards 2" >:: (fun _ -> assert_equal [8; 9; 10; 11; 12; 13; 15; 4; 29; 38] (List.nth result2 2).cards);
	"charlie's total score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 2).total_score);
	"charlie's round score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 2).round_score);
	"charlie's id 2" >:: (fun _ -> assert_equal "charlie" (List.nth result2 2).player_id);
	"charlie is_AI 2" >:: (fun _ -> assert_equal false (List.nth result2 2).is_AI);
	"charlie's position 2" >:: (fun _ -> assert_equal 0 (List.nth result2 2).position);

	"drake's cards 2" >:: (fun _ -> assert_equal [16; 17; 18; 20; 1; 23; 40] (List.nth result2 3).cards);
	"drake's total score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 3).total_score);
	"drake's round score 2" >:: (fun _ -> assert_equal 0 (List.nth result2 3).round_score);
	"drake's id 2" >:: (fun _ -> assert_equal "drake" (List.nth result2 3).player_id);
	"drake is_AI 2" >:: (fun _ -> assert_equal false (List.nth result2 3).is_AI);
	"drake's position 2" >:: (fun _ -> assert_equal 3 (List.nth result2 3).position);
]

(*play C10, D2, C2, C8  --- Player 2 has no clubs *)
(* play ellie: C2342, C-4, C2, C1, C10   Bob: C1, D3   Charlie: D10, C3     Drake: C5    *)
(* let turn_cards1 = List.sort (fun card1 card2 -> Pervasives.compare card1 card2) (play_turn player_lst1)
 *)
let player_lst3 =  [
	 {cards = [22; 23; 24; 25; 26]; total_score = 0; round_score = 0; player_id = "ellie"; is_AI = false; position = 0};
	 {cards = [1; 2; 3; 4; 5; 6; 7]; total_score = 0; round_score = 0; player_id = "bob"; is_AI = false; position = 1};
	 {cards = [8; 9; 10; 11; 12; 13; 14; 15]; total_score = 0; round_score = 0; player_id = "charlie"; is_AI = false; position = 2};
	 {cards = [16; 17; 18; 19; 20; 35]; total_score = 0; round_score = 0; player_id = "drake"; is_AI = false; position = 3}]
(* !HB ~ play ellie:  C10   Bob: D3   Charlie: C3     Drake: H10, C5    *)
(* let turn_cards2 = List.sort (fun card1 card2 -> Pervasives.compare card1 card2) (play_turn player_lst3)
 *)
let player_lst4 =  [
	 {cards = [22; 23; 24; 25; 26]; total_score = 0; round_score = 0; player_id = "ellie"; is_AI = false; position = 0};
	 {cards = [1; 2; 3; 4; 5; 6; 7]; total_score = 0; round_score = 0; player_id = "bob"; is_AI = false; position = 1};
	 {cards = [8; 9; 10; 11; 12; 13; 14; 15]; total_score = 0; round_score = 0; player_id = "charlie"; is_AI = false; position = 2};
	 {cards = [35]; total_score = 0; round_score = 0; player_id = "drake"; is_AI = false; position = 3}]
(* let turn_cards3 = List.sort (fun card1 card2 -> Pervasives.compare card1 card2) (play_turn player_lst4)
 *)
let turn_test = [
	(* "turn1" >:: (fun _ -> assert_equal [1; 14; 20; 22] turn_cards1);  *)
	(* "turn1B" >:: (fun _ -> assert_equal [2; 15; 17; 22] turn_cards1);  *)
	(* "turn2A" >:: (fun _ -> assert_equal [2; 15; 17; 22] turn_cards2);  *)
	(* "turn3A" >:: (fun _ -> assert_equal [2; 15; 22; 35] turn_cards3);  *)
	"HB" >:: (fun _ -> assert_equal true (!hearts_broken));
]
*)
let tests = "test suite" >::: points_turn_test @ legal_moves_test

let _ = run_test_tt_main tests
