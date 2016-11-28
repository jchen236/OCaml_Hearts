open OUnit2
open Whoknows

let points_turn_test = "points turn test" >::: [
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

let legal_moves_test = "legal moves test" >::: [
	"all spades, hearts not broken" >:: (fun _ -> assert_equal [42; 41] (get_legal_moves (Some 40) false [1; 14; 27; 42; 41] ));
	"all diamonds, hearts not broken" >:: (fun _ -> assert_equal [1; 2] (get_legal_moves (Some 3) false [1; 2; 14; 27; 42; 41] ));
	"all clubs, hearts not broken" >:: (fun _ -> assert_equal [14; 15] (get_legal_moves (Some 16) false [1; 2; 14; 15; 27; 42; 41] ));
	"all spades, HB" >:: (fun _ -> assert_equal [42; 41] (get_legal_moves (Some 40) true [1; 14; 27; 42; 41] ));
	"all diamonds, HB" >:: (fun _ -> assert_equal [1; 2] (get_legal_moves (Some 3) true [1; 2; 14; 27; 42; 41] ));
	"all clubs, HB" >:: (fun _ -> assert_equal [14; 15] (get_legal_moves (Some 16) true [1; 2; 14; 15; 27; 42; 41] ));
	"all hearts" >:: (fun _ -> assert_equal [27; 28] (get_legal_moves (Some 29) true [1; 2; 14; 15; 27; 28; 42; 41] ));
	"spades, but you have no spades" >:: (fun _ -> assert_equal [1; 14; 27] (get_legal_moves (Some 40) true [1; 14; 27] ));
	"spades, but you have no spades HB" >:: (fun _ -> assert_equal [1; 14; 27] (get_legal_moves (Some 40) false [1; 14; 27] ));
	"diamonds, but you have no diamonds" >:: (fun _ -> assert_equal [14; 15; 27; 28; 42; 41] (get_legal_moves (Some 3) false [14; 15; 27; 28; 42; 41] ));
	"diamonds, but you have no diamonds HB" >:: (fun _ -> assert_equal [14; 15; 27; 28; 42; 41] (get_legal_moves (Some 3) true [14; 15; 27; 28; 42; 41] ));
	"clubs, but you have no clubs" >:: (fun _ -> assert_equal [1; 2; 27; 28; 42; 41] (get_legal_moves (Some 16) false [1; 2; 27; 28; 42; 41] ));
	"clubs, but you have no clubs HB" >:: (fun _ -> assert_equal [1; 2; 27; 28; 42; 41] (get_legal_moves (Some 16) true [1; 2; 27; 28; 42; 41] ));
	"hearts, but you have no hearts" >:: (fun _ -> assert_equal [1; 2; 14; 15; 42; 41] (get_legal_moves (Some 29) true [1; 2; 14; 15; 42; 41] ));
	"First player, !HB" >:: (fun _ -> assert_equal [1; 2; 14; 15; 42; 41] (get_legal_moves None false [1; 2; 14; 15; 27; 28; 42; 41] ));
	"First player, HB" >:: (fun _ -> assert_equal [1; 2; 14; 15; 27; 28; 42; 41] (get_legal_moves None true [1; 2; 14; 15; 27; 28; 42; 41] ));
	"First player, have only hearts but !HB" >:: (fun _ -> assert_equal [27; 28;] (get_legal_moves None true [27; 28] ));
]

let tests = "test suite" >::: [points_turn_test] @ [legal_moves_test]

let _ = run_test_tt_main tests