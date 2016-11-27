open OUnit2
open Whoknows

let points_turn_test = "points turn test" >::: [
	"all spades" >:: (fun _ -> assert_equal ("p4", 0) (calculate_turn_result [("p1", 40); ("p2", 41); ("p3", 42); ("p4", 43)]));
	"all spades, queen of spades" >:: (fun _ -> assert_equal ("p1", 13) (calculate_turn_result [("p1", 50); ("p2", 41); ("p3", 42); ("p4", 43)]));
]

let tests = "test suite" >::: points_turn_test

let _ = run_test_tt_main tests