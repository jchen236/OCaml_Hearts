(* Contains methods to parse json files as well as write them
 * for data saving. Utilizies the YoJson module. *)

(* Prints the top 5 player high scores from the leaderboard *)
val display_high_score : unit -> unit

(* Creates a new json file containing the default values for an individual player's statistics *)
val create_new_json_file : string -> unit

(* Resets an existing json file to the default values for an individual player's statistics *)
val reset_json_file : string -> unit

(* Updates an existing player's stats from the given username and stats from the last game*)
val update_existing_json: string -> bool -> int -> unit

(* Prints an individual player's statistics for the given username*)
val display_player_stats : string -> unit
