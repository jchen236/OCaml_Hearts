(* Contains methods to parse json files as well as write them
 * for data saving. Utilizies the YoJson module. *)

(* Takes in a json file and parses using the Yojson.Basic.json type. Only
 * creates a json data type that we can utilize for further processing. *)
val parse : string -> json

(* Takes in a json data type and parses data appropriately. Will consist of a
 * name, value pair. Used when trying to login into a username. Will return
 * an error message if name is not found. *)
val retrieve_stats : json -> (string * int)

(* Takes a username, password, and a state to record the player's current
 * stats for usage in the game. Produces a json type. If there is no json file
 * this method will also handle the creation of such file. *)
val save_user : string -> string -> state -> json

(* Using the provided information from retrieve_stats, this method will load
 * the player's corresponding score and name into the game before it starts. *)
val load_stats : (string * int) -> state
