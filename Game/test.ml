open OUnit2
open Command
open Game
open State
open Sim

(* Helper Functions
 ******************************************************************************)


(** Helper method to create an assert_equals test *)
let create_eq_test
    (name:string)
    (expected_output:'a)
    (output:'a) =
  name >:: (fun _ ->
      assert_equal expected_output output)

(** Helper method to create an assert_raises test *)
let create_exception_test
    (name:string)
    (expected_error:exn)
    (expression:exn) =
  name >:: (fun _ ->
      assert_raises expected_error (fun () -> expression))

(** Helper method to create a list of assert_equals tests. The name of the tests
    will be given by [func_name] and the first argument in each assoc stored in
    [arr]. The second argument will be the [input] of [func], and the third
    argument will be the [expected] output. *)
let create_eq_test_list
    (func_name: string)
    (func: 'a -> 'b)
    (arr: (string * 'a * 'b) list) =
  List.map (fun (case,input,expected) ->
      let str = Printf.sprintf "%s: %s" func_name case in
      str >:: (fun _ ->
          assert_equal expected (func input)))
    arr

(** Helper method to create a list of assert_raises tests. The name of the tests
    will be given by [func_name] and the first argument in each assoc stored in
    [arr]. The second argument will be the [input] of [func], and the third
    argument will be the [expected] error. *)
let create_exception_test_list
    (func_name: string)
    (func: 'a -> 'b)
    (arr: (string * 'a * exn) list) =
  List.map (fun (case,input,expected) ->
      let str = Printf.sprintf "%s: %s" func_name case in
      str >:: (fun _ ->
          assert_raises expected (fun () -> func input)))
    arr

(** Helper method to create a player  *)
let build_single_player player_name position rating current_team price 
    player_id = {
  player_name = player_name; position = position; rating = rating;
  current_team = current_team; price = price; player_id = player_id;
  status = Rest; level = 0;
}

let get_from_result = function
  | Illegal -> failwith "Should not happen"
  | Legal t -> t

let retrieve_buy_sell_info 
    (func: State.t -> Game.player_name -> State.result)
    (st: State.t)
    (player_name: Game.player_name)
  = match func st player_name with
  | Illegal -> get_current_shop st
  | Legal new_st -> get_current_shop new_st

let sort_players = 
  List.sort (fun p1 p2 -> compare p1.player_name p2.player_name)

(** [data/test.json] holds the same team information as [data/game.json], except
    there are only two free agents available in the market: [Swen] and [Elroy].
    There are three different special events: [friendly match], [champions
    league game], [charity game], and [promotional game]. Your team id is [3],
    corresponding to the team OKC, [City Thunder]. Your starting team is:
    [Bart], [Titus], [Garfinkel], [Churchill], and [Jesse]. *)
let adv = from_json (Yojson.Basic.from_file "data/test.json")
let st = init_state adv ""
let player_list = [
  (build_single_player "Bart" "Small Forward" {
      pace=80; shooting=79; defending=70; passing=78; dribbling=87;
      physicality=77; overall=82} "City Thunder" 399 11);
  (build_single_player "Titus" "Shooting Guard" {
      pace=89; shooting=86; defending=76; passing=81; dribbling=83;
      physicality=81; overall=86} "City Thunder" 421 12);
  (build_single_player "Garfinkel" "Power Forward") {
    pace=80; shooting=80; defending=72; passing=82; dribbling=81;
    physicality=69; overall=81} "City Thunder" 393 13;
  (build_single_player "Churchill" "Point Guard") {
    pace=87; shooting=86; defending=85; passing=89; dribbling=89;
    physicality=82; overall=88} "City Thunder" 433 14;
  (build_single_player "Jesse" "Center") {
    pace=79; shooting=74; defending=84; passing=81; dribbling=75;
    physicality=86; overall=83} "City Thunder" 404 15;
]
let active_player_lst = List.map (fun p -> {p with status=Active}) player_list 
let market_lst = [
  (build_single_player "Swen" "Center") {
    pace=82; shooting=73; defending=84; passing=81; dribbling=85;
    physicality=86; overall=85} "" 415 91;
  (build_single_player "Elroy" "Shooting Guard") {
    pace=77; shooting=86; defending=81; passing=71; dribbling=86;
    physicality=89; overall=87} "" 427 92;
]
let st_active = List.fold_left
    (fun st p -> active st p.player_name |> get_from_result) st player_list
let sim = init_sim Balanced
let rating = {attack=0; defense=0; chemistry=72; overall=0} 
let init_team = {team_name="City Thunder"; description="Team City Thunder";
                 play_style="balanced"; team_rating=rating; players=[];
                 team_id=3; tactic=Balanced}

(* Testing suite
 *****************************************************************************)

(** Test command parsing for the Command module *)
let parse_tests = List.flatten [
    create_exception_test_list "parse invalid" (parse) [
      "Empty", "", Empty;
      "Team Malformed", "team test", Malformed;
      "Shop Malformed", "shop test", Malformed;
      "Next_day Malformed", "nextday test", Malformed;
      "Quit Malformed", "quit test", Malformed;
    ];
    create_eq_test_list "parse with no extra values" (parse) [
      "quit", "quit", Quit;
      "team", "team", Team;
      "shop", "shop", Shop;
      "next_day", "nextday", Next_day;
      "help", "help", Help;
      "practice", "practice", Practice
    ];
    create_eq_test_list "parse with one value" (parse) [
      "buy player", "buy player", Buy ["player"];
      "sell player", "sell player", Sell ["player"];
      "scout player", "scout player", Scout ["player"];
      "challenge player", "challenge player", Challenge ["player"];
      "activate player", "activate player", Activate ["player"];
      "rest player", "rest player", Rest ["player"];
      "analyze team", "analyze team", Analyze ["team"];
      "move place", "move place", Move ["place"];
      "train player", "train player", Train ["player"];
      "update tatics", "update tatics", Update ["tatics"]
    ];
    (* Parsing with two or more values *)
    create_eq_test_list "parse with two plus values" (parse) [
      "buy michael jordan", "buy michael jordan", Buy ["michael";"jordan"];
      "sell My Star Player", "sell My Star Player", Sell ["My";"Star";"Player"];
      "scout That guy over ThEre", "scout That guy over ThEre", Scout 
        ["That";"guy";"over";"ThEre"];
    ];
  ]

let move_tests = List.flatten [
    create_eq_test_list "test move" (move st) [
      "invalid", ["ma in"], Illegal;
      "main valid", ["main"], Legal st;
    ];

    create_eq_test_list "test move"
      (fun x -> x |> get_from_result |> get_current_location) [
      "main", move st ["main"], Main;
      "arena", move st ["arena"], Arena;
      "train", move st ["training"], Training_Field;
      "shop", move st ["shop"], Shop
    ];
  ]

let activate_tests = List.flatten [
    [create_eq_test "test activate: None" ([]) 
       (st |> get_current_active_players)];
    [create_eq_test "test activate: all" (active_player_lst)
       (st_active |> get_current_active_players)];
    create_eq_test_list "test activate" (active st_active) [
      "Invalid", "None", Illegal;
      "Already Acitvated", "Bart", Illegal
    ];
    create_eq_test_list "test activate length name"
      (fun x -> x |> get_current_active_players |> List.length) [
      "none active", (st), 0;
      "all active", (st_active), 5;
      "activate Bart", (active st "Bart" |> get_from_result), 1;
      "activate Titus", (active st "Titus" |> get_from_result), 1;
      "activate Garfinkel", (active st "Garfinkel" |> get_from_result), 1;
      "activate Churhill", (active st "Churchill" |> get_from_result), 1;
      "activate Jesse", (active st "Jesse" |> get_from_result), 1;
    ];
    create_eq_test_list "test activate player name"
      (fun x -> x |> get_from_result |> get_current_active_players) [
      "Bart", active st "Bart", [List.nth active_player_lst 0];
      "Titus", active st "Titus", [List.nth active_player_lst 1];
      "Garfinkel", active st "Garfinkel", [List.nth active_player_lst 2];
      "Churchill", active st "Churchill", [List.nth active_player_lst 3];
      "Jesse", active st "Jesse", [List.nth active_player_lst 4]
    ];
    create_eq_test_list "test activate length id"
      (fun x -> x |> get_current_active_players |> List.length) [
      "activate Bart", (active st "11" |> get_from_result), 1;
      "activate Titus", (active st "12" |> get_from_result), 1;
      "activate Garfinkel", (active st "13" |> get_from_result), 1;
      "activate Churhill", (active st "14" |> get_from_result), 1;
      "activate Jesse", (active st "15" |> get_from_result), 1;
    ];
    create_eq_test_list "test activate player id"
      (fun x -> x |> get_from_result |> get_current_active_players) [
      "Bart", active st "11", [List.nth active_player_lst 0];
      "Titus", active st "12", [List.nth active_player_lst 1];
      "Garfinkel", active st "13", [List.nth active_player_lst 2];
      "Churchill", active st "14", [List.nth active_player_lst 3];
      "Jesse", active st "15", [List.nth active_player_lst 4]
    ];
  ]

let initial_load_tests = [
  create_eq_test "load_test: location" (Main) (get_current_location st);
  create_eq_test "load test: money" (2000) (get_current_money st);
  create_eq_test "load test: date" (1) (get_current_date st);
  create_eq_test "load test: rating" 
    ({attack=0; defense=0; chemistry=72; overall=0})
    (get_current_rating st);
  create_eq_test "load test: active players" [] (get_current_active_players st);
  create_eq_test "load test: current players"
    (player_list |> List.map (fun p -> p.player_name)) (get_current_players st);
  create_eq_test "load test: energy" (50) (get_current_energy st);
  create_eq_test "load test: market" (market_lst |> sort_players)
    (get_current_market st |> sort_players);
  create_eq_test "load test: sim" (sim) (get_current_sim st);
]

(** Test buy player for the State module *)
let buy_tests = List.flatten [
    create_eq_test_list "test buy" (buy st) [
      "invalid", "", Illegal;
      "player in team", "Bart", Illegal;
      "player in opp team", "Issak", Illegal;
    ];
    create_eq_test_list "test buy" (retrieve_buy_sell_info buy st) [
      "invalid", "", ["Swen"; "Elroy"];
      "invalid", "N/A", ["Swen"; "Elroy"];
      "Swen name", "Swen", ["Elroy"];
      "Elroy name", "Elroy", ["Swen"];
      "Swen id", "91", ["Elroy"];
      "Elroy id", "92", ["Swen"];
    ];
    create_eq_test_list "test invalid buy" (retrieve_buy_sell_info buy st) [
      "invalid", "", ["Swen"; "Elroy"];
      "invalid", "N/A", ["Swen"; "Elroy"];
      "Swen name", "Swenk", ["Swen"; "Elroy"];
      "Elroy name", "ElroKy", ["Swen"; "Elroy"];
      "Swen id", "102", ["Swen"; "Elroy"];
      "Elroy id", "104", ["Swen"; "Elroy"];
    ];
  ]


(** Helper method to create a player  *)
let build_single_player 
    player_name 
    position 
    rating 
    current_team 
    price 
    player_id
    status = {
  player_name = player_name;
  position = position;
  rating = rating;
  current_team = current_team;
  price = price;
  player_id = player_id;
  status = status;
  level = 0;
}

(** Test sell player for the State module *)
let sell_tests = List.flatten [
    create_eq_test_list "test sell" (retrieve_buy_sell_info sell st) [
      "invalid", "", ["Swen"; "Elroy"];
      "Bart name", "Bart", ["Bart"; "Swen"; "Elroy"];
      "Titus name", "Titus", ["Titus"; "Swen"; "Elroy"];
      "Garfinkel name", "Garfinkel", ["Garfinkel"; "Swen"; "Elroy"];
      "Churchill name", "Churchill", ["Churchill"; "Swen"; "Elroy"];
      "Jesse name", "Jesse", ["Jesse"; "Swen"; "Elroy"];
      "Bart id", "11", ["Bart"; "Swen"; "Elroy"];
      "Titus id", "12", ["Titus"; "Swen"; "Elroy"];
      "Garfinkel id", "13", ["Garfinkel"; "Swen"; "Elroy"];
      "Churchill id", "14", ["Churchill"; "Swen"; "Elroy"];
      "Jesse id", "15", ["Jesse"; "Swen"; "Elroy"];
    ];
  ]

(** Test scout player for the State module *)
let scout_tests = List.flatten [
    create_eq_test_list "test scout" (scout st) [
      "None", "Ok", None;
      "Bart", "Bart", Some (List.nth player_list 0);
      "Titus", "Titus", Some (List.nth player_list 1);
      "Garfinkel", "Garfinkel", Some (List.nth player_list 2);
      "Churchill", "Churchill", Some (List.nth player_list 3);
      "Jesse", "Jesse", Some (List.nth player_list 4);
    ];
  ]

let analyze_tests = [
  create_eq_test "test analyze: none activated" (Some init_team)
    (analysis st adv "City Thunder");
]

let rest_tests = List.flatten [
    create_eq_test_list "test rest default"
      (fun x -> x |> get_current_active_players |> List.length) [
      "all active", st_active, 5;
      "none active", st, 0;
    ];
    create_eq_test_list "test rest players in market" (rest st) [
      "Swen name", "Swen", Illegal;
      "Elroy name", "Elroy", Illegal;
      "Swen id", "91", Illegal;
      "Elroy id", "92", Illegal;
    ];
    create_eq_test_list "test rest players in other teams" (rest st) [
      "Chan name", "Chan", Illegal;
      "Pat name", "Pat", Illegal;
      "Patrick id", "1", Illegal;
      "Laurens id", "68", Illegal;
    ];
    create_eq_test_list "test activate players in other teams" (active st) [
      "Chan name", "Chan", Illegal;
      "Pat name", "Pat", Illegal;
      "Patrick id", "1", Illegal;
      "Laurens id", "68", Illegal;
    ];
    create_eq_test_list "test rest already resting" (rest st) [
      "Bert name", "Bert", Illegal;
      "Titus name", "Titus", Illegal;
      "Garfinkel name", "Garfinkel", Illegal;
      "Churchill name", "Churchill", Illegal;
      "Jesse name", "Jesse", Illegal;
      "Bert id", "11", Illegal;
      "Titus id", "12", Illegal;
      "Garfinkel id", "13", Illegal;
      "Churchill id", "14", Illegal;
      "Jesse id", "15", Illegal;
    ];
    create_eq_test_list "test rest length names"
      (fun x -> 
         x |> get_from_result |> get_current_active_players |> List.length) [
      "rest Bart", rest st_active "Bart", 4;
      "rest Titus", rest st_active "Titus", 4;
      "rest Garfinkel", rest st_active "Garfinkel", 4;
      "rest Churchill", rest st_active "Churchill", 4;
      "rest Jesse", rest st_active "Jesse", 4;
    ];
    create_eq_test_list "test rest length ids"
      (fun x -> 
         x |> get_from_result |> get_current_active_players |> List.length) [
      "rest Bart", rest st_active "11", 4;
      "rest Titus", rest st_active "12", 4;
      "rest Garfinkel", rest st_active "13", 4;
      "rest Churchill", rest st_active "14", 4;
      "rest Jesse", rest st_active "15", 4;
    ];
    create_eq_test_list "test rest everyone"
      (fun x -> 
         x |> get_from_result |> get_current_active_players |> List.length) [
      "rest everyone", rest st_active "everyone", 0;
      "rest everyone", rest st "everyone", 0;
    ];
  ]

let train_tests = List.flatten [
    create_eq_test_list "test train invalid init_st" (train st) [
      "not a player", "N/A", Illegal;
      "agent in market Swen", "Swen", Illegal;
      "agent in market Elroy", "Elroy", Illegal;
      "agent in market Pat", "Pat", Illegal;
      "agent in market Chan", "Chan", Illegal;
      "agent in market Kelwin", "Kelwin", Illegal;
      "agent in market Menard", "Menard", Illegal;
      "agent in market Laurens", "Laurens", Illegal;
      "agent in market Fred", "Fred", Illegal;

    ];
    create_eq_test_list "test train increase level"
      (fun x -> x |> get_from_result |> get_current_team
                |> List.filter (fun p -> p.level = 1)
                |> List.map (fun p -> p.player_name)) [
      "Bart name", train st "Bart", ["Bart"];
      "Titus name", train st "Titus", ["Titus"];
      "Garfinkel name", train st "Garfinkel", ["Garfinkel"];
      "Churchill name", train st "Churchill", ["Churchill"];
      "Jesse name", train st "Jesse", ["Jesse"];
      "Bart id", train st "11", ["Bart"];
      "Titus id", train st "12", ["Titus"];
      "Garfinkel id", train st "13", ["Garfinkel"];
      "Churchill id", train st "14", ["Churchill"];
      "Jesse id", train st "15", ["Jesse"];
    ];
  ]

let challenge_tests = [
  create_eq_test "test challenge: invalid" (Illegal)
    (challenge st_active "Not a team");
  create_eq_test "test challenge: not enough players" (Illegal)
    (challenge st "Lakers");
  create_eq_test "test challenge: energy" (20)
    (challenge st_active "Lakers" |> get_from_result |> get_current_energy);
]

let event_tests = [
  create_eq_test "test event: invalid" (0)
    (match (get_current_event st) with 
     | None -> 0
     | Some h -> (yes st h |> get_from_result |> get_current_energy)
    );
  create_eq_test "test event: invalid" (0)
    (match (get_current_event st) with 
     | None -> 0
     | Some h -> (yes st h |> get_from_result |> get_current_energy)
    );
]

let update_tests = List.flatten [
    create_eq_test_list "test update" 
      (fun x -> x |> get_from_result |> get_current_sim) [
      "change UltraDefensive", update st "ultra_defensive", init_sim (UltraDefensive);
      "change Defensive", update st "defensive", init_sim (Defensive);
      "change Balanced", update st "balanced", init_sim (Balanced);
      "change Attacking", update st "attacking", init_sim (Attacking);
      "change UltraAttacking", update st "ultra_attacking", init_sim (UltraAttacking);
    ];
  ]


let suite =
  "test suite for Midterm Project" >::: List.flatten [
    parse_tests;
    initial_load_tests;
    scout_tests;
    analyze_tests;
    buy_tests;
    sell_tests;
    move_tests;
    activate_tests;
    rest_tests;
    update_tests;
    challenge_tests;
    train_tests;
    event_tests;
  ]

let _ = run_test_tt_main suite