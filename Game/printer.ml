open Game
open State
open Command
open Sim

(* Helper Functions for Printing
 ******************************************************************************)

(** The number of columns in the currently opened terminal that the game is
    running in. *)
let width : int ref = ref 0

(** The number of rows in the currently opened terminal that the game is running
    in. *)
let height : int ref = ref 0

(** The current sort setting for all interactive tables that print one or more
    teams. *)
let team_table_sort : team_sort ref = ref TeamNone

(** The current sort setting for all interactive tables that print one or more 
    players. *)
let player_table_sort : player_sort ref = ref PlayerNone

(* Defined in module *)
let reset_sort () =
  team_table_sort := TeamNone; player_table_sort := PlayerNone

(* Defined in module *)
let set_player_sort sort = player_table_sort := sort

(* Defined in module *)
let set_team_sort sort = team_table_sort := sort

(** The ordinal of a number related the the quarter. Used for printing game
    results in beta project stage. *)
let quarter_of_int = function
  | 0 -> "1st Quarter"
  | 1 -> "2nd Quarter"
  | 2 -> "3rd Quarter"
  | 3 -> "4th Quarter"
  | _ -> failwith "impossible - printer.ml quarter_of_int"

(** [status_to_string status] is the string representation of a 
    player status. *)
let status_to_string : status -> string = function
  | Active -> "active" | Rest -> "rest"

(** [sort_team_list lst] is a sorted version of [lst] with respect to the
    current sort setting given by [team_table_sort]. Used for team list
    tables. *)
let sort_team_list lst = match !team_table_sort with
  | TeamName ->
    List.sort (fun t1 t2 -> compare t1.team_name t2.team_name) lst
  | Description ->
    List.sort (fun t1 t2 -> compare t1.description t2.description) lst
  | TeamID ->
    List.sort (fun t1 t2 -> t1.team_id - t2.team_id) lst
  | Offence ->
    List.sort (fun t1 t2 -> t1.team_rating.attack - t2.team_rating.attack) lst
  | Defence ->
    List.sort (fun t1 t2 -> t1.team_rating.defense - t2.team_rating.defense) lst
  | TeamOverall ->
    List.sort (fun t1 t2 -> t1.team_rating.overall - t2.team_rating.overall) lst
  | TeamNone -> lst

(** [sort_team_list lst] is a sorted version of [lst] with respect to the
    current sort setting given by [team_table_sort]. Used for player list
    tables. *)
let sort_player_list lst = match !player_table_sort with
  | PlayerName ->
    List.sort (fun p1 p2 -> compare p1.player_name p2.player_name) lst 
  | Price ->
    List.sort (fun p1 p2 -> p1.price - p2.price) lst
  | Level ->
    List.sort (fun p1 p2 -> p1.level - p2.level) lst 
  | Position ->
    List.sort (fun p1 p2 -> compare p1.position p2.position) lst 
  | PlayerID ->
    List.sort (fun p1 p2 -> p1.player_id - p2.player_id) lst
  | Status ->
    List.sort (fun p1 p2 -> compare (status_to_string p1.status)
                  (status_to_string p2.status)) lst
  | Pace ->
    List.sort (fun p1 p2 -> p1.rating.pace - p2.rating.pace) lst
  | Shooting ->
    List.sort (fun p1 p2 -> p1.rating.shooting - p2.rating.shooting) lst
  | Defending ->
    List.sort (fun p1 p2 -> p1.rating.defending - p2.rating.defending) lst
  | Passing ->
    List.sort (fun p1 p2 -> p1.rating.passing - p2.rating.passing) lst
  | Dribbling ->
    List.sort (fun p1 p2 -> p1.rating.dribbling - p2.rating.dribbling) lst
  | Physicality ->
    List.sort (fun p1 p2 -> p1.rating.physicality - p2.rating.physicality) lst
  | PlayerOverall | Cost ->
    List.sort (fun p1 p2 -> p1.rating.overall - p2.rating.overall) lst
  | PlayerNone -> lst

(* Table headers for players *)
let generic_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Player Stats: "; "|| Pace "; "|| Shooting "; "|| Defending "; 
  "|| Passing "; "|| Dribbling "; "|| Physicality "; "|| Overall ";
]

(* Table headers for players with a team *)
let scout_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Current Team "; "|| Player Stats: "; "|| Pace "; "|| Shooting "; 
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

(* Table headers for players in the team menu *)
let user_player_headers = [
  "|     Name    "; "|| Price "; "||   Position     "; "|| ID # ";
  "|| Status "; "|| Player Stats: "; "|| Pace "; "|| Shooting ";
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

(* Table headers for players in the training field *)
let train_player_headers = [
  "|     Name    "; "||  Cost "; "|| Level "; "||   Position     "; "|| ID # ";
  "|| Status "; "|| Player Stats: "; "|| Pace "; "|| Shooting ";
  "|| Defending "; "|| Passing "; "|| Dribbling "; "|| Physicality "; 
  "|| Overall ";
]

(* Table headers for teams *)
let team_headers = [
  "|    Team Name    "; "||    Description    "; "|| ID # ";
  "|| Team Stats: " ; "|| Offence " ; "|| Defence "; "|| Overall ";
]

(* Table headers for your team in the team menu *)
let your_team_headers = [
  "|    Team Name    "; "||    Description    "; "|| ID # ";
  "|| Team Stats: "; "|| Offence " ; "|| Defence "; "|| Overall ";
  "|| Team Record: "; "|| Wins "; "|| Draws "; "|| Losses "
]

(* Table headers for match result *)
let game_result_headers = [
  "|               "; "|| Your Score "; "|| Opponents Score";
]

(* Table headers for help command *)
let help_headers = [
  "|        Command       "; 
  "||                            Description                           "
]

(* Array of information for players *)
let generic_player_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int;
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int])

(* Array of information for players with a team *)
let scout_player_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; p.current_team; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int])

(* Array of information for active players in team menu *)
let user_active_player_array lst = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; "Active"; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int]) 
    (List.filter (fun p -> p.status = Active) lst)

(* Array of information for resting players in team menu *)
let user_resting_player_array lst = List.map (fun p ->
    let r = p.rating in
    [p.player_name; p.price |> string_of_int; p.position;
     p.player_id |> string_of_int; "Rest"; ""; r.pace |> string_of_int;
     r.shooting |> string_of_int; r.defending |> string_of_int; 
     r.passing |> string_of_int; r.dribbling |> string_of_int; 
     r.physicality |> string_of_int; r.overall |> string_of_int]) 
    (List.filter (fun p -> p.status = Rest) lst)

(* Array of information for teams *)
let team_array = List.map (fun t ->
    let r = t.team_rating in
    [t.team_name; t.description; t.team_id |> string_of_int; "";
     r.attack |> string_of_int; r.defense |> string_of_int;
     r.overall |> string_of_int])

(* Array of information for your team in team menu *)
let your_team_array sim_t = List.map (fun t ->
    let r = t.team_rating in
    [t.team_name; t.description; t.team_id |> string_of_int; "";
     r.attack |> string_of_int; r.defense |> string_of_int;
     r.overall |> string_of_int; ""; sim_t |> get_wins |> string_of_int;
     sim_t |> get_draws |> string_of_int; sim_t |> get_losses |> string_of_int])

(* Array of information for players in training field *)
let train_array = List.map (fun p ->
    let r = p.rating in
    [p.player_name; (r.overall * 3) |> string_of_int; p.level |> string_of_int; 
     p.position; p.player_id |> string_of_int; 
     if p.status=Rest then "Rest" else "Active"; ""; 
     r.pace |> string_of_int; r.shooting |> string_of_int;
     r.defending |> string_of_int; r.passing |> string_of_int; 
     r.dribbling |> string_of_int; r.physicality |> string_of_int; 
     r.overall |> string_of_int])

(* Array of commands and their respective descriptions *)
let help_array = [
  ["team"; "Display your team information and manage your active players."];
  ["quit"; "Quit the game."];
  ["help"; "Display the possible commands."];
  ["scout [playername]"; "Display the information about [playername]."];
  ["analyze [teamname]"; "Display the information about [teamname]."];
  ["move [location]"; "Move to [location] menu."]
]
(* Array of commands and their respective descriptions specific to the 
   main menu *)
let help_array_main = [
  ["nextday"; "Move on to the next day, update your information."];
]

(* Array of commands and their respective descriptions specific to the 
   shop menu *)
let help_array_shop = [
  ["shop"; "Enter the player shop."]
]

(* Array of commands and their respective descriptions specific to the
   arena menu *)
let help_array_arena = [
  ["arena"; "Enter the game arena."]
]

(* Array of commands and their respective descriptions specific to the
   training menu *)
let help_array_training = [
  ["practice"; "Enter the training field."]
]

(** [game_result_array result] is the display of the score at the end of each
    quarter based on [result]. *)
let game_result_array result = List.mapi (fun i (our,opp) -> 
    [quarter_of_int i; our |> string_of_int; opp |> string_of_int])
    (List.combine result.our_score result.opp_score)

(** [your_active_team adv st] is the user's team with at the game state gievn by
    [st]. Retrieves the information from the game file [adv] and replaces the
    rating and active players with those given by [st]. *)
let your_active_team adv state =
  match get_your_team adv |> get_single_team adv with
  | None -> failwith "impossible - printer.ml your_active_team"
  | Some h -> {h with team_rating= get_current_rating state;
                      players= get_current_active_players state}

(** [wait_for_enter ()] waits for the user to press enter in stdin, until the
    enter key is pressed,  *)
(* Sourced from https://stackoverflow.com/questions/13410159/how-to-read-a-character-in-ocaml-without-a-return-key *)
let wait_for_enter () =
  print_string "-- PRESS ENTER --";
  flush stdout;
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_echo = false; } in
  ignore (input_line stdin);
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  ignore(Sys.command "stty echo");
  ()

(** [wait_for_yes_no ()] is [true] if the user types 'y', or [false] if the user
    types 'n'. This function waits for the user to press either 'y' or 'n' in
    stdin, by wich it will return from the pattern match. The user's inputs will
    not be echoed back to them. *)
let wait_for_yes_no () =
  print_string "[y/n]";
  flush stdout;
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_echo = false; } in
  let rec read () =
    match input_char stdin with
    | 'y' | 'Y' -> true
    | 'n' | 'N' -> false
    | _ -> read () in
  let ans = read () in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  ignore(Sys.command "stty echo");
  ans


(** [print_table headers rows] prints a table with headers given by [headers]
    and rows given by [rows]. If a message should be printed at the top of the
    table, you can specify the message in the optional argument [msg].

    Requires: len([headers]) = len(elements of [rows]) *)
let print_table ?(msg:string option = None) (headers:string list)
    (rows:string list list) =
  (* Setup variables *)
  let length = String.length (List.fold_left (^) "" headers) in
  let headers_len = List.map (String.length) headers in
  (** [add_row init row] adds the row to init, but if any strings in [row] will
      not fit correctly in the column, it splits the string and creates a new
      row containing the parts that wouldn't fit. *)
  let rec add_row ?(rev:bool = true) init row = 
    (** [row_assoc] is an association list of [(str,l)], where str is the string
        in the row, and l is the max length of the where it fits in its column.
        To find l we take the length of the column's header and subtract 3
        (b/c we print ["|| " ^ str] and [String.length "|| " = 3]). *)
    let row_assoc = 
      List.map (fun (str,l) -> (str,l-3)) (List.combine row headers_len) in
    let all_short = 
      List.for_all (fun (str,l) -> String.length str < l) row_assoc in
    if all_short then row::init else 
      let first_row = List.map (fun (str,l) -> if str = "" then "" else
                                   String.sub str 0 (min (String.length str) l))
          row_assoc in
      let next_rows = List.map (fun (str,l) ->
          if String.length str < l
          then "" else String.sub str l (String.length str - l))
          row_assoc in
      if rev then List.rev (add_row ([first_row]) next_rows ~rev:false) @ init
      else add_row [first_row] next_rows ~rev:false @ init in 
  let split_rows = List.fold_left (add_row) [] (List.rev rows) in

  (* Print table header *)
  Format.open_tbox (); Format.set_tab ();
  let () = match msg with
    | None -> Format.print_tab ();
    | Some a -> Format.print_string a; Format.print_tab (); in
  Format.print_string ("|" ^ (String.make (length-1) '=') ^ "|");
  Format.print_tab ();
  List.iter (fun (x:string) -> Format.print_string x; Format.set_tab ())
    (headers);
  Format.print_string ("|\n|" ^ (String.make (length-1) '=') ^ "|");

  (* Print the table information from a list of entries *)
  let rec print_rows = function
    | [] -> ()
    | h::t ->
      Format.print_tab ();
      List.iteri (fun i str ->
          if i=0 then (Format.printf "| %s" str; Format.print_tab ())
          else (Format.printf "|| %s" str; Format.print_tab ())) h;
      Format.printf "|"; print_rows t; in
  print_rows split_rows;
  Format.print_string ("\n|" ^ (String.make (length-1) '=') ^ "|");
  Format.print_string "\n\n";
  Format.print_flush ();
  ()


(* Defined in module *)
let print_red = ANSITerminal.(print_string [red])

(* Defined in module *)
let print_blue = ANSITerminal.(print_string [blue])

(** [rebase_size ()] updates the width variable to the current width of the
    terminal. Done by storing the size in a text file, reading the file, then
    deleting it. *)
let rebase_size () =
  ignore(Sys.command "stty size > size.txt");
  let channel = open_in "size.txt" in
  let nums = channel |> input_line |> String.split_on_char ' ' in
  width := int_of_string (List.nth nums 1);
  height := int_of_string (List.nth nums 0);
  close_in channel;
  ignore(Sys.command "rm -f \"size.txt\"")


(** [print_stars ()] prints a row of stars exactly spanning the current 
    terminal width. *)
let print_stars () =
  rebase_size ();
  print_endline (String.make !width '*')


(* Defined in module *)
let loc_to_string = function
  | Main -> "Main"
  | Shop -> "Shop"
  | Arena -> "Arena"
  | Training_Field -> "Training Field"

(* Defined in module *)
let cmd_to_string = function
  | Challenge _ -> "challenge"
  | Team -> "team"
  | Arena -> "arena"
  | Shop -> "shop"
  | Activate _ -> "activate"
  | Rest _ -> "rest"
  | Buy _ -> "buy"
  | Sell _ -> "sell"
  | Scout _ -> "scout"
  | Analyze _ -> "analyze"
  | Next_day -> "next_day"
  | Quit -> "quit"
  | Move _ -> "move"
  | Help -> "help"
  | Train _ -> "train"
  | Practice -> "practice"
  | Update _ -> "update"
  | Sort _ -> "sort"
  | Yes -> "yes"
  | No -> "no"

(* Ascii Graphics Printing
 ******************************************************************************)

(** [print_lines num input] prints [num] lines from the channel [input] to
    stdout. *)
let print_lines num input =
  for i=1 to num do print_endline (input_line input) done

(** [print_red_lines] prints [num] lines from the channel [input] to stdout. The
    printed lines come out red. *)
let print_red_lines num input =
  for i=1 to num do print_red ((input_line input) ^ "\n") done

(** Prints the graphic that shows when loading the game. *)
let print_start_screen_graphic () =
  (* start_screen_logo.txt has 25 lines of text *)
  ignore(Sys.command "stty -echo");
  let input = open_in "graphics/start_screen_logo.txt" in
  (* Print Basketball Man *)
  print_lines 5 input; Unix.sleepf 0.5;
  print_lines 7 input; Unix.sleepf 0.5;
  print_lines 7 input; Unix.sleepf 0.75;
  (* Print logo *)
  for i=1 to 6 do print_lines 1 input; Unix.sleepf 0.125 done;
  ignore(Sys.command "stty echo");
  close_in input

(** Prints the game name on the start screen. *)
let print_game_title () =
  ignore(Sys.command "stty -echo");
  (* game_title.txt has 12 lines of text *)
  let input = open_in "graphics/game_title.txt" in
  print_red_lines 12 input;
  ignore(Sys.command "stty echo");
  close_in input

(* defined in module *)
let print_training_graphic () =
  ignore(Sys.command "stty -echo && clear");
  rebase_size ();
  if !height >= 28 then (* 28 chosen as half of # lines of largest file *)
    (* train_large_graphic_one.txt has 56 lines of text *)
    (* train_large_graphic_two.txt has 43 lines of text *)
    let input1 = open_in "graphics/train_large_graphic_one.txt" in
    let input2 = open_in "graphics/train_large_graphic_two.txt" in
    print_lines (min 56 !height) input1;
    Unix.sleepf 1.5; ignore(Sys.command("clear"));
    print_lines (min 43 !height) input2;
    Unix.sleepf 1.5; ignore(Sys.command("clear"));
    ignore(Sys.command "stty echo");
    close_in input1; close_in input2
  else 
    (* train_small_graphic_one has 6 lines of text *)
    (* train small_graphic two has 6 lines of text *)
    let input1 = open_in "graphics/train_small_graphic_one.txt" in
    let input2 = open_in "graphics/train_small_graphic_two.txt" in
    print_lines 6 input1; Unix.sleepf 1.5; ignore(Sys.command("clear"));
    print_lines 6 input2; Unix.sleepf 1.5; ignore(Sys.command("clear"));
    ignore(Sys.command "stty echo");
    close_in input1; close_in input2


(* Main Printing Functions for Game Implementation
 ******************************************************************************)

(* Defined in module *)
let print_title_screen () =
  ignore(Sys.command "clear");
  print_start_screen_graphic ();
  Unix.sleepf 1.75;

  ignore(Sys.command "clear");
  print_stars ();
  print_game_title ();
  ANSITerminal.(print_string [red]
                  "\nWelcome to Fantasy Sports Managers.\n";);
  print_endline "Please enter the name of the game file you want to load.\n";
  print_endline "Note: small terminal dimensions may cause broken graphics. \
                 If you see broken graphics, please resize the window to be \
                 sufficiently wide.\n";
  print_stars ();
  print_string  "\n> "


(* Defined in module *)
let print_cmd_not_possible cmd =
  let general =
    "Command not possible in current location.\n "
  in match cmd with
  | Shop | Sell _ | Buy _ -> 
    print_string (general ^ "Try 'move shop' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Arena | Challenge _ | Update _ ->
    print_string (general ^ "Try 'move arena' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Next_day -> (* | Press -> *)
    print_string (general ^ "Try 'move main' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Train _ | Practice ->
    print_string (general ^ "Try 'move arena' before doing the '" ^
                  cmd_to_string cmd ^ "' command.\n")
  | Activate _ | Rest _ ->
    print_string (general 
                  ^ "To manage your team, try the 'team' command \
                     before doing the '" ^ cmd_to_string cmd 
                  ^ "' command.\n")
  | Quit | Help | Team | Scout _ | Analyze _ | Move _ | Sort _ ->
    print_string general
  | Yes | No -> print_string "Not valid command"


(** Used in print_menu to display the commands that a player may type to play
    the game. *)
let print_available_commands =
  let general =
    "Commands (type help for more information):\
     \n     General: team, quit, help, scout, analyze, move"
  in function
    | Main -> ANSITerminal.(
        print_string [blue] (general ^ " [shop, arena, training]\
                                        \n     Location specific: nextday.\n"))
    | Shop -> ANSITerminal.(
        print_string [blue] (general ^ " [main, arena, training]\
                                        \n     Location specific: shop.\n"))
    | Arena -> ANSITerminal.(
        print_string [blue] (general ^ " [main, shop, training]\
                                        \n     Location specific: arena, \
                                        update.\n"))
    | Training_Field -> ANSITerminal.(
        print_string [blue] (general ^ " [main, shop, arena]\
                                        \n     Location specific: practice.\n"))

(** [print_help_command_helper array] prints the table of commands that are
    possible in the user's current location, which are determined by both the
    general help commands and the ones given in [array]. *)
let print_help_command_helper array =
  print_table help_headers (help_array @ array)
    ~msg: (Some "Here are the commands you can carry out \
                 in your current menu!");
  wait_for_enter ()


(* Defined in module *)
let print_help_command location =
  ignore(Sys.command "clear");
  match location with
  | Main -> print_help_command_helper help_array_main
  | Shop -> print_help_command_helper help_array_shop
  | Arena -> print_help_command_helper help_array_arena
  | Training_Field -> print_help_command_helper help_array_training


(* Defined in module *)
let print_basic_info adv state = 
  let team_name = get_team_name state in 
  let current_date = state |> get_current_date |> string_of_int in 
  let current_money = state |> get_current_money |> string_of_int in
  let name_str = List.fold_left (fun acc elt -> elt.player_name ^ ", " ^ acc) 
      "" (get_current_active_players state) in
  print_endline ("Day " ^ current_date ^ ":");
  print_endline ("Hello " ^ (get_user_name state) ^ ", manager of the "
                 ^ team_name ^ "!");
  print_endline ("You have " ^ current_money ^ " dollars.");
  if List.length (get_current_active_players state) = 0 
  then print_string "Nobody are ready to play! Go to the team menu to \
                     activate players on your team!\n"
  else 
    print_string ((String.sub name_str 0 (String.length name_str - 2)) 
                  ^ " are ready to play!\n")


(* Defined in module *)
let print_next_day_info state = 
  match get_current_event state with
  | None -> print_string "Today is nothing special!\n"
  | Some h -> print_red ((h.event_name) ^ " happens today!\n");
    print_string ("You are gonna spend " ^ (string_of_int h.event_cost) 
                  ^ " energy to participate, and get "
                  ^ (string_of_int h.event_reward) 
                  ^ " money as reward. Would you like to participate? \
                     [yes/no]\n")


(* Defined in module *)
let print_menu adv state =
  ignore(Sys.command "clear");
  print_stars ();
  print_endline ("current location: " ^
                 loc_to_string (get_current_location state));
  print_endline ("current energy: " ^
                 (get_current_energy state |> string_of_int) ^ " / 50\n");
  print_basic_info adv state;
  print_newline ();
  print_available_commands (get_current_location state);
  print_stars ()


(* Defined in module *)
let print_game result =
  ignore(Sys.command "clear");
  print_table game_result_headers (game_result_array result);
  begin
    if (List.nth result.opp_score 3 > List.nth result.our_score 3) then
      print_string "You Lose! You gain 100 dollars in pity money. \n" else
    if (List.nth result.opp_score 3 = List.nth result.our_score 3) then
      print_string "It's a Tie! You won 200 dollars in ad revenue and bets.\n"
    else print_string "You Win! You won 300 dollars from the won bets and \
                       happy fans. Congratulations!\n"
  end;
  wait_for_enter ()

(* defined in module *)
let ask_prompt st new_st =
  print_string "\nContinue?\n\n";
  print_string ("Energy Change: " ^ (st |> get_current_energy |> string_of_int)
                ^ " -> " ^ (new_st |> get_current_energy |> string_of_int)
                ^ "\n");
  if get_current_location st = Arena
  then 
    print_string ("Budget Change: " ^ (st |> get_current_money |> string_of_int)
                  ^ " -> ?\n")
  else
    print_string ("Budget Change: " ^ (st |> get_current_money |> string_of_int)
                  ^ " -> " ^ (new_st |> get_current_money |> string_of_int)
                  ^ "\n");
  wait_for_yes_no ()  


(* defined in module *)
let print_setup_b st name =
  print_string ("\nHello " ^ name ^ "! ");
  print_string ("You are the manager of the " ^ (get_team_name st) ^ ".\n");
  print_string "Start your adventure as a manager of an upcoming basketball \
                team by activating all of your players by \
                name in the team menu. ";
  print_string "You can do this by typing the '";
  print_blue "team";
  print_string "' command!\n";
  print_string "Learn the rest of the commands by typing the '";
  print_blue "help";
  print_string "' command at anytime!\n\n";

  print_string "You'll be starting out in the main menu, but you can move to \
                different menus to access different areas of your management \
                by typing the following commands:\n";
  print_blue "'move arena'";
  print_string " : Move to the arena, where you can challenge other teams \
                once a day. Gain your prestige here!\n";
  print_blue "'move shop'";
  print_string " : Move to the shop, where you can purchase and sell players \
                to and from the free market.\n";
  print_blue "'move training'";
  print_string " : Move to the training field, where you can train the \
                players on your team. Increase their stats to improve your \
                overall team's stats!\n\n";

  print_string "Your ultimate goal is to manage the best basketball team there \
                is, so make the most of every day by challenging other \
                teams in the arena and training your players!\n\
                Once you've used up all of your energy or are ready to update \
                your information, you can type the '";
  print_blue "nextday";
  print_string "' command to move on to the next day!\n\n\n";

  print_red "**IMPORTANT**\n";
  print_string ("Before pressing ENTER to begin your journey as the \
                 manager of the " ^ (get_team_name st) 
                ^ ", resize your screen so that the following\n\
                   line of characters appears in one line for the best \
                   game experience:\n\n");
  print_red "<--------------------------------------------------------------\
             ---------------------------------------------------------------\
             ------------------------------------------->\n\n";

  print_stars ();
  print_string "\n";
  wait_for_enter ()


(* defined in module *)
let print_setup_a () =
  ignore(Sys.command "clear");
  print_stars ();
  print_string "Welcome to Fantasy Sports Managers!\n";
  print_string "Enter your name: "


let print_quarter_prompt quarter sim =
  ignore(Sys.command "clear");
  print_table game_result_headers (sim |> last_result |> game_result_array);
  print_string (quarter_of_int quarter ^ ":\n");
  print_string ("Would you like to change your tatic?\n");
  wait_for_yes_no ()

(* Table printing functions
 ******************************************************************************)


(** Used in Scout *)
(* Defined in module *)
let rec print_single_player player = 
  ignore(Sys.command ("clear"));
  let headers = scout_player_headers in
  let array = (scout_player_array [player]) in
  print_table headers array;
  wait_for_enter ()


(** Used in Analyze *)
(* Defined in module *)
let rec print_single_team team = 
  ignore(Sys.command ("clear"));
  print_endline ("Here is the information about the " ^ team.team_name ^ "!");
  print_table team_headers (team_array [team]);
  let new_counter = count_stats_ability team.players in
  let msg = team.team_name ^ " is a " ^
            (match fst new_counter - snd new_counter with
             | n when n<0 -> "defensive-oriented team!\n"
             | n when n=0 -> "perfectly balanced team!\n"
             | n -> "offensive-oriented team!\n") in
  print_string msg;

  let player_headers = generic_player_headers in
  let player_array = generic_player_array team.players in
  print_table player_headers player_array;
  wait_for_enter ()


(* Printing functions for game loops
 ******************************************************************************)


(* Defined in module *)
let print_shop adv state =
  ignore(Sys.command ("clear"));
  let headers = generic_player_headers in
  let array = 
    generic_player_array (get_current_market state |> sort_player_list) in
  print_table headers array
    ~msg:(Some "Welcome to the Shop! Here is a list of all the free agents \
                available for purchase!");
  print_string ("Current Budget: " 
                ^ (state |> get_current_money |> string_of_int)
                ^ " dollars.\n\n");
  print_blue ("To buy someone, type 'buy [playername]' where [playername] is \
               the name of the player you want to buy.\n\
               To sell someone, type 'sell [playername]' where [playername] \
               is the name of the player you want to sell.\n\
               To sort the list by a column in ascending order, type 'sort \
               [column]' where [column] is the name of the header you want to \
               sort by.\n\n\
               To back out of the shop, type 'back'.\n")


(* TODO Implement *)
(* defined in module *)
let print_team adv state =
  ignore(Sys.command "clear");
  print_endline "Here is the information about your current team!\n";
  (* Print team lists *)
  let player_headers = user_player_headers in
  let team_headers = your_team_headers in
  let team_arr = your_team_array (get_current_sim state) 
      [your_active_team adv state] in
  let active_array = user_active_player_array
      (get_current_team state |> sort_player_list) in
  let rest_array = user_resting_player_array
      (get_current_team state |> sort_player_list) in
  print_table team_headers team_arr ~msg:(Some "Overall Stats:");
  print_newline (); print_newline ();
  print_table player_headers active_array ~msg: (Some "Active players:");
  print_table player_headers rest_array ~msg: (Some "Resting players:");
  print_blue ("To activate a player, type 'activate [player]' where \
               [player] is the name or id # of the player you want to set on \
               your active roster. \n\
               You can activate multiple players by their id #s spaced \
               apart.\n\n\
               To rest a player, type 'rest [player]' where \
               [player] is the name or id # of the player you want to remove \
               from your active roster.\n\
               Type 'rest everyone' to set all players to rest.\n\n\
               To sort the list by a column in ascending order, type 'sort \
               [column]' where [column] is the name of the header you want to \
               sort by.\n\n\
               To leave the team menu, type 'back'.\n")


(* Defined in module *)
let print_arena adv state =
  ignore(Sys.command ("clear"));
  let headers = team_headers in
  let your_array = team_array [your_active_team adv state] in
  let opp_array =
    get_current_opponent_teams state |> sort_team_list |> team_array in
  print_table headers opp_array
    ~msg:(Some "Welcome to the Arena! Here is a list of all the opponent teams \
                available for challenge!");

  begin if (state |> get_current_active_players |> List.length) < 5 then
      print_string "Note: you do not have enough active players to challenge a \
                    team!\n"
    else print_string "Your team is ready to play!\n" end;

  print_table headers your_array ~msg:(Some "Your team's stats:");
  print_blue ("To schedule a match against another team, type 'challenge \
               [teamname]' where [teamname] is the name of the team you want \
               to play against.\n\n\
               To analyze a team's stats, type 'analyze [teamname]', where \
               [teamname] is the name of the team you want to analyze.\n\n\
               To update your team's tactics, type 'update [tactic]', where \
               [tactic] is the new game tactic you would like to implement.\n\n\
               To sort the list by a column in ascending order, type 'sort \
               [column]' where [column] is the name of the header you want to \
               sort by.\n\n\
               To back out of the arena, type 'back'.\n")


(* defined in module *)
let print_train adv state =
  ignore(Sys.command ("clear"));
  let headers = train_player_headers in 
  let array = train_array (get_current_team state |> sort_player_list) in 
  print_table headers array
    ~msg:(Some "Welcome to the Training Field! Here is a list of the players \
                on your team!");
  print_string ("Current Budget: " 
                ^ (state |> get_current_money |> string_of_int)
                ^ " dollars.\n");
  print_string ("Current Energy: " 
                ^ (state |> get_current_energy |> string_of_int)
                ^ " / 50\n\n");
  print_blue ("To train someone, type 'train [playername]' where [playername] \
               is the name of the player you want to train.\n\n\
               To sort the list by a column in ascending order, type 'sort \
               [column]' where [column] is the name of the header you want to \
               sort by.\n\n\
               To exit the training field, type 'back'.\n")
