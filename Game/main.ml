open Command
open State
open Game
open Printer
open Sim

exception NotLegal

(* Helper Functions
 ******************************************************************************)


let checkLegal = function 
  | Illegal -> raise NotLegal
  | Legal h -> h

let quit () = ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0

let empty () = print_red "Please enter a command.\n"

let malformed () = print_red "Not a valid command.\n"

let yesno () = print_red "There is no event today anymore.\n"

(* Helper functions that don't change the state *)
let analysis_helper team adv st =
  let team_name = (team |> String.concat " ") in
  match analysis st adv team_name with 
  | None ->
    print_red ("Sorry, there is no such team!\nIf you want to analyze your \
                own team, type 'team'.\n"); ()
  | Some h -> print_single_team h; ()

let scout_helper player st =
  let player_name = (player |> String.concat " ") in
  match scout st player_name with 
  | None -> print_string ("Sorry, there is no such player!\n"); ()
  | Some p -> 
    print_string ("Here is the information about " ^ player_name ^ ":\n");
    print_single_player p; ()


(* Helper functions that change the sorting *)
let sort_player_helper header = try begin
  String.concat " " header |> parse_player_sort |> set_player_sort;
  print_red ("Sorted by " ^ (String.concat " " header ^ ".\n"))
end with
| Malformed -> print_red "Not a valid header.\n"

let sort_team_helper header = try begin
  String.concat " " header |> parse_team_sort |> set_team_sort;
  print_red ("Sorted by " ^ (String.concat " " header ^ ".\n"))
end with
| Malformed -> print_red "Not a valid header.\n"


(* Helper functions that DO change the state *)
let buy_helper player state =
  let player_name = (player |> String.concat " ") in 
  match checkLegal (buy state player_name) with 
  | exception NotLegal ->
    print_red ("You cannot buy " ^ player_name ^ ".\n"); state
  | new_st -> if (ask_prompt state new_st) then 
      (print_red ("You bought " ^ (player |> String.concat " ") ^ "\n"); new_st)
    else state

let sell_helper player state =
  let player_name = (player |> String.concat " ") in 
  match checkLegal (sell state player_name) with 
  | exception NotLegal ->
    print_red ("You cannot sell " ^ player_name ^ ".\n"); state
  | new_st ->  if (ask_prompt state new_st) then 
      (print_red ("You sold " ^ (player |> String.concat " ") ^ ".\n"); new_st)
    else state

let rec read_new_tatic prev_tatic =
  print_string "\n> ";
  match (read_line ()) with
  | "back" -> prev_tatic
  | str -> begin
      match get_tactic str with
      | UnknownTactic -> print_string "Not a valid tactic.\n";
        read_new_tatic prev_tatic
      | UltraDefensive | Defensive | Balanced | Attacking | UltraAttacking as t
        -> t
    end

let challenge_helper team state =
  let team_name = (String.concat " " team) in 
  match challenge state team_name with
  | Illegal ->
    print_red ("Sorry, either there is no such team, you don't have \
                enough energy, or you have less than five players \
                activated!\n"); state
  | Legal st -> if (ask_prompt state st) then
      let rec full_quarters st opp_team sim quarter = 
        if quarter > 4 then (st |> update_challenge_money) else
          let t = challenge_quarter st opp_team sim quarter in
          let new_tatic = if (quarter < 4 && print_quarter_prompt quarter
                                (get_current_sim t))
            then (print_string "\n\nPlase select a new tactic. Your options are:\
                        \nultra_defensive, defensive, balanced, attacking, and \
                        ultra_attacking.\nTo continue without changing your \
                        tactic, type 'back'.\n\n";
                  read_new_tatic (t |> get_current_sim |> get_sim_tactic))
            else t |> get_current_sim |> get_sim_tactic in
          full_quarters t opp_team (update_tactic (get_current_sim t) new_tatic)
            (quarter+1)
      in
      let new_st = full_quarters st team_name (st |> get_current_sim) 1 in
      (new_st |> get_current_sim |> last_result |> print_game; new_st)
    else state

let update_helper tactic state =
  let tactic_name = (String.concat " " tactic) in 
  match update state tactic_name with
  | Illegal ->
    print_red "Sorry, there is no such tactic! You can try ultra_defensive, \
               defensive, balanced, attacking, and ultra_attacking.\n"; state
  | Legal new_st -> 
    print_blue (tactic_name ^ " is now updated. " ^ tactic_name 
                ^ " is your new tactic.\n"); new_st

let rec activate_helper player state =
  let player_name = (String.concat " " player) in
  if (Str.string_match (Str.regexp "[0-9]+[ ]+[0-9]") player_name 0) then
    let rec multi_id st = function
      | [] -> st
      | h::t ->
        let new_st = activate_helper [h] st in
        multi_id new_st t in
    multi_id state player
  else
    match active state player_name with
    | Illegal -> (* TODO Fix this error message *)
      print_red "Sorry, there is no such player in your team or \
                 you cannot activate an active player or \
                 you cannot activate more than 5 players!\n"; state
    | Legal new_st -> 
      print_blue ("Activation successful. " 
                  ^ player_name ^ " is now activated.\n");
      new_st

let rest_helper player state = 
  let player_name = (String.concat " " player) in 
  match rest state player_name with
  | Illegal ->
    print_red "Sorry, there is no such player in your team!\n"; state
  | Legal new_st ->
    print_blue ("Deactivation successful. " 
                ^ player_name ^ " is now resting.\n");
    new_st

let train_helper player state =
  let player_name = (String.concat " " player) in 
  match checkLegal (train state player_name) with
  | exception NotLegal -> print_red "You cannot train this player.\n"; state
  | new_st -> if (ask_prompt state new_st) then 
      (print_training_graphic ();
       print_blue ("You trained " ^ player_name ^ ".\n"); new_st)
    else state



(** [shop_loop adv state] is the game loop on the shop screen. Reached by
    using commands [move shop] and [shop] from the main screen. Exited by typing
    "back". *)
let rec shop_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> reset_sort (); print_menu adv state; loop adv state
  | str -> 
    match parse str with
    | Quit -> quit ()
    | exception Empty -> print_red "Please enter a command.\n"; 
      shop_loop adv state
    | exception Malformed -> print_red "Not a valid command.\n";
      shop_loop adv state
    | Buy player ->
      let st = buy_helper player state in print_shop adv st; shop_loop adv st
    | Sell player ->
      let st = sell_helper player state in print_shop adv st; shop_loop adv st
    | Sort header -> 
      sort_player_helper header; print_shop adv state; shop_loop adv state
    | _ -> print_red "You cannot do that here.\n"; shop_loop adv state

(** [arena_loop adv state] is the game loop on the arena screen. Reached by
    using commands [move arena] and [arena] from the main screen. Exited by
    typing "back". *)
and arena_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> reset_sort (); print_menu adv state; loop adv state
  | str ->
    match parse str with
    | Quit -> quit ()
    | exception Empty -> empty (); arena_loop adv state
    | exception Malformed -> malformed (); arena_loop adv state
    | Challenge team ->
      let st = challenge_helper team state in
      print_arena adv st; arena_loop adv st
    | Analyze team ->
      analysis_helper team adv state; 
      print_menu adv state; arena_loop adv state
    | Update tactic -> 
      let st = update_helper tactic state in 
      print_team adv st; arena_loop adv st
    | Sort header ->
      sort_team_helper header; print_arena adv state; arena_loop adv state
    | _ -> print_red "You cannot do that here.\n"; arena_loop adv state

(** [team_loop adv state] is the game loop on the team screen. Reached by
    using the [team] command from any screen. Exited by typing "back". *)
and team_loop adv state =
  print_string "\n> ";
  match read_line () with
  | "back" -> reset_sort (); print_menu adv state; loop adv state
  | str ->
    match parse str with 
    | Quit -> quit ()
    | exception Empty -> empty (); 
      print_team adv state; team_loop adv state
    | exception Malformed -> malformed (); 
      print_team adv state; team_loop adv state
    | Scout player -> scout_helper player state; print_team adv state;
      team_loop adv state
    | Activate player ->
      let st = activate_helper player state in 
      print_team adv st; team_loop adv st
    | Rest player ->
      let st = rest_helper player state in 
      print_team adv st; team_loop adv st
    | Sort header ->
      sort_player_helper header; print_team adv state; team_loop adv state
    | Update tactic ->
      let st = update_helper tactic state in 
      print_team adv st; team_loop adv st
    | _ -> print_string "You cannot do that here.\n"; team_loop adv state

(** [train_loop adv state] is the training field loop on the team screen. 
    Reached by using commands [move training] and [practice] from the main 
    screen. Exited by typing "back". *)
and train_loop adv state = 
  print_string "\n> ";
  match read_line () with
  | "back" -> reset_sort (); print_menu adv state; loop adv state
  | str ->
    match parse str with
    | Quit -> quit ()
    | exception Empty -> empty (); print_train adv state;
      train_loop adv state
    | exception Malformed -> malformed (); print_train adv state;
      train_loop adv state
    | Train player -> 
      let st = train_helper player state in 
      print_train adv st; train_loop adv st
    | Sort header ->
      sort_team_helper header; print_train adv state; train_loop adv state
    | _ -> print_red "You cannot do that here.\n"; train_loop adv state


(** The game loop on the main screen. *)
and loop adv state = 
  print_string "\n> ";
  match read_line () with
  | str -> 
    match parse str with 
    | Quit -> quit ()
    | exception Empty -> empty (); loop adv state
    | exception Malformed -> malformed (); loop adv state
    | Next_day -> begin
        match get_current_location state with
        | Main ->
          let new_st = checkLegal (next_day adv state) in 
          print_next_day_info new_st; 
          print_menu adv new_st; loop adv new_st
        | _ -> print_cmd_not_possible Next_day; loop adv state
      end
    | Challenge team -> begin
        match get_current_location state with 
        | Arena -> print_string "To schedule a match, enter the arena by using \
                                 the 'arena' command.\n"; loop adv state
        | _ -> print_cmd_not_possible (Challenge team); loop adv state
      end
    | Team -> print_team adv state; team_loop adv state
    | Arena -> begin
        match get_current_location state with 
        | Arena -> (print_arena adv state; arena_loop adv state)
        | _ -> print_cmd_not_possible (Arena); loop adv state
      end
    | Shop -> begin
        match get_current_location state with 
        | Shop -> (print_shop adv state; shop_loop adv state)
        | _ -> print_cmd_not_possible (Shop); loop adv state
      end
    | Buy player | Sell player as cmd -> (match get_current_location state with
        | Shop -> 
          print_string ("To " ^ (cmd_to_string cmd) 
                        ^ "a player, enter the shop by using the 'shop' \
                           command.\n"); loop adv state
        | _ -> print_cmd_not_possible cmd; loop adv state)
    | Activate _ | Rest _ ->
      print_string "To manage your team, try the 'team' command first.\n";
      loop adv state
    | Scout player -> scout_helper player state;
      print_menu adv state; loop adv state
    | Analyze team -> analysis_helper team adv state;
      print_menu adv state; loop adv state
    | Move location -> begin
        let location_string = String.concat " " location in
        match move state location with
        | Illegal -> 
          print_string ("Sorry, that is not a valid location! \
                         Try 'move main' to go to the home menu, \
                         'move shop' to go to the shop menu, \n\
                         'move arena' to go to the arena menu, \
                         and 'move training' to go to the training menu.\n");
          loop adv state
        | Legal(new_state) -> 
          print_string ("Moved to " ^ location_string ^ ".\n"); 
          print_menu adv new_state; loop adv new_state
      end
    | Help -> 
      print_help_command (get_current_location state); 
      print_menu adv state; loop adv state
    | Practice -> begin
        match get_current_location state with 
        | Training_Field -> 
          print_train adv state; train_loop adv state
        | _ -> 
          print_string "Command not possible in current location.\n \
                        Try 'move training' before doing the 'practice' \
                        command.\n"; loop adv state
      end
    | Train _ -> begin
        match get_current_location state with
        | Training_Field -> 
          print_string "To train a player, enter the training field by using \
                        the 'practice' command.\n"; train_loop adv state
        | _ -> 
          print_string "Command not possible in current location.\n Try 'move \
                        training' before doing the 'train' command.\n";
          loop adv state
      end
    | Sort _ -> 
      print_string "You cannot do that here."; 
      print_menu adv state; loop adv state
    | Update tactic ->
      let st = update_helper tactic state in print_team adv st; team_loop adv st
    | Yes -> 
      begin
        match get_current_event state with 
        | None -> yesno (); loop adv state 
        | Some h -> 
          match yes state h with
          | Illegal -> print_string ("Sorry, you don't have enough energy to \
                                      participate "^ h.event_name^"\n"); 
            loop adv state
          | Legal new_st -> print_string ("Welcome to the "^ h.event_name^
                                          ". You spend "^
                                          (string_of_int h.event_cost)^
                                          " energy to acquire "^(
                                            string_of_int h.event_reward)
                                          ^" dollars!\n" ^ "Have fun!\n");   
            loop adv new_st 
      end
    | No -> 
      begin
        match get_current_event state with 
        | None -> yesno (); loop adv state
        | Some h -> 
          match no state h with 
          | Illegal -> failwith "impossible - main.ml loop No"
          | Legal new_st -> print_string ("Sorry to hear that you don't \
                                           want to participate "^ h.event_name^
                                          ". It should be fun! :(\n"); 
            loop adv new_st
      end

(** [setup adv] prints the introductory information necessary for the player to
    start playing the game. *)
let setup adv = 
  print_setup_a ();
  let name = read_line () in 
  let state = init_state adv name in
  print_setup_b state name;
  print_menu adv state;
  loop adv state

let rec play_game ()=
  let file = read_line () in
  if (file = "quit")
  then begin ignore(Sys.command "clear"); print_string "Bye!\n"; exit 0 end 
  else match Yojson.Basic.from_file file with
    | exception End_of_file -> ()
    | exception _ ->
      print_string "Please enter a valid file name.\n> "; play_game ()
    | js -> begin
        ignore(Sys.command "clear");
        print_red ("File succesfully loaded. You are now playing with " ^
                   file ^ ".\n");
        flush stdout;
        let adv = from_json js in
        Unix.sleep 1;
        setup adv
      end

let main () =
  print_title_screen ();
  play_game ()

(* Execute the game engine. *)
let () = main ()