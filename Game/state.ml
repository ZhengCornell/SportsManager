open Game
open Command
open Sim

(* defined in module *)
type location = 
  | Main
  | Shop
  | Arena
  | Training_Field


(* defined in module *)
type team_name = string

(* defined in module *)
type player_name = string

(* defined in module *)
type t = {
  user_name: string;
  team: team_name;
  current_location: location;
  current_market: single_player list;
  current_rating: team_rating;
  current_money: int;
  current_date: int;
  current_players_in_team: single_player list;
  current_opponent_teams: single_team list;
  current_energy: int;
  sim_status: Sim.t;
  today_event: single_event option;
}

(* defined in module *)
type result = Legal of t | Illegal

(* defined in module *)
let get_user_name t =
  t.user_name

(* defined in module *)
let get_team_name t =
  t.team

(* defined in module *)
let get_current_location t =
  t.current_location

(* defined in module *)
let get_current_money t = 
  t.current_money

(* defined in module *)
let get_current_date t = 
  t.current_date

(* defined in module *)
let get_current_rating t =
  t.current_rating

(* defined in module *)
let get_current_players t = 
  t.current_players_in_team |> List.map (fun elt -> elt.player_name)

(* defined in module *)
let get_current_active_players t = 
  t.current_players_in_team |> List.filter (fun elt -> elt.status = Active)

(* defined in module *)
let get_current_team t =
  t.current_players_in_team

(* defined in module *)
let get_current_opponent_teams t = 
  t.current_opponent_teams 
  |> List.sort (fun t1 t2 -> t2.team_rating.overall - t1.team_rating.overall)

(* defined in module *)
let get_current_energy t =
  t.current_energy

(* defined in module *)
let get_current_market t =
  t.current_market |> List.sort (fun p1 p2 -> p1.price - p2.price)

(* defined in module *)
let get_current_sim t =
  t.sim_status

(** [build_t user_name team location market rating money date players progress] 
    creates a new state with the given parameters as the fields. *)
let build_t user_name team_name current_location current_market current_rating 
    current_money current_date current_players_in_team 
    current_opponent_teams current_energy sim_status today_event = 
  {
    user_name = user_name;
    team = team_name;
    current_location = current_location;
    current_market = current_market;
    current_rating = current_rating;
    current_money = current_money;
    current_date = current_date; 
    current_players_in_team = current_players_in_team;
    current_opponent_teams = current_opponent_teams;
    current_energy = current_energy;
    sim_status = sim_status;
    today_event = today_event
  }

(* defined in module *)
let init_state adv name =
  let my_team_info = 
    match get_single_team adv (get_your_team adv) with 
    | None -> failwith "impossible - state.ml init_state"
    | Some v -> v in
  let current_opponent_teams = 
    adv |> get_teams |> List.filter 
      (fun single_team -> my_team_info.team_name <> single_team.team_name) in
  {
    user_name = name;
    team = my_team_info.team_name;
    current_location = Main;
    current_market = get_market adv;
    current_rating = {attack=0; defense=0;
                      chemistry=my_team_info.team_rating.chemistry; overall=0};
    current_money = get_budget adv;
    current_date = get_date adv;
    current_players_in_team = my_team_info.players;
    current_opponent_teams = current_opponent_teams;
    current_energy = 50;
    sim_status = Sim.init_sim my_team_info.tactic;
    today_event = None
  }

(* defined in module *)
let next_day adv st =  
  match get_single_event adv (st.current_date + 1) with
  | None -> Legal (build_t st.user_name st.team st.current_location 
                     st.current_market st.current_rating 
                     (st.current_money + 100) (st.current_date + 1) 
                     st.current_players_in_team 
                     st.current_opponent_teams 50 st.sim_status None)
  | Some h -> Legal (build_t st.user_name st.team st.current_location 
                       st.current_market st.current_rating 
                       (st.current_money + 100) (st.current_date + 1) 
                       st.current_players_in_team 
                       st.current_opponent_teams 50 st.sim_status (Some h))

(** [player_of_string str lst] is the player option given by the identifier
    [str]. [str] can be either a valid player id, or player name. Returns
    [Some player] if sre is a valid name or id, or [None] if str is neither a
    valid name nor id. *)
let player_of_string str player_lst =
  if (Str.string_match (Str.regexp "[0-9]+$") str 0)
  then check_player_list (int_of_string str) player_lst
  else check_player_list' str player_lst

(* defined in module *)
let scout st player_name =
  let player_list = st.current_market @ st.current_players_in_team in
  if (Str.string_match (Str.regexp "[0-9]+$") player_name 0)
  then check_player_list (int_of_string player_name) player_list
  else check_player_list' player_name player_list

(* defined in module *)
let analysis st adv team_name =
  if (team_name = st.team) then 
    match List.find_opt (fun h -> h.team_name = team_name) (get_teams adv) with
    | None -> None
    | Some team -> 
      (Some {team with team_rating=st.current_rating;
                       players= List.filter (fun p -> p.status=Active)
                           st.current_players_in_team})
  else 
    List.find_opt (fun h -> h.team_name = team_name) (st.current_opponent_teams)

(** [remove player lst] is the list of players after having removed [player]
    from [lst]. *)
let remove (player_info : single_player) (lst : single_player list) = 
  List.filter (fun elt -> elt.player_id <> player_info.player_id) lst

(* defined in module *)
let sell st player_name = 
  match player_of_string player_name st.current_players_in_team with
  | None -> Illegal
  | Some player -> 
    (* remove [player] from your team *)
    let current_new_players_in_team = 
      remove player st.current_players_in_team in 
    (* update [player]'s information *)
    let new_player_info = {player with current_team = "";
                                       status = Rest} in
    (* add [player] to the market *)
    let current_market = new_player_info :: st.current_market in 
    (* add [player]'s price to your money balance *)
    let current_money = st.current_money + player.price in 
    (* update the game state *)
    Legal (build_t st.user_name st.team st.current_location current_market 
             st.current_rating current_money st.current_date 
             current_new_players_in_team st.current_opponent_teams 
             st.current_energy st.sim_status st.today_event)

(* defined in module *)
let buy st player_name = 
  match player_of_string player_name st.current_market with
  | None -> Illegal
  | Some player -> 
    (* update [player]'s information *)
    let new_player_info = {player with current_team = st.team;
                                       status = Rest} in
    (* add [player] to your team *)
    let current_new_players_in_team = 
      new_player_info :: st.current_players_in_team in 
    (* remove [player] from the market *)
    let current_market = remove new_player_info st.current_market in 
    (* deduct [player]'s cost from your money balance *)
    let current_money = st.current_money - player.price in 
    (* if your money is now negative, then don't allow the purchase *)
    if current_money < 0 then Illegal else 
      Legal (build_t st.user_name st.team st.current_location current_market 
               st.current_rating current_money st.current_date 
               current_new_players_in_team st.current_opponent_teams 
               st.current_energy st.sim_status st.today_event)

(* defined in module *)
let move t location =
  match String.concat "" location with
  | "main" -> Legal(build_t t.user_name t.team Main t.current_market 
                      t.current_rating t.current_money t.current_date 
                      t.current_players_in_team t.current_opponent_teams 
                      t.current_energy t.sim_status t.today_event)
  | "shop" -> Legal(build_t t.user_name t.team Shop t.current_market 
                      t.current_rating t.current_money t.current_date 
                      t.current_players_in_team t.current_opponent_teams 
                      t.current_energy t.sim_status t.today_event)
  | "arena" -> Legal(build_t t.user_name t.team Arena t.current_market 
                       t.current_rating t.current_money t.current_date 
                       t.current_players_in_team t.current_opponent_teams 
                       t.current_energy t.sim_status t.today_event)
  | "training" -> Legal(build_t t.user_name t.team Training_Field 
                          t.current_market t.current_rating t.current_money 
                          t.current_date t.current_players_in_team 
                          t.current_opponent_teams t.current_energy 
                          t.sim_status t.today_event)
  | _ -> Illegal

(** [replace player player_lst] is the list of players which [player] replace 
    the object with the same name in [player_lst] *)
let rec replace 
    (player : Game.single_player) 
    (player_lst : Game.single_player list) = 
  match player_lst with 
  | [] -> player_lst
  | h :: t -> if h.player_name = player.player_name then player :: t
    else h :: (replace player t)

(** [check_if_activate_allowed player_lst] is whether [player_lst] has 5 
    activated players or not.  *)
let check_if_activate_allowed (player_lst : Game.single_player list) = 
  let count = List.fold_left 
      (fun acc elt -> if elt.status = Active then acc + 1 else acc) 
      0 player_lst in 
  count < 5

(* defined in module *)
let count_stats_num (player_lst : Game.single_player list) = 
  List.fold_left (fun acc p -> 
      if p.position = "defender"  then 
        let def_num = snd acc + 1 in (fst acc, def_num) 
      else if p.position = "forward" then 
        let att_num = fst acc + 1 in (att_num, snd acc)
      else acc
    ) (0, 0) player_lst

(* defined in module *)
let count_stats_ability (player_lst : Game.single_player list) = 
  List.fold_left (fun acc p -> 
      if p.position = "defender" then 
        let def_idx = snd acc + p.rating.overall in (fst acc, def_idx) 
      else if p.position = "forward" then 
        let att_idx = fst acc + p.rating.overall in (att_idx, snd acc)
      else acc
    ) (0, 0) player_lst

(** [updated_attack_rating acc p_lst] is the updated offense team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_attack_rating acc num_players = function 
  |[] -> acc/num_players
  |h::t -> let off_avg = (h.rating.shooting + h.rating.dribbling 
                          + h.rating.passing + h.rating.dribbling) / 4 in
    (updated_attack_rating (acc + off_avg) (num_players+1) t)

(** [updated_defense_rating acc p_lst] is the updated defense team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_defense_rating acc num_players= function
  | [] -> acc/num_players
  | h::t -> let def_avg = (h.rating.defending + h.rating.physicality) / 2 in
    (updated_defense_rating (acc + def_avg) (num_players+1) t)

(** [updated_overall_rating acc p_lst] is the updated overall team rating of
    all the players in [p_lst] after having trained a player. Tail-recursive. *)
let rec updated_overall_rating acc num_players = function
  | [] -> acc/num_players
  | h::t -> (updated_overall_rating (acc+ h.rating.overall) (num_players+1) t)

(** [updated_team_rating lst] is [x], where [x] represents the team rating
    based off of the players in [lst]. *)
let updated_team_rating new_team chemistry = 
  {chemistry = chemistry;
   attack = updated_attack_rating 0 0 new_team;
   defense = updated_defense_rating 0 0 new_team;
   overall = updated_overall_rating 0 0 new_team}

(** [incr_rating rating position] is the updated rating of a player after 
    having been trained. Based on [position], [rating] will differ, but the
    overall player rating increases by 2.
    Example: If [position] is "Center", [rating] is increased in the following
    manner: Pace increases by 1, Shooting increases by 2, Passing increases by
    2, Dribbling increases by 1, Defending increases by 3, and Physicality
    increases by 3. *)
let incr_rating rating = function
  | "Power Forward" -> (* defending || passing *)
    {pace = rating.pace+2; shooting = rating.shooting+2; 
     passing = rating.passing+1; dribbling = rating.dribbling+2; 
     defending = rating.defending+3; physicality = rating.physicality+2; 
     overall = rating.overall+2}
  | "Small Forward" -> (* pace + dribbling || physicality + defending *)
    {pace = rating.pace+3; shooting = rating.shooting+2; 
     passing = rating.passing+2; dribbling = rating.dribbling+3; 
     defending = rating.defending+1; physicality = rating.physicality+1; 
     overall = rating.overall+2}
  | "Shooting Guard" -> (* shooting + passing || defending + pace *)
    {pace = rating.pace+1; shooting = rating.shooting+3; 
     passing = rating.passing+3; dribbling = rating.dribbling+2; 
     defending = rating.defending+1; physicality = rating.physicality+2; 
     overall = rating.overall+2}
  | "Point Guard" -> (* shooting + passing || dribbling + defending *)
    {pace = rating.pace+2; shooting = rating.shooting+3; 
     passing = rating.passing+3; dribbling = rating.dribbling+1; 
     defending = rating.defending+1; physicality = rating.physicality+2; 
     overall = rating.overall+2}
  | "Center" -> (* physicality + defending || pace + dribbling *)
    {pace = rating.pace+1; shooting = rating.shooting+2; 
     passing = rating.passing+2; dribbling = rating.dribbling+1; 
     defending = rating.defending+3; physicality = rating.physicality+3; 
     overall = rating.overall+2}
  | _ -> failwith "impossible, not a valid position to increase rating with"

(** [train_helper t player cost] is [r] when attempting to train [player]. 
    If [cost] does not exceed the user's current energy level given in game 
    state [t], and if the monetary cost of training [player] does not exceed
    the user's current money given in game state [t], then [r] is [Legal t'],
    where in [t'], the user now has a deducted energy level and balance, and
    [player] has its upgrade level and rating increased. Otherwise, [r] is
    [Illegal]. *)
let train_helper (t:t) (player:Game.single_player) (energy_cost:int) =
  (* deduct energy cost *)
  let new_energy = t.current_energy - energy_cost in
  (* deduct money cost *)
  let new_money = t.current_money - (3 * player.rating.overall) in
  (* if energy is negative as a result, stop *)
  if new_energy < 0 then Illegal 
  (* if money is negative as a result, stop *)
  else if new_money < 0 then Illegal 
  else
    (* update [player]'s information *)
    let new_player = 
      {player with level = player.level + 1; 
                   rating = incr_rating player.rating player.position} in
    (* exchange this updated information with the old information *)
    let new_team = replace new_player t.current_players_in_team in 
    (* update the team's rating *)
    let new_rating = updated_team_rating new_team t.current_rating.chemistry in
    Legal(build_t t.user_name t.team t.current_location t.current_market 
            new_rating new_money t.current_date new_team 
            t.current_opponent_teams new_energy t.sim_status t.today_event)

(* defined in module *)
let train t player =
  match player_of_string player t.current_players_in_team with 
  | None -> Illegal (* checks if [player] is on the user's team *)
  | Some h ->
    match h.level with
    | num when num < 0 -> Illegal
    | num when num > 4 -> Illegal 
    | num -> train_helper t h (num+6)

(* defined in module *)
let active st player = 
  match player_of_string player st.current_players_in_team with
  | None -> Illegal
  | Some h when h.status = Active -> Illegal
  | Some player ->
    match check_if_activate_allowed st.current_players_in_team with
    | false -> Illegal
    | true -> 
      (* update [player]'s information *)
      let new_player_info = {player with status = Active} in
      (* exchange this updated information with the old information *)
      let current_new_players_in_team = 
        replace new_player_info st.current_players_in_team in
      let updated_rating = updated_team_rating
          (List.filter (fun p -> p.status = Active) current_new_players_in_team)
          st.current_rating.chemistry in
      Legal {st with current_players_in_team = current_new_players_in_team;
                     current_rating = updated_rating}

(* defined in module *)
let rest st player_name = match player_name with
  | "everyone" -> 
    let rested_lst = List.map (fun player -> {player with status = Rest})
        st.current_players_in_team in
    let updated_rating = {attack=0; defense=0; overall = 0;
                          chemistry=st.current_rating.chemistry} in
    Legal {st with current_players_in_team = rested_lst;
                   current_rating = updated_rating}
  | _ -> begin
      match player_of_string player_name st.current_players_in_team with
      | None -> Illegal
      | Some h when h.status = Rest -> Illegal
      | Some player ->
        (* update [player]'s information *)
        let new_player_info = {player with status = Rest} in
        (* exchange this updated information with the old information *)
        let current_new_players_in_team = 
          replace new_player_info st.current_players_in_team in 
        let updated_rating = updated_team_rating
            (List.filter (fun p -> p.status = Active) 
               current_new_players_in_team)
            st.current_rating.chemistry in
        Legal {st with current_players_in_team = current_new_players_in_team;
                       current_rating = updated_rating}
    end

(* defined in module *)
let challenge_quarter t (team_name : Game.team_name) sim quarter =
  let team = List.find (fun t -> t.team_name=team_name)
      t.current_opponent_teams in
  let sim_t = Sim.sim_quarter sim t.current_rating
      team.team_rating team.tactic quarter in
  {t with sim_status = sim_t}

(* defined in module *)
let update_challenge_money t =
  match previous_outcome t.sim_status with
  | Win -> {t with current_money = t.current_money + 300}
  | Tie -> {t with current_money = t.current_money + 200}
  | Loss -> {t with current_money = t.current_money + 100}
  | Void -> failwith "impossible"


(** [challenge_energy_check t team] checks if the user is able to 
    challenge [team] based on their energy level in game state [t]. *)
let challenge_energy_check t team = 
  (* deduct energy cost *)
  let new_energy = t.current_energy - 30 in 
  if new_energy < 0 then Illegal (* if energy is negative as a result, stop *)
  else
    let new_sim = reset_game_result t.sim_status
    in Legal {t with current_energy = new_energy; sim_status = new_sim}

(* defined in module *)
let challenge t (team: Game.team_name) = 
  match List.find_opt (fun t -> t.team_name=team) t.current_opponent_teams with 
  | None -> Illegal
  | Some h -> 
    if check_if_activate_allowed (t.current_players_in_team) then Illegal
    else challenge_energy_check t h

(* defined in module *)
let update t (tactic_name: string) = 
  let tactic = Game.get_tactic tactic_name in 
  match tactic with 
  | UltraDefensive 
  | Defensive
  | Balanced
  | Attacking
  | UltraAttacking -> 
    Legal {t with sim_status = Sim.update_tactic t.sim_status tactic}
  | UnknownTactic -> Illegal

(* defined in module *)
let get_current_shop t = 
  t.current_market |> List.map (fun elt -> elt.player_name)

(* defined in module *)
let get_current_event t = 
  t.today_event

(* defined in module *)
let yes st (event : Game.single_event) = 
  if st.current_energy > 30 then 
    let current_money = st.current_money + event.event_reward in 
    Legal {st with current_money = current_money; 
                   current_energy = st.current_energy - 30;
                   today_event = None}
  else Illegal

(* defined in module *)
let no st (event : Game.single_event) = 
  Legal {st with current_money = st.current_money; 
                 current_energy = st.current_energy;
                 today_event = None}