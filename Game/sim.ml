open Random
open Game

(** This type represents the form of a team. It changes based on results *)
type form = 
  | VeryWeak
  | Weak
  | Normal
  | Strong
  | VeryStrong

(** This type represents the teams progress in the game so far. This holds
    the statistics of the performance so far. *)
type progress = {games_played: int; wins:int; losses:int; draws:int}

(* defined in module *)
type outcome = 
  | Win | Tie | Loss | Void

(* defined in module *)
type game_result = {
  our_score:int list;
  opp_score:int list;
}

(* defined in module *)
type t = {current_form:form; tactic:Game.tactic; current_record: progress;
          last_5_games: outcome list; last_result:game_result}

(* defined in module *)
let get_wins t = t.current_record.wins

(* defined in module *)
let get_draws t = t.current_record.draws

(* defined in module *)
let get_losses t = t.current_record.losses

(* defined in module *)
let get_sim_tactic t = t.tactic

(* defined in module *)
let init_sim (tactic:Game.tactic)  = 
  {current_form=Normal; tactic=tactic; 
   current_record={games_played=0; wins=0; losses=0; draws=0}; 
   last_5_games = []; last_result = {our_score=[]; opp_score=[]}}

(* defined in module *)
let last_result sim_t = sim_t.last_result

(* defined in module *)
let previous_outcome sim_t = 
  match sim_t.last_5_games with 
  | [] -> Void
  | h::_ -> h

(* defined in module *)
let update_tactic (sim_t:t) (tactic:Game.tactic) = 
  {sim_t with tactic=tactic}

(** [get_form_score form] returns a numberical value for the form of a team, 
    which is used as a factor to create dynamic ratings during a match 
    simulation *)
let get_form_score = function
  | VeryWeak -> -6
  | Weak -> -4
  | Normal -> 0
  | Strong -> 4
  | VeryStrong -> 6

(** [get_random_form seed] returns a random form for the team we are facing to 
    be used in simulation during games.*)
let gen_random_form = function
  | a when a > -1 && a < 4 -> VeryWeak
  | a when a >= 4 && a < 10 -> Weak
  | a when  a >= 10 && a < 19 -> Normal
  | a when  a >= 18 && a < 25 -> Strong
  | a  -> VeryStrong

(** [get_tactic_number tactic] returns a numerical mapping to a tactic for 
    purposes of comparision during simulation.  *)
let get_tactic_number = function
  | UltraAttacking -> 5
  | Attacking -> 4
  | Balanced -> 3
  | Defensive -> 2
  | UltraDefensive -> 1
  | UnknownTactic -> raise (Failure "This should not be happening")

(**[compute_points lower upper acc] generates random scores for the 4 quarters
    within the bounds given and returns the scores after each quarter as a list.
    For example, compute_points 5 15 [] can return [6;16;26;37]  *)
let rec compute_match_points lower upper acc = 
  match acc with
  | a::b::c::d::[] -> [a; a+b; a+b+c; a+b+c+d]
  | [] -> 
    compute_match_points lower upper ((lower + Random.int (upper - lower))::[])
  | h -> 
    compute_match_points lower upper (( lower + Random.int (upper - lower))::h)

(**[compute_quarter_points lower upper acc] computes the points for one quarter 
   of the game, instead of the whole game. *)
let compute_quarter_points lower upper acc = 
  (lower + Random.int (upper - lower))::acc

(**[calculate_points form_difference scoring_rating defending_rating 
   scoring_tactic defending_tactic] probabilistically computes the number of 
   points that theattacking team, with rating [scoring_rating] and tactics
   [scoring_tactics] will score against the defending 
   team with [defending_rating] and [defending_tactics].   *)
let calculate_points form_difference scoring_rating defending_rating
    scoring_tactic defending_tactic f = 
  begin
    match scoring_tactic with
    (* Using the tactic_numbers defined in get_tactic_number. It allows us to 
       treat tactics as numbers that can be used in calculations. *)
    (* The lower and upper limits take into account the attacking strength
       of the attacking team, the defending strength of the defending team,
       the difference in the team's forms (polar opposite forms will result in 
       wild changes), and the tactics deployed by both teams. These limits are
       normalized with some bounds to give realistic scorelines.)
    *)
    | tact when tact > 3 && tact - defending_tactic < 2 -> 
      let lower = (scoring_rating.attack - 64) 
                  - (defending_rating.defense-76) 
                  + form_difference 
                  + (3*scoring_tactic)/(3+ scoring_tactic - defending_tactic) 
                  |> max 15 in
      let upper = 
        max (27 + (scoring_rating.attack +scoring_tactic - 80)) (lower+7) in 
      f lower (min 31 upper) []
    | tact when tact > 3 && tact - defending_tactic >= 2 ->
      let lower = (scoring_rating.attack - 69) 
                  - (defending_rating.defense-73) 
                  + form_difference 
                  + (3*scoring_tactic)/(3+ scoring_tactic - defending_tactic) 
                  |> max 13 in
      let upper = 
        max (25 + (scoring_rating.attack + scoring_tactic - 81)) (lower + 6) in
      f lower (min 26 upper) []
    | tact when tact = 3 && tact - defending_tactic > 0 ->
      let lower = (scoring_rating.attack - 72) 
                  - (defending_rating.defense-75) 
                  + form_difference 
                  + (3*scoring_tactic)/(3+ scoring_tactic - defending_tactic) 
                  |> max 12 in
      let upper = 
        max (23 + (scoring_rating.attack + scoring_tactic - 83)) (lower + 6) in 
      f lower (min 23 upper) []
    | tact when tact = 3 && tact - defending_tactic <= 0 ->
      let lower = (scoring_rating.attack - 70) 
                  - (defending_rating.defense-76) 
                  + form_difference 
                  + (3*scoring_tactic)/(3+ scoring_tactic - defending_tactic) 
                  |> max 13 in
      let upper = 
        max (23 + (scoring_rating.attack + scoring_tactic - 82)) (lower + 6) in 
      f lower (min 24 upper) []
    | tact when tact < 3 && defending_tactic - tact < 2 ->
      let lower = (scoring_rating.attack - 74) 
                  - (defending_rating.defense - 76) 
                  + form_difference 
                  + (3*scoring_tactic)/(3+ defending_tactic - scoring_tactic) 
                  |> max 12 in
      let upper = 
        max(23 + (scoring_rating.attack + scoring_tactic - 82)) (lower + 6) in 
      f lower (min 20 upper) []
    | tact when tact < 3 && defending_tactic - tact >= 2 ->
      let lower = (scoring_rating.attack - 72) 
                  - (defending_rating.defense - 76) 
                  + form_difference 
                  +  (3*scoring_tactic)/(3+defending_tactic - scoring_tactic) 
                  |> max 12 in
      let upper = 
        max (22 + (scoring_rating.attack + scoring_tactic - 81)) (lower + 6) in 
      f lower (min 20 upper) []
    | _ -> f 15 24 []  
  end

(** [update_last_games lst points] returns the updated list of points gained by
    the user's team in the last 5 games after adding [points] in it.  *)
let update_last_games lst outcome = 
  match List.rev lst with
  | a::b::c::d::e::[] -> outcome::a::b::c::d::[]
  | h::t -> outcome::h::t
  | [] -> outcome::[]

(** [get_outcome our_score opp_score] returns the number of points the user's
    team will win after this game. A win results in 3, draw in 1 and a loss 
    in 0. *)
let get_outcome our_score opp_score = 
  if our_score > opp_score then Win
  else if our_score = opp_score then Tie
  else Loss

(** [update_form current_form points] returns the updated form after factoring 
    in the points gained in the latest result. *)
let update_form current_form (outcome:outcome) = 
  (* This is rudimentary for now, and we plan on making it more sophisticated 
     in the next sprint.  *)
  match current_form with
  | VeryWeak when outcome = Win -> Weak
  | VeryWeak -> VeryWeak
  | Weak when outcome = Tie -> Weak
  | Weak when outcome = Win -> Normal
  | Weak -> VeryWeak
  | Normal when outcome = Tie -> Normal
  | Normal when outcome = Win -> Strong
  | Normal -> Weak
  | Strong when outcome = Win -> VeryStrong
  | Strong when outcome = Tie -> Strong
  | Strong -> Normal
  | VeryStrong when outcome = Win -> VeryStrong
  |_ -> Strong

(** [update_record current_record points] returns the the updated progress 
    record afrer factoring in the points won in the latest fixture.  *)
let update_record (current_record:progress) = function
  | Win -> {games_played=current_record.games_played+1;
            wins = current_record.wins+1;
            losses=current_record.losses;
            draws=current_record.draws}
  | Tie -> {games_played=current_record.games_played+1;
            wins = current_record.wins;
            losses=current_record.losses;
            draws=current_record.draws+ 1}
  | Loss -> {games_played= current_record.games_played+1;
             wins = current_record.wins;
             losses=current_record.losses+1;
             draws=current_record.draws}
  | Void -> {games_played= current_record.games_played;
             wins = current_record.wins;
             losses=current_record.losses;
             draws=current_record.draws}

(* defined in module *)
let sim_match (sim_t:t) (team_rating:Game.team_rating) 
    (opp_team_rating:Game.team_rating) (opp_tactic:Game.tactic) = 
  let our_form_score = get_form_score sim_t.current_form  in 
  let opp_form_score = Random.int 30 |> gen_random_form |> get_form_score in
  let our_points_scored = calculate_points (our_form_score - opp_form_score) 
      team_rating opp_team_rating (get_tactic_number sim_t.tactic) 
      (get_tactic_number opp_tactic) (compute_match_points) in
  let opp_points_scored = calculate_points (opp_form_score - our_form_score) 
      opp_team_rating team_rating (get_tactic_number opp_tactic) 
      (get_tactic_number sim_t.tactic) (compute_match_points) in
  let our_total_score = List.fold_left (+) 0 our_points_scored in 
  let opp_total_score = List.fold_left (+) 0 opp_points_scored in
  let outcome =  get_outcome our_total_score opp_total_score in 
  {tactic = sim_t.tactic; 
   last_5_games = update_last_games sim_t.last_5_games outcome;
   current_form = update_form (sim_t.current_form) outcome;
   current_record = update_record sim_t.current_record outcome;
   last_result = {our_score = our_points_scored; opp_score = opp_points_scored}
  }

(* defined in module *)
let sim_quarter (sim_t:t) (team_rating:Game.team_rating) 
    (opp_team_rating:Game.team_rating) (opp_tactic:Game.tactic) (quarter:int) = 
  let our_form_score = get_form_score sim_t.current_form  in 
  let opp_form_score = Random.int 30 |> gen_random_form |> get_form_score in
  let our_points_scored =
    [(List.nth (calculate_points (our_form_score - opp_form_score) team_rating 
                  opp_team_rating (get_tactic_number sim_t.tactic)
                  (get_tactic_number opp_tactic) compute_quarter_points) 0)
     + (List.fold_left (+) 0 sim_t.last_result.our_score)] in
  let opp_points_scored =
    [(List.nth (calculate_points (opp_form_score - our_form_score) opp_team_rating
                  team_rating (get_tactic_number opp_tactic)
                  (get_tactic_number sim_t.tactic) compute_quarter_points) 0)
     + (List.fold_left (+) 0 sim_t.last_result.our_score)] in

  let last_result = if quarter > 0 then
      {our_score=sim_t.last_result.our_score@our_points_scored;
       opp_score =sim_t.last_result.opp_score@opp_points_scored} else 
      {our_score=our_points_scored;opp_score=opp_points_scored} in
  let our_total_score = List.fold_left (+) 0 last_result.our_score in 
  let opp_total_score = List.fold_left (+) 0 last_result.opp_score in
  let points =  get_outcome our_total_score opp_total_score in
  let current_record = 
    if quarter = 4 
    then update_record sim_t.current_record points 
    else sim_t.current_record in
  let current_form = 
    if quarter = 4 
    then update_form sim_t.current_form points 
    else sim_t.current_form in 
  let last_5_games = 
    if quarter = 4 
    then update_last_games sim_t.last_5_games points
    else sim_t.last_5_games in

  {tactic = sim_t.tactic; 
   last_5_games = last_5_games;
   current_form = current_form;
   current_record = current_record;
   last_result = last_result;
  }

let reset_game_result t = {t with last_result = {our_score=[]; opp_score=[]}}