(** The abstract type representing the simulation state during a game. *)
type t 

(** This type represents the outcome of a game played. *)
type outcome = 
  | Win | Tie | Loss | Void

(** The type representing outcome of a typical basketball game. 
    For example: {our_score=[14;29;54;78]; opp_score=[21;35;47;67]} denotes a
    game that ended 78-67 in our favor with the scores updated after every 
    quarter.  *)
type game_result = {
  our_score:int list;
  opp_score:int list;
}

(** [get_wins t] is the number of wins of the team associated with [t]. *)
val get_wins : t -> int

(** [get_draws t] is the number of draws of the team associated with [t]. *)
val get_draws : t -> int

(** [get_losses t] is the number of losses of the team associated with [t]. *)
val get_losses : t -> int

(** [get_sim_tactic sim_t] is the current team tactic. *)
val get_sim_tactic : t -> Game.tactic

(** [init_sim tactic] initializes the sim_t record for playing purposes.  *)
val init_sim : Game.tactic -> t

(** [update_tactic sim_t tactic] updates the tactic that the team will employ in
    upcoming matches and returns the updated sim_t record.  *)
val update_tactic : t -> Game.tactic -> t

(** [sim_match sim_t our_rating opp_rating opp_tactic] sims a game between the 
    two teams according to the tactics given and returns the updated sim_t 
    record. *)
val sim_match : t -> Game.team_rating -> Game.team_rating -> Game.tactic -> t

(**[sim_quarter sim_t our_rating opp_rating opp_tactic quarter] sims a quarter
   between the two teams according to the tactics given and returns the 
   updated sim_t record. *)
val sim_quarter : t -> Game.team_rating -> Game.team_rating 
  -> Game.tactic -> int -> t

(** [last_result sim_t] returns the scoreline for the last game played by the 
    team. It retuns an empty scoreline if tno game has been played.   *)
val last_result : t -> game_result

(** [previous_outcome sim_t] is the [outcome] of the most recent match. *)
val previous_outcome: t -> outcome

(** [reset_game_result sim_t] is [sim_t] with its last result cleared, and left
    with empty lists. *)
val reset_game_result : t -> t
