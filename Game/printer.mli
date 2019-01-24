(** 
    This module holds all the required printing functions for the game's 
    terminal based front end. 
*)

(* Non-printing functions
 ******************************************************************************)

(** [cmd_to_string cmd] is the string representation of [cmd] in lowercase. *)
val cmd_to_string : Command.command -> string

(** [reset_sort ()] resets the current sort status defined by the user. *)
val reset_sort : unit -> unit

(** [set_player_sort sort] sets the current sorting of the player lists to
    [sort]. The list is then sorted in ascending order next time it is 
    printed. *)
val set_player_sort : Command.player_sort -> unit

(** [set_team_sort sort] sets the current sorting of the team lists to
    [sort]. The list is then sorted in ascending order next time it is 
    printed. *)
val set_team_sort : Command.team_sort -> unit


(* Main printing functions
 ******************************************************************************)

(** [print_red str] prints [str] to stdout in red text. Equivalent to function
    [ANSITerminal.(print_string [red] str)]. *)
val print_red : string -> unit

(** [print_blue str] prints [str] to stdout in blue text. Equivalent to function
    [ANSITerminal.(print_string [blue] str)]. *)
val print_blue : string -> unit

(** [print_cmd_not_possible cmd] prints a helpful message if a user tries to use
    a command in the wrong area in the game. *)
val print_cmd_not_possible : Command.command -> unit

(** [print_help_command location] prints the required message when the 'help'
    command is called by the user. The help command displays slightly different 
    messages based on [location].

    Example: [print_help_command Arena] prints all of the general commands and
    their respective descriptions, and the 'arena' command and its 
    description. *)
val print_help_command : State.location -> unit

(** [print_menu adv state] prints the menu containing the player's basic data,
    such as their current position, budget, and team. All information is given
    by [state]. *)
val print_menu : Game.t -> State.t -> unit

(** [print_next_day_info state] prints the information of the updated game
    state [state] after advancing by one day. *)
val print_next_day_info : State.t -> unit

(** [print_game result] prints the outcome of [result]. The score after each
    quarter and the resulting winner is displayed. *)
val print_game : Sim.game_result -> unit

(** [ask_prompt state new_state] prints a question that requires the user's
    confirmation, where confirming would move the game state from [state] to 
    [new_state]. *)
val ask_prompt : State.t -> State.t -> bool

(** [print_setup_b state name] displays the user's name [name], and introduces
    the user to the game by displaying the preliminary commands necessary for
    the basics of the game. *)
val print_setup_b : State.t -> string -> unit

(** [print_setup_a ()] displays a welcome message and a message that prompts
    the user to enter their name. *)
val print_setup_a : unit -> unit

(** [print_quarter_promt quarter sim] is [true] if the user wants to change
    their tatic, but false otherwise. *)
val print_quarter_prompt : int -> Sim.t -> bool


(* Graphics printing
 ******************************************************************************)

(** [print_title_screen ()] Prints the screen that the player sees before they
    load in a game. Prints out the intro graphic defined in
    [graphics/start_screen_logo.txt], then prints out the screen where you type
    in the name of the file you want to load. *)
val print_title_screen : unit -> unit

(** [print_training_graphic ()] prints a short graphic depicting a person
    practicing. If the terminal is sufficiently tall (56 rows), prints a high
    definition graphic, but if it is under, prints a stick figure graphic.
    Graphics used are defined in [graphics/train_...]. Used for the [Train]
    function. Clears graphic once finished. *)
val print_training_graphic : unit -> unit

(* Table printing functions
 ******************************************************************************)

(** [print_single_player player] prints the information of [player] in a table. 
    Clears the terminal of text beofre printing.

    Table headers are:
    [Name; Price; Position; Rating; Current Team; Player ID].

    After printing the table, stdin will wait for the enter key to be pressed
    before moving on. *)
val print_single_player : Game.single_player -> unit

(** [print_single_team team] Prints the information of [team] in a table.
    Uses two tables: One for displaying the overall information of [team], and
    another for displaying the player composition of [team]. Clears the terminal
    of text before printing.

    Overall team table headers are:
    [Team Name; Description; Team ID; Team Stats:; Offence; Defence; Overall].
    Team Composition headers are:
    [Name; Price; Position; Player ID; Player Stats:; Pace; Shooting; Defending;
    Passing; Dribbling; Physicality; Overall].

    After printing the table, stdin will wait for the enter key to be pressed
    before moving on. *)
val print_single_team : Game.single_team -> unit

(** [print_shop_table adv state] prints a table of the current players in the
    market defined in [state]. Only prints the players that are not currently in
    a team. 

    Table headers:
    [Name; Price; Position; Player ID; Player Stats:; Pace; Shooting; Defending;
    Passing; Dribbling; Physicality; Overall]. *)
(* Used in shop_loop function in main.ml *)
val print_shop : Game.t -> State.t -> unit

(** [print_team adv state] prints the the information of the player's
    current team composition as defined in [state]. Clears the terminal of text
    before printing.

    Table headers are:
    [Name; Price; Position; Player ID; Player Stats:; Pace; Shooting; Defending;
    Passing; Dribbling; Physicality; Overall]. *)
(* Used in team_loop function in main.ml *)
val print_team : Game.t -> State.t -> unit

(** [print_arena adv state] prints a table of the current teams in the arena
    defined in [state].

    Table Headers:
    [Team Name; Description; Team ID; Team Stats:; Offence; Defence; Overall]. 
*)
(* Used in arena_loop function in main.ml *)
val print_arena : Game.t -> State.t -> unit

(** [print_train adv state] prints (to stdout) a list of the current players
    on the user's team defined in [state].

    Table headers are:
    [Name; Price; Position; Player ID; Player Stats:; Pace; Shooting; Defending;
    Passing; Dribbling; Physicality; Overall]. *)
(* Used in train_loop function in main.ml *)
val print_train : Game.t -> State.t -> unit