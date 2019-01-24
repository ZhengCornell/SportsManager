(* defined in module *)
type object_phrase = string list

(* defined in module *)
type command = 
  | Challenge of object_phrase
  | Team
  | Arena
  | Shop
  | Activate of object_phrase
  | Rest of object_phrase
  | Buy of object_phrase
  | Sell of object_phrase
  | Scout of object_phrase
  | Analyze of object_phrase
  | Next_day
  | Quit
  | Move of object_phrase
  | Help
  | Train of object_phrase
  | Practice
  | Update of object_phrase
  | Sort of object_phrase
  | Yes
  | No

(* defined in module *)
type player_sort =
  | PlayerName | Price | Cost | Level | Position | PlayerID | Status | Pace
  | Shooting | Defending | Passing | Dribbling | Physicality | PlayerOverall
  | PlayerNone

(* defined in module *)
type team_sort =
  | TeamName | Description | TeamID | Offence | Defence | TeamOverall | TeamNone

(* defined in module *)
exception Empty

(* defined in module *)
exception Malformed

(** [check_status str_lst] creates the command associated with [str_lst].

    Raises: [Malformed] if the command is [Quit], [Team], [Arena], [Shop], 
                [Next_day], [Practice], or [Help], and there is a 
                trailing object_phrase, or 
                if the command is [Schedule], [Active], [Rest], [Buy], [Sell], 
                [Scout], [Analysis], [Move], [Train], or [Update] and there is 
                no trailing object_phrase. *)
let check_status h t =
  match String.lowercase_ascii h with
  | "quit" when t = [] -> Quit
  | "team" when t = [] -> Team
  | "arena" when t = [] -> Arena
  | "shop" when t = [] -> Shop
  | "nextday" when t = [] -> Next_day
  | "practice" when t = [] -> Practice
  | "help" when t = [] -> Help
  | "yes" when t = [] -> Yes
  | "no" when t = [] -> No

  | "challenge" when t != [] -> Challenge t
  | "activate" when t != [] -> Activate t
  | "rest" when t != [] -> Rest t
  | "buy" when t != [] -> Buy t
  | "sell" when t != [] -> Sell t
  | "scout" when t != [] -> Scout t
  | "analyze" when t != [] -> Analyze t
  | "move" when t != [] -> Move t
  | "train" when t != [] -> Train t
  | "update" when t != [] -> Update t
  | "sort" when t != [] -> Sort t


  | _ -> raise Malformed

(** [check_empty lst] checks that [lst] is not the empty list, and therefore
    allow the process of creating a command to continue.

    Raises: [Empty] if [lst] is empty, aka an empty user input. *)
let check_empty = function
  | [] -> raise Empty
  | h :: t -> check_status h t

(** [deal str] is the list created from [str] after having been split by the 
    empty character. Empty string entries are removed.

    Example: [deal "Hello    world!"] would be [["Hello";"world!"]]. *)
let deal str = 
  String.split_on_char ' ' str |> 
  List.filter (fun s -> (String.length s) > 0)

(* defined in module *)
let parse str =
  str |> deal
  |> check_empty

(* defined in module *)
let parse_player_sort str = match String.lowercase_ascii str with
  | "name" | "player name" -> PlayerName
  | "price" -> Price
  | "cost" -> Cost
  | "level" -> Level
  | "position" -> Position
  | "id" | "id #" | "player id" -> PlayerID
  | "status" -> Status
  | "pace" -> Pace
  | "shooting" -> Shooting
  | "defending" -> Defending
  | "passing" -> Passing
  | "dribbling" -> Dribbling
  | "physicality" -> Physicality
  | "overall" -> PlayerOverall
  | "none" | "reset" -> PlayerNone
  | _ -> raise Malformed

(* defined in module *)
let parse_team_sort str = match String.lowercase_ascii str with
  | "name" | "team name" -> TeamName
  | "description" -> Description
  | "id" | "id #" | "team id" -> TeamID
  | "offence" -> Offence
  | "defence" -> Defence
  | "overall" -> TeamOverall
  | "none" | "reset" -> TeamNone
  | _ -> raise Malformed