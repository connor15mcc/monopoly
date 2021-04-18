type player = {
  pos : int;
  cash : int;
  properties : Board.square list;
  cards : Cards.card list;
  jail : bool;
  token : Token.token option;
  name : string option;
  bankrupt : bool;
}

let init_player =
  {
    pos = 0;
    cash = 0;
    properties = [];
    cards = [];
    jail = false;
    token = None;
    name = None;
    bankrupt = false;
  }

(* [move p dr] returns a new player type p after moving dr spaces. *)
let move p dr =
  {
    pos = p.pos + dr;
    cash = p.cash;
    properties = p.properties;
    cards = p.cards;
    jail = p.jail;
    token = p.token;
    name = p.name;
    bankrupt = p.bankrupt;
  }

let position player = player.pos

let cash player = player.cash

let properties player = player.properties

let cards player = player.cards

let jail player = player.jail

let token player = player.token

let name player = player.name

let sum_mortgage_value prop = List.fold_left ( + ) 0 prop

let int_of_square sq =
  match Board.mortgage sq with None -> 0 | Some x -> x

let intlist_of_squarelist lst = List.map int_of_square lst

(* TODO: need to include houses *)
let net_worth player =
  player.cash
  + sum_mortgage_value (intlist_of_squarelist player.properties)

let bankrupt player = net_worth player >= 0

let add_property player property =
  {
    pos = player.pos;
    cash = player.cash;
    properties = property :: player.properties;
    cards = player.cards;
    jail = player.jail;
    token = player.token;
    name = player.name;
    bankrupt = player.bankrupt;
  }
