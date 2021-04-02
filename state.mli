type property

type game_state

val init : game_state

val move : game_state -> Player.player list -> game_state

val next_player : game_state -> Player.player

val roll_dice : unit -> int