
#' Title
#'
#' @param letter
#'
#' @return
#' @export
#'
#' @examples
GiveFullColourName = function(letter) {
  return(colours_dict[letter])
}

SwitchPlayer = function(player){
  return(ifelse(test = player==1, yes = 2, no = 1))
}
GetPlayerName = function(player){
  return(ifelse(test = player==1, yes = "player1", no = "player2"))
}

GetPlayerHand = function(game_state, player){
  game_state[[GetPlayerName(player)]]$hand
}
