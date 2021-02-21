
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

LookWhichCardsYouCanGetOnBoard <- function(one_card, board) {
  one_card_value <- GetValuesOfCards(one_card)
  board_with_values <- setNames(object = GetValuesOfCards(board),
                                nm = board)
  # don't consider cards on the board that are higher than your card
  board_with_values <- board_with_values[board_with_values <= one_card_value]
  take_opportunities <- vector()
  for (i in 1:length(board_with_values)) {
    for (combination  in combn(board_with_values, i, simplify = F)) {
      if (sum(combination) == one_card_value) {
        take_opportunities[[i]] <- names(combination)
      }
    }
  }
  return(take_opportunities)
}




