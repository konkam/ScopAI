#' Title
#'
#' @param starting_deck
#' @param decisions1
#' @param decisions2
#' @param starting_player
#'
#' The game state is:
#' deck (card to draw)
#' hand1
#' hand2
#' stack1
#' stack2
#' board
#' turn counter
#'
#' @return
#' @export
#'
RunGame = function(starting_deck, decisions1, decisions2, starting_player){
 1+1
}

#' Title
#'
#' @param game_state
#' @param starting_player
#'
#' @return
#' @export
#'
DealPlayersCards = function(game_state, starting_player){
  if(starting_player!=1&&starting_player!=2){
    stop(print("starting_player should be 1 or 2"))
  }
  if(starting_player==1){
    game_state$hand1 = c(game_state$hand1, game_state$deck[1:3])
    game_state$hand2 = c(game_state$hand2, game_state$deck[4:6])
  }
  else{
    game_state$hand1 = c(game_state$hand1, game_state$deck[4:6])
    game_state$hand2 = c(game_state$hand2, game_state$deck[1:3])
  }
  game_state$deck = game_state$deck[7:length(game_state$deck)]
  return(game_state)
}

#' Title
#'
#' @param game_state
#'
#' @return
#' @export
#'
#' @examples
DealBoardCards = function(game_state){
  game_state$board = game_state$deck[1:4]
  game_state$deck = game_state$deck[5:length(game_state$deck)]
  return(game_state)
}

#' Title
#'
#' @param game_state
#' @param player
#' @param decision
#'
#' A decision is a list(play = card played, a single card, take = cards taken.)
#'
#' @return
#' @export
#'
#' @examples
PlayCard = function(game_state, player, decision){
  if(player!=1&&player!=2){
    stop(print("starting_player should be 1 or 2"))
  }
  if(player==1){
    game_state$hand1 = game_state$hand1[game_state$hand1!=decision$play]
    game_state$stack1 = c(game_state$stack1, decision$play, decision$take)
  }
  if(player==2){
    game_state$hand2 = game_state$hand2[game_state$hand2!=decision$play]
    game_state$stack2 = c(game_state$stack2, decision$play, decision$take)
  }
  game_state$board = game_state$board[!game_state$board%in%decision$take]
  return(game_state)
}
