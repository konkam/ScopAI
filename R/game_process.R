#' Title
#'
#' @param starting_deck
#' @param decisions1
#' @param decisions2
#' @param starting_player
#'
#' The game state is a list with elements:
#' deck (card to draw)
#' hand1
#' hand2
#' stack1
#' stack2
#' board
#' turn
#' last_taker
#'
#' @return
#'
RunGame = function(seed, decisions1, decisions2, starting_player){
  if(length(decisions1)!=18){
    stop("decision1 must contain 18 decisions")
  }
  if(length(decisions2)!=18){
    stop("decision2 must contain 18 decisions")
  }
  second_player = ifelse(starting_player==1, )
  game_state = InitialiseGameState()
 while (game_state$turn < 36){
    while(1==1){
      1+1
    }
 }
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
    game_state$player1$hand = c(game_state$player1$hand, game_state$deck[1:3])
    game_state$player2$hand = c(game_state$player2$hand, game_state$deck[4:6])
  }
  else{
    game_state$player1$hand = c(game_state$player1$hand, game_state$deck[4:6])
    game_state$player2$hand = c(game_state$player2$hand, game_state$deck[1:3])
  }
  game_state$deck = game_state$deck[7:length(game_state$deck)]
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
#'
#' @examples
PlayCard = function(game_state, player, decision){
  if(player!=1&&player!=2){
    stop(print("starting_player should be 1 or 2"))
  }
  if(player==1){
    game_state$player1$hand = game_state$player1$hand[game_state$player1$hand!=decision$play]
    if(length(decision$take>0)){
      game_state$player1$stack = c(game_state$player1$stack, decision$play, decision$take)
      game_state$board = game_state$board[!game_state$board%in%decision$take]
      game_state$last_taker=1
    }
    else{
      game_state$board = c(game_state$board, decision$play)
    }
  }
  else if(player==2){
    game_state$player2$hand = game_state$player2$hand[game_state$player2$hand!=decision$play]
    if(length(decision$take>0)){
      game_state$player2$stack = c(game_state$player2$stack, decision$play, decision$take)
      game_state$board = game_state$board[!game_state$board%in%decision$take]
      game_state$last_taker=2
    }
    else{
      game_state$board = c(game_state$board, decision$play)
    }
  }
  game_state$turn = game_state$turn + 1
  return(game_state)
}

FinishGame = function(game_state){
  if(game_state$last_taker==1){
    game_state$player1$stack = c(game_state$player1$stack, game_state$board)
  }
  else if(game_state$last_taker==2){
    game_state$player2$stack = c(game_state$player2$stack, game_state$board)
  }
  else{
    stop(print("game_state$last_taker should be 1 or 2"))
  }
  game_state$board = NULL
  return(game_state)
}

CheckAtFinish = function(game_state){
  if(game_state$turn != 36){
    stop("Trying to finish the game too early")
  }
  if(length(game_state$deck)!=0){
    stop("Trying to finish but the deck is not empty")
  }
}
