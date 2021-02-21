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
RunGame = function(starting_player, DecisionFunction, seed = 1){
  game_state = InitialiseGameState()
  current_player = starting_player
  game_states = list()
  game_states[[game_state$turn]]=game_state
 while (length(game_state$deck)>=6&&game_state$turn<=36){
   # print(length(game_state$deck))

    while(length(GetPlayerHand(game_state, current_player))>0){
      game_state = PlayCard(game_state = game_state, player = current_player, decision = DecisionFunction(game_state, current_player))
      current_player = SwitchPlayer(current_player)
      game_states[[game_state$turn]]=game_state
    }
   game_state = DealPlayersCards(game_state = game_state, starting_player = starting_player)
 }
  # print(length(game_state$deck))
  game_state = FinishGame(game_state = game_state)
  # At the end of the game, people have NAs in their hands
  return(list(score_player1 = GiveScoreFromStateForAPlayer(game_state, player = 1), score_player2 = GiveScoreFromStateForAPlayer(game_state, player = 2), game_history = game_states))
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
  if(length(game_state$deck)<6){
    stop("The deck does not have enough cards left to deal")
  }
  if(starting_player==1){
    game_state$player1$hand = c(game_state$player1$hand, game_state$deck[1:3])
    game_state$player2$hand = c(game_state$player2$hand, game_state$deck[4:6])
  }
  else{
    game_state$player1$hand = c(game_state$player1$hand, game_state$deck[4:6])
    game_state$player2$hand = c(game_state$player2$hand, game_state$deck[1:3])
  }
  if(length(game_state$deck)==6){
    game_state$deck = c()
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
PlayCard <- function(game_state, player, decision) {
  if (player != 1 && player != 2) {
    stop(print("starting_player should be 1 or 2"))
  }
  if (player == 1) {
    game_state$player1$hand <- game_state$player1$hand[game_state$player1$hand != decision$play]
    if (length(decision$take) > 0) {
      game_state$player1$stack <- c(game_state$player1$stack, decision$play, decision$take)
      game_state$board <- game_state$board[!game_state$board %in% decision$take]
      game_state$last_taker <- 1
      if (length(game_state$board) == 0) game_state$player1$scope <- game_state$player1$scope + 1
    } else{
      game_state$board <- c(game_state$board, decision$play)
    }
  } else {
    game_state$player2$hand <- game_state$player2$hand[game_state$player2$hand != decision$play]
    if (length(decision$take) > 0) {
      game_state$player2$stack <- c(game_state$player2$stack, decision$play, decision$take)
      game_state$board <- game_state$board[!game_state$board%in%decision$take]
      game_state$last_taker <- 2
      if (length(game_state$board) == 0) game_state$player2$scope <- game_state$player2$scope + 1
    } else {
      game_state$board <- c(game_state$board, decision$play)
    }
  }
  game_state$turn <- game_state$turn + 1
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
