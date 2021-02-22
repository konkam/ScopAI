RandomDecision <- function(game_state, player) {
  play <- sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take <- TakeableCardsOnBoardBruteForce(card = play, board = game_state$board) %>%
    sample(size = 1) %>%
    .[[1]]
  return(list(play = play, take = take))
}

DummyDecision <- function(game_state, player) {
  play <- sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take <- c("B1")
  return(list(play = play, take = take))
}

invlogit = function(x) 1/(1+exp(x))

OptimisableDecision <- function(game_state, player, params) {
  hand = GetPlayerHand(game_state = game_state, player = player)
  decisions_by_denari = list()
  for (card in hand){
    take_options = TakeableCardsOnBoardBruteForce(card = card, board = game_state$board)
    for (t in take_options){
      if (length(t)>0) denari = CountDenariNumber(c(card, t))
      else denari = 0
      decision = list(play = card, take = t)
      if ( !as.character(denari) %in% names(decisions_by_denari)) decisions_by_denari[[as.character(denari)]] = list(decision)
      else {
        decisions_by_denari[[as.character(denari)]] = append(decisions_by_denari[[as.character(denari)]], list(decision))
        }
    }
  }
  u = runif(n = 1)
  if (u<invlogit(params)){
    maxdenari = max(as.numeric(names(decisions_by_denari)))
    return(decisions_by_denari[[as.character(maxdenari)]][[1]])
  }
  else{
    mindenari = min(as.numeric(names(decisions_by_denari)))
    return(decisions_by_denari[[as.character(mindenari)]][[1]])
  }

}
