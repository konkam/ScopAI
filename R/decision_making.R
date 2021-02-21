RandomDecision = function(game_state, player){
  play = sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take
}

DummyDecision = function(game_state, player){
  play = sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take = c("B1")
  return(list(play = play, take = take))
}
