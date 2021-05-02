#' Convert the state of the game as seen by a player to a mostly boolean encoding
#' 
#' Cards are encoding as boolean variables, scores and turns are left as integer variables
#'
#' @param state. A game state as obtained from the function `RunGame$game_history[[turn]]`. 
#' @param player. 1 or 2 depending on who is playing. 
#'
#' @return A vector resulting from the concatenation of 40 binary entries in stack_player, in stack_opponent, board, hand, a real between 0 and 1 indicating how close is the game to its end, and two integers representing the number of scope for the player and for the opponent.
#'
#' @examples
#'   g <- InitialiseGameState()
#'   g2 <- ScopAI:::PlayCard(g, decision = list(play = "B2", take = c("B9", "B10")), player = 1)
#'   ScopAI:::EncodeStateAsBinaryVector(g2)
EncodeStateAsBinaryVector = function(state, player = 1){
  player_name = paste("player", player, sep="")
  opponent_name = SwitchPlayer(player = player) %>% paste("player", ., sep="")
#   unknown = (ordered_deck %in% c(state$deck, state["opponent_name"])) %>% as.numeric()
  stack_player = (ordered_deck %in% state[[player_name]][["stack"]]) %>% as.numeric %>% setNames(ordered_deck %>% paste("stack_player", ., sep="_"))
  stack_opponent = (ordered_deck %in% state[[opponent_name]][["stack"]]) %>% as.numeric %>% setNames(ordered_deck %>% paste("stack_opponent", ., sep="_"))
  board = (ordered_deck %in% state$board) %>% as.numeric  %>% setNames(ordered_deck %>% paste("board", ., sep="_"))
  hand = (ordered_deck %in% state[[player_name]][["hand"]]) %>% as.numeric %>% setNames(ordered_deck %>% paste("hand", ., sep="_"))
  scaled_turn = state$turn/37 %>% setNames("scaled_turn")
  player_scope = state[[player_name]][["scope"]] %>% setNames("player_scope")
  opponent_scope = state[[opponent_name]][["scope"]] %>% setNames("opponent_scope")
  c(stack_player, stack_opponent, board, hand, scaled_turn, player_scope, opponent_scope) %>% return
}

#' Convert the action taken by a player to a boolean encoding
#'
#' @param decision. The result of a decision function, a list with arguments "play" and "take". 
#'
#' @return A binary vector of size 80.
#'
#' @examples
#'     g <- InitialiseGameState()
#'     decision <- ScopAI:::RandomDecision(g, player = 1)
#'     ScopAI:::EncodeActionAsBinaryVector(decision)
EncodeActionAsBinaryVector = function(decision){
  play = (ordered_deck %in% decision$play) %>% as.numeric %>% setNames(ordered_deck %>% paste("play", ., sep="_"))
  take = (ordered_deck %in% decision$take) %>% as.numeric %>% setNames(ordered_deck %>% paste("take", ., sep="_"))
  c(play, take) %>% return
}
