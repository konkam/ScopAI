#' Is A Decision Possible
#' Check if a decision is possible according to the decision alone, without taking account any other parameter
#' @param decision
#'
IsADecisionPossible <- function(decision) {
  if (length(decision) != 2) stop("decision has not the good format (list of length 2)")
  if (any(sort(names(decision)) != c("play", "take"))) stop("decision has not the good format (named list with play / take)")
  if (!"none" %in% decision$take) {
    if (GetSumValuesOfCards(decision$take) != GetValueOfCard(decision$play)) stop("the played card value doesn't match the taken cards")
  }
}

#' Is A Decision Valid
#' Check if a decision is valid according to the decision alone, but also to the hand and board
#' @param game_state
#' @param player
#' @param decision
#'
IsADecisionValid <- function(game_state = InitialiseGameState(seed = 1),
                             player = 1,
                             decision) {
  IsADecisionPossible(decision)
  if (!decision$play %in% game_state[[paste0("player", player)]]$hand) stop("the card played is not in the hand of the player")
  if (any(!decision$take %in% c(game_state$board, "none"))) stop("some of the cards taken are not on the board")
  if (any(GetValuesOfCards(game_state$board) == GetValueOfCard(decision$play)) &
      length(decision$take) > 1) stop("at least one card on the board matches the value of the play, you cannot take several card")
  if ("none" %in% decision$take) {
    if (any(unlist(TakeableCardsOnBoardOptimized(decision$play,
                                                 game_state$board)) != "none")) stop("you have to take cards if it is possible")
  }
}

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

invlogit <- function(x) 1 / (1 + exp(x))

OptimisableDecision <- function(game_state, player, params) {
  hand <- GetPlayerHand(game_state = game_state, player = player)
  decisions_by_denari <- list()
  for (card in hand) {
    take_options <- TakeableCardsOnBoardBruteForce(card = card, board = game_state$board)
    for (t in take_options) {
      if (length(t) > 0) {
        denari <- CountDenariNumber(c(card, t))
      } else {
        denari <- 0
      }
      decision <- list(play = card, take = t)
      if (!as.character(denari) %in% names(decisions_by_denari)) {
        decisions_by_denari[[as.character(denari)]] <- list(decision)
      } else {
        decisions_by_denari[[as.character(denari)]] <- append(decisions_by_denari[[as.character(denari)]], list(decision))
      }
    }
  }
  u <- runif(n = 1)
  if (u < invlogit(params)) {
    maxdenari <- max(as.numeric(names(decisions_by_denari)))
    return(decisions_by_denari[[as.character(maxdenari)]][[1]])
  }
  else {
    mindenari <- min(as.numeric(names(decisions_by_denari)))
    return(decisions_by_denari[[as.character(mindenari)]][[1]])
  }
}
