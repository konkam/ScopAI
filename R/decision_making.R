#' Is A Decision Possible
#' Check if a decision is possible according to the decision alone, without taking account any other parameter
#' @param decision
#'
IsADecisionPossible <- function(decision) {
  if (length(decision) != 2) stop("decision has not the good format (list of length 2)")
  if (any(sort(names(decision)) != c("play", "take"))) stop("decision has not the good format (named list with play / take)")
  if (!is.null(decision$take)) {
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
  if (!decision$play %in% GetPlayerHand(game_state, player)) stop("the card played is not in the hand of the player")
  if (any(!decision$take %in% game_state$board)) stop("some of the cards taken are not on the board")
  if (any(GetValuesOfCards(game_state$board) == GetValueOfCard(decision$play)) &
      length(decision$take) > 1) stop("at least one card on the board matches the value of the play, you cannot take several card")
  if (is.null(decision$take)) {
    if (any(!is.null(unlist(TakeableCardsOnBoardOptimized(decision$play,
                                                          game_state$board))))) stop("you have to take cards if it is possible")
  }
}


RandomDecision <- function(game_state, player) {
  play <- sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take <- TakeableCardsOnBoardBruteForce(card = play, board = game_state$board) %>%
    sample(size = 1) %>%
    .[[1]]
  return(list(play = play, take = take))
}

RandomDecisionOptimized <- function(game_state, player) {
  ListAllPossibleDecisions(game_state, player) %>% sample(size = 1) %>% .[[1]]
}

OptimizedDecision <- function(game_state,
                              player = 1,
                              if_equality_min_risk_of_scopa = T,
                              cards_weight = 1,
                              primiera_weight = 1,
                              sette_bello_weight = 1,
                              denari_weight = 1,
                              scope_weight = 1,
                              explain = F) {
  if (explain) ShowHandsAndBoard(game_state)
  # list the possible decisions
  possible_decisions <- ListAllPossibleDecisions(game_state = game_state, player = player)

  # compute the expected score for each decision
  expected_scores <- sapply(possible_decisions, function(dec)
    GiveExpectedScoreForADecision(game_state = game_state, player = player, decision = dec,
                                  cards_weight = cards_weight,
                                  primiera_weight = primiera_weight,
                                  sette_bello_weight = sette_bello_weight,
                                  denari_weight = denari_weight,
                                  scope_weight = scope_weight))

  # take the highest expected scores
  optimized_decisions <- possible_decisions[expected_scores == max(expected_scores)]
  if (explain) print(glue::glue("the highest expected score is {round(max(expected_scores), 2)}"))

  # if there is only one choose it
  if (length(optimized_decisions) == 1) return(optimized_decisions[[1]])

  # otherwise, you can play at random
  if (!if_equality_min_risk_of_scopa) return(sample(optimized_decisions, 1)[[1]])

  if (explain) print("there are different decisions possibles with the same expected score")
  # or you can try to minimize the risk of scopa
  # start by getting the board value for each decision
  new_board_values <- sapply(optimized_decisions, function(dec)
    GetSumValuesOfCards(PlayCard(game_state = game_state, player = player, dec)$board))
  # order the decisions according to this value
  optimized_decisions <- optimized_decisions[order(new_board_values)]
  new_board_values <- sort(new_board_values)

  # if there is at least one decision leaving the board higher than 10, restrict to it and then play the lowest remaining board with []
  if (new_board_values[length(new_board_values)] > 10) {
    if (explain) print("took a decision allowing to have a board higher than 10 (anti-scopa)")
    return(optimized_decisions[new_board_values > 10][[1]])
  }

  if (explain) print("took a decision giving the lowest remaining board")
  # otherwise just play for the lowest remaining board
  return(optimized_decisions[[1]])
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
