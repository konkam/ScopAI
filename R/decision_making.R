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
IsADecisionValid <- function(game_state,
                             player,
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

DummyDecision <- function(game_state, player) {
  play <- sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take <- c("B1")
  return(list(play = play, take = take))
}

RandomDecision <- function(game_state, player) {
  play <- sample(x = GetPlayerHand(game_state = game_state, player = player), size = 1)
  take <- TakeableCardsOnBoardBruteForce(card = play, board = game_state$board) %>%
    sample(size = 1) %>%
    first
  return(list(play = play, take = take))
}

RandomDecisionOptimized <- function(game_state = InitialiseGameState(seed = 1), player = 1) {
  ListAllPossibleDecisions(game_state, player) %>% sample(size = 1) %>% first
}
# According to microbenchmark, RandomDecisionOptimized is actually longer than RandomDecision :)

OptimizedDecision <- function(game_state,
                              player,
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


# Anticipate the other next move -------

OptimizedDecisionNPlus1 <- function(game_state,
                                    player,
                                    option_for_n_plus_1 = c("cheater", "worst_case_scenario",
                                                            "random_play", "true_calculus",
                                                            "ponderated_scenario",
                                                            "true_if_not_too_much")[1],
                                    cards_weight = 1,
                                    primiera_weight = 1,
                                    sette_bello_weight = 1,
                                    denari_weight = 1,
                                    scope_weight = 1) {

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

  expected_scores_n_plus_1 <- rep(0, length(possible_decisions))
  other_player <- SwitchPlayer(player)

  # if true_if_not_too_much, don't do the true calculus for combinations of 3 with a possible deck too big
  # to avoid to have choose(33, 3) = more than 500 combinations
  # but when the hand is 2 it is maximum 500 combinations
  # and when the hand is 3 but the deck is less than 12 it is max 455 combinations
  if (option_for_n_plus_1 == "true_if_not_too_much") {
    option_for_n_plus_1 <- ifelse(length(GetPlayerHand(game_state, other_player)) == 3 & length(game_state$deck > 12),
                                  "ponderated_scenario",
                                  "true_calculus")
  }

  if (option_for_n_plus_1 == "cheater" & game_state$turn <= 36) {
    expected_scores_n_plus_1 <- sapply(possible_decisions, function(dec) {
      game_state_after_dec <- PlayCard(game_state, player, dec)
      if (game_state$turn %% 6 == 0) game_state_after_dec <- DealPlayersCards(game_state_after_dec,
                                                                               starting_player = other_player)
      GiveExpectedScoreForADecision(game_state = game_state_after_dec,
                                    player = other_player,
                                    decision = OptimizedDecision(game_state_after_dec,
                                                                 other_player,
                                                                 if_equality_min_risk_of_scopa = T,
                                                                 cards_weight = cards_weight,
                                                                 primiera_weight = primiera_weight,
                                                                 sette_bello_weight = sette_bello_weight,
                                                                 denari_weight = denari_weight,
                                                                 scope_weight = scope_weight,
                                                                 explain = F),
                                    cards_weight = cards_weight,
                                    primiera_weight = primiera_weight,
                                    sette_bello_weight = sette_bello_weight,
                                    denari_weight = denari_weight,
                                    scope_weight = scope_weight)
    }
    )
  }

  if (option_for_n_plus_1 == "worst_case_scenario" & game_state$turn <= 36) {
    possible_hands <- GetPossibleCardsInHandOfAPlayer(game_state, other_player)
    expected_scores_n_plus_1 <- sapply(possible_decisions, function(dec) {
      game_state_after_dec <- PlayCard(game_state, player, dec)
      if (game_state$turn %% 6 == 0) game_state_after_dec <- DealPlayersCards(game_state_after_dec,
                                                                               starting_player = other_player)
      sapply(possible_hands, function(this_hand) {
        game_state_with_this_hand <- game_state_after_dec
        game_state_with_this_hand[[GetPlayerName(other_player)]]$hand <- this_hand
        GiveExpectedScoreForADecision(game_state = game_state_with_this_hand,
                                      player = other_player,
                                      decision = OptimizedDecision(game_state_with_this_hand,
                                                                   other_player,
                                                                   if_equality_min_risk_of_scopa = T,
                                                                   cards_weight = cards_weight,
                                                                   primiera_weight = primiera_weight,
                                                                   sette_bello_weight = sette_bello_weight,
                                                                   denari_weight = denari_weight,
                                                                   scope_weight = scope_weight,
                                                                   explain = F),
                                      cards_weight = cards_weight,
                                      primiera_weight = primiera_weight,
                                      sette_bello_weight = sette_bello_weight,
                                      denari_weight = denari_weight,
                                      scope_weight = scope_weight)
      }) %>% max() # end of the sapply on the possible_hands
    }) # end of sapply on the possible_decisions
  } # end of the if statement (worst_case_scenario)

  if (option_for_n_plus_1 == "random_play" & game_state$turn <= 36) {
    possible_hands <- GetPossibleCardsInHandOfAPlayer(game_state, other_player)
    expected_scores_n_plus_1 <- sapply(possible_decisions, function(dec) {
      game_state_after_dec <- PlayCard(game_state, player, dec)
      if (game_state$turn %% 6 == 0) game_state_after_dec <- DealPlayersCards(game_state_after_dec,
                                                                               starting_player = other_player)
      sapply(possible_hands, function(this_hand) {
        game_state_with_this_hand <- game_state_after_dec
        game_state_with_this_hand[[GetPlayerName(other_player)]]$hand <- this_hand
        GiveExpectedScoreForADecision(game_state = game_state_with_this_hand,
                                      player = other_player,
                                      decision = OptimizedDecision(game_state_with_this_hand,
                                                                   other_player,
                                                                   if_equality_min_risk_of_scopa = T,
                                                                   cards_weight = cards_weight,
                                                                   primiera_weight = primiera_weight,
                                                                   sette_bello_weight = sette_bello_weight,
                                                                   denari_weight = denari_weight,
                                                                   scope_weight = scope_weight,
                                                                   explain = F),
                                      cards_weight = cards_weight,
                                      primiera_weight = primiera_weight,
                                      sette_bello_weight = sette_bello_weight,
                                      denari_weight = denari_weight,
                                      scope_weight = scope_weight)
      }) %>% mean() # end of the sapply on the possible_hands
    }) # end of sapply on the possible_decisions
  } # end of the if statement (random_play)

  if (option_for_n_plus_1 == "ponderated_scenario" & game_state$turn <= 36) {
    possible_hands <- GetPossibleCardsInHandOfAPlayer(game_state, other_player)
    number_of_options <- length(possible_hands)
    size_of_hand <- length(GetPlayerHand(game_state, other_player))
    ponderations <- rep(1, number_of_options)
    if (size_of_hand == 2) ponderations <- c(rep(1.5, floor(number_of_options/2)),
                                             rep(1, number_of_options - floor(number_of_options/2)))
    if (size_of_hand == 3) ponderations <- c(rep(3, floor(number_of_options/3)),
                                             rep(2, floor(number_of_options/3)),
                                             rep(1, number_of_options - 2*floor(number_of_options/3)))
    expected_scores_n_plus_1 <- sapply(possible_decisions, function(dec) {
      game_state_after_dec <- PlayCard(game_state, player, dec)
      if (game_state$turn %% 6 == 0) game_state_after_dec <- DealPlayersCards(game_state_after_dec,
                                                                               starting_player = other_player)
      sapply(possible_hands, function(this_hand) {
        game_state_with_this_hand <- game_state_after_dec
        game_state_with_this_hand[[GetPlayerName(other_player)]]$hand <- this_hand
        GiveExpectedScoreForADecision(game_state = game_state_with_this_hand,
                                      player = other_player,
                                      decision = OptimizedDecision(game_state_with_this_hand,
                                                                   other_player,
                                                                   if_equality_min_risk_of_scopa = T,
                                                                   cards_weight = cards_weight,
                                                                   primiera_weight = primiera_weight,
                                                                   sette_bello_weight = sette_bello_weight,
                                                                   denari_weight = denari_weight,
                                                                   scope_weight = scope_weight,
                                                                   explain = F),
                                      cards_weight = cards_weight,
                                      primiera_weight = primiera_weight,
                                      sette_bello_weight = sette_bello_weight,
                                      denari_weight = denari_weight,
                                      scope_weight = scope_weight)
      }) %>% sort(decreasing = T) %>% weighted.mean(x = ., w = ponderations) # end of the sapply on the possible_hands
    }) # end of sapply on the possible_decisions
  } # end of the if statement (ponderated_scenario)

  if (option_for_n_plus_1 == "true_calculus" & game_state$turn <= 36) {
    possible_hands <- GetPossibleHandsOfAPlayer(game_state, other_player)
    expected_scores_n_plus_1 <- sapply(possible_decisions, function(dec) {
      game_state_after_dec <- PlayCard(game_state, player, dec)
      if (game_state$turn %% 6 == 0) game_state_after_dec <- DealPlayersCards(game_state_after_dec,
                                                                               starting_player = other_player)
      sapply(possible_hands, function(this_hand) {
        game_state_with_this_hand <- game_state_after_dec
        game_state_with_this_hand[[GetPlayerName(other_player)]]$hand <- this_hand
        GiveExpectedScoreForADecision(game_state = game_state_with_this_hand,
                                      player = other_player,
                                      decision = OptimizedDecision(game_state_with_this_hand,
                                                                   other_player,
                                                                   if_equality_min_risk_of_scopa = T,
                                                                   cards_weight = cards_weight,
                                                                   primiera_weight = primiera_weight,
                                                                   sette_bello_weight = sette_bello_weight,
                                                                   denari_weight = denari_weight,
                                                                   scope_weight = scope_weight,
                                                                   explain = F),
                                      cards_weight = cards_weight,
                                      primiera_weight = primiera_weight,
                                      sette_bello_weight = sette_bello_weight,
                                      denari_weight = denari_weight,
                                      scope_weight = scope_weight)
      }) %>% mean() # end of the sapply on the possible_hands
    }) # end of sapply on the possible_decisions
  } # end of the if statement (true calculus)

  expected_scores_n_n_plus_1 <- expected_scores - expected_scores_n_plus_1
  # take the highest expected scores
  optimized_decisions <- possible_decisions[expected_scores_n_n_plus_1 == max(expected_scores_n_n_plus_1)]

  # if there is only one choose it
  if (length(optimized_decisions) == 1) return(optimized_decisions[[1]])

  # otherwise, try to minimize the risk of scopa
  # start by getting the board value for each decision
  new_board_values <- sapply(optimized_decisions, function(dec)
    GetSumValuesOfCards(PlayCard(game_state = game_state, player = player, dec)$board))
  # order the decisions according to this value
  optimized_decisions <- optimized_decisions[order(new_board_values)]
  new_board_values <- sort(new_board_values)

  # if there is at least one decision leaving the board higher than 10, restrict to it and then play the lowest remaining board with []
  if (new_board_values[length(new_board_values)] > 10) {
    return(optimized_decisions[new_board_values > 10][[1]])
  }

  # otherwise just play for the lowest remaining board
  return(optimized_decisions[[1]])
}

OptimizedDecisionNPlus1_cheater <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                            sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "cheater",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
}

OptimizedDecisionNPlus1_worst_case_scenario <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                                        sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "worst_case_scenario",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
}

OptimizedDecisionNPlus1_random_play <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                                sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "random_play",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
}

OptimizedDecisionNPlus1_true_calculus <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                                  sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "true_calculus",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
}

OptimizedDecisionNPlus1_ponderated_scenario <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                                        sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "ponderated_scenario",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
}

OptimizedDecisionNPlus1_true_if_not_too_much <- function(game_state, player, cards_weight = 1, primiera_weight = 1,
                                                        sette_bello_weight = 1, denari_weight = 1, scope_weight = 1) {
  OptimizedDecisionNPlus1(game_state = game_state, player = player,
                          option_for_n_plus_1 = "true_if_not_too_much",
                          cards_weight = cards_weight, primiera_weight = primiera_weight,
                          sette_bello_weight = sette_bello_weight, denari_weight = denari_weight,
                          scope_weight = scope_weight)
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
