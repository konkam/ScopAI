#' Get The Primiera Equivalent Of Cards:
#' Use the dictionary to transform a set of cards into their Primiera scores
#' @param cards
#'
#' GetPrimieraValuesOfCards(c("D7", "C8"))
GetPrimieraValuesOfCards <- function(cards) {
  if (length(cards) == 0) {
    return(0)
  }
  primiera_dict[GetValuesOfCards(cards)]
}

# count cards ----------
#' Count Denari Number:
#' Count the number of Denari cards within a set of cards
#' The higher number of Denari cards gets one point
#' @param cards
#'
#' @examples
#' ScopAI:::CountDenariNumber(c("D7", "C8"))
CountDenariNumber <- function(cards) {
  length(SubsetOneColourInCards(cards))
}

#' Count Cards Number:
#' Count the number of cards within a set of cards
#' The higher number of cards gets one point
#' @param cards
#'
#' @examples
#' ScopAI:::CountCardsNumber(c("D7", "C8"))
CountCardsNumber <- function(cards) {
  length(cards)
}

#' Count Seven Number:
#' Count the number of Seven cards within a set of cards.
#' It is usualy a good proxy of who will win the Primiera point
#' @examples
#' ScopAI:::CountSevenNumber(c("D7", "C8"))
CountSevenNumber <- function(cards) {
  length(SubsetOneValueInCards(cards))
}

#' Count Primiera:
#' Compute the Primiera score which consist of the best hand of 4 cards,
#' each of one colour, with a specific scoring for each value (the 7 is the best)
#' If some colours are missing, they will contribute 0
#' The best Primiera score gets one point
#' @param cards A vector
#'
#' @return
#'
#' @examples
#' ScopAI:::CountPrimiera(c("D7", "C8", "S1", "B2", "D1"))
CountPrimiera <- function(cards) {
  sum(
    max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "D"))),
    max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "B"))),
    max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "S"))),
    max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "C")))
  )
}

# compute scores ------
#' Give Denari Score For A Player
#' Who has the most Denari?
#' @param stack_player A vector
#' @param stack_other A vector
#'
GiveDenariScoreForAPlayer <- function(stack_player, stack_other) {
  if (CountDenariNumber(stack_player) > CountDenariNumber(stack_other)) {
    return(1)
  }
  return(0)
}


#' Give Cards Score For A Player
#' Who has the most cards?
#'
#' @inheritParams GiveDenariScoreForAPlayer
GiveCardsScoreForAPlayer <- function(stack_player, stack_other) {
  if (CountCardsNumber(stack_player) > CountCardsNumber(stack_other)) {
    return(1)
  }
  return(0)
}

#' Give Primiera Score For A Player
#' Who has the best Primiera?
#' @inheritParams GiveDenariScoreForAPlayer
GivePrimieraScoreForAPlayer <- function(stack_player, stack_other) {
  if (CountPrimiera(stack_player) > CountPrimiera(stack_other)) {
    return(1)
  }
  return(0)
}

#' Give Sette Bello Score For A Player
#' Who has the sette bello (7 of Dinero)?
#' @param stack_player A vector
#'
GiveSetteBelloScoreForAPlayer <- function(stack_player) {
  if ("D7" %in% stack_player) {
    return(1)
  }
  return(0)
}

#' Give Score From State For A Player
#' For a given game state, compute the score for a player
#' @param game_state A list containing the game state at this turn
#' @param player 1 or 2 depending on the player
#'
GiveScoreFromStateForAPlayer <- function(game_state, player = 1) {
  other_player <- player %% 2 + 1
  player_data <- game_state[[GetPlayerName(player)]]
  other_data <- game_state[[GetPlayerName(other_player)]]
  sum(
    player_data$scope,
    GiveSetteBelloScoreForAPlayer(player_data$stack),
    GivePrimieraScoreForAPlayer(player_data$stack, other_data$stack),
    GiveCardsScoreForAPlayer(player_data$stack, other_data$stack),
    GiveDenariScoreForAPlayer(player_data$stack, other_data$stack)
  )
}


# scores as expected values -------------
#' Give Binomial Probability Of Having At Least K Successes
#' Helper funcion for expected scores
#' @param k an integer
#' @param n an integer
#' @param p a real between 0 and 1 (probability)
GiveBinomialProbaAtLeastK <- function(k, n, p = 1/2) {
  if (k > n) return(0)
  sum(sapply(k:n, function(i) choose(n, i))*p^k*(1-p)^(n-k))
}

#' Give Expected Denari Score For A Player
#' What is the expected denari score?
#' Should be 1 if at least 6 Denari for player 1,
#' -1 if at least 6 Denari for player 2,
#' 0 if each player has 5 Denari,
#' otherwise assume an equal probability for each player for the remaining Denaris
#' @param stack_player A vector
#' @param stack_other A vector
#'
GiveDenariExpectedScoreForAPlayer <- function(stack_player, stack_other) {
  D1 <- CountDenariNumber(stack_player)
  D2 <- CountDenariNumber(stack_other)
  GiveBinomialProbaAtLeastK(6 - D1, 10 - (D1 + D2)) - GiveBinomialProbaAtLeastK(6 - D2, 10 - (D1 + D2))
}

#' Give Expected Cards Score For A Player
#' What is the expected cards score?
#' Should be 1 if at least 21 cards for player 1,
#' -1 if at least 21 cards for player 2,
#' 0 if each player has 20 cards,
#' otherwise assume an equal probability for each player for the remaining cards
#' @param stack_player A vector
#' @param stack_other A vector
#'
GiveCardsExpectedScoreForAPlayer <- function(stack_player, stack_other) {
  C1 <- length(stack_player)
  C2 <- length(stack_other)
  GiveBinomialProbaAtLeastK(21 - C1, 40 - (C1 + C2)) - GiveBinomialProbaAtLeastK(21 - C2, 40 - (C1 + C2))
}

#' Give Relative Primiera For A Given Colour
#' Assume that all remaining cards of the colour can be given at each player with probability 1/2
#' Only focus on cards whose primieras are higher than at least one stack, otherwise it is neutral for this colour
#'
#' Explanation for the sapply found in the function:
#' Imagine that you draw a tree of decision for the n remaining cards of this colour, ordered by growing primiera value:
#' there are 2^n trajectories giving each card to player 1 or player 2
#' half of results will give the highest card to player1 (last branches of the tree) -> proba 1/2
#' among the remaining trajectories, half will give the second highest primiera to player1 -> proba 1/4
#' among the remaining trajectories, half will give the third highest to player 1 -> proba 1/8
#' etc... until only one trajectory of drawing only the smallest cards -> proba 1/2^n
#' and also one trajectory with 0 cards for player 1 -> also proba 1/2^n
#' you need to also take into account the fact that you already have a card of value max_player
#' -> therefore add the max in the formula
#' the sapply formula calculates the expected value with the coeeficient in 2^(k-1) and a division by 2^n for the mean
#' max(0, k-1) in the formula is to count 1/2^n for the trajectory with 0 cards for the player
#'
#' @param stack_player a set of cards
#' @param stack_other a set of cards
#' @param colour among c("B", "C", "D", "S")
GiveRelativePrimieraForAGivenColour <- function(stack_player, stack_other, colour = "D") {
  stack_player <- SubsetOneColourInCards(stack_player, colour)
  stack_other <- SubsetOneColourInCards(stack_other, colour)
  max_player <- max(GetPrimieraValuesOfCards(stack_player))
  max_other <- max(GetPrimieraValuesOfCards(stack_other))
  remaining_primiera_of_interest <- setdiff(paste0(colour, 1:10), c(stack_player, stack_other)) %>%
    GetPrimieraValuesOfCards() %>%
    .[. > min(max_player, max_other)] %>% # the cards with values below are neutral for the expected primiera
    sort()
  cards_to_be_decided <- length(remaining_primiera_of_interest)
  if (cards_to_be_decided == 0) return(max_player - max_other)

  expected_primiera_player <- sapply(0:cards_to_be_decided, function(k)
    2^(max(0, k-1))/(2^cards_to_be_decided)*max(remaining_primiera_of_interest[k],
                                                max_player)) %>%
    sum()

  expected_primiera_other <- sapply(0:cards_to_be_decided, function(k)
    2^(max(0, k-1))/(2^cards_to_be_decided)*max(remaining_primiera_of_interest[k],
                                                max_other)) %>%
    sum()

  return(expected_primiera_player - expected_primiera_other)
}

GivePrimieraExpectedScoreForAPlayer <- function(stack_player, stack_other) {
  stack_player_B <- SubsetOneColourInCards(stack_player, "B")
  stack_other_B <- SubsetOneColourInCards(stack_other, "B")
  max_player_B <- max(GetPrimieraValuesOfCards(stack_player_B))
  max_other_B <- max(GetPrimieraValuesOfCards(stack_other_B))
  remaining_primiera_of_interest_B <- setdiff(B_cards, c(stack_player_B, stack_other_B)) %>%
    GetPrimieraValuesOfCards() %>%
    .[. > min(max_player_B, max_other_B)] %>% # the cards with values below are neutral for the expected primiera
    sort()
  cards_B <- length(remaining_primiera_of_interest_B)

  stack_player_C <- SubsetOneColourInCards(stack_player, "C")
  stack_other_C <- SubsetOneColourInCards(stack_other, "C")
  max_player_C <- max(GetPrimieraValuesOfCards(stack_player_C))
  max_other_C <- max(GetPrimieraValuesOfCards(stack_other_C))
  remaining_primiera_of_interest_C <- setdiff(C_cards, c(stack_player_C, stack_other_C)) %>%
    GetPrimieraValuesOfCards() %>%
    .[. > min(max_player_C, max_other_C)] %>% # the cards with values below are neutral for the expected primiera
    sort()
  cards_C <- length(remaining_primiera_of_interest_C)

  stack_player_D <- SubsetOneColourInCards(stack_player, "D")
  stack_other_D <- SubsetOneColourInCards(stack_other, "D")
  max_player_D <- max(GetPrimieraValuesOfCards(stack_player_D))
  max_other_D <- max(GetPrimieraValuesOfCards(stack_other_D))
  remaining_primiera_of_interest_D <- setdiff(D_cards, c(stack_player_D, stack_other_D)) %>%
    GetPrimieraValuesOfCards() %>%
    .[. > min(max_player_D, max_other_D)] %>% # the cards with values below are neutral for the expected primiera
    sort()
  cards_D <- length(remaining_primiera_of_interest_D)

  stack_player_S <- SubsetOneColourInCards(stack_player, "S")
  stack_other_S <- SubsetOneColourInCards(stack_other, "S")
  max_player_S <- max(GetPrimieraValuesOfCards(stack_player_S))
  max_other_S <- max(GetPrimieraValuesOfCards(stack_other_S))
  remaining_primiera_of_interest_S <- setdiff(S_cards, c(stack_player_S, stack_other_S)) %>%
    GetPrimieraValuesOfCards() %>%
    .[. > min(max_player_S, max_other_S)] %>% # the cards with values below are neutral for the expected primiera
    sort()
  cards_S <- length(remaining_primiera_of_interest_S)

  # pick_combinations <- expand.grid(B_pick = 0:cards_B,
  #                                  C_pick = 0:cards_C,
  #                                  D_pick = 0:cards_D,
  #                                  S_pick = 0:cards_S,
  #                                  stringsAsFactors = F)
  #
  # cards_to_be_decided <- sum(cards_B, cards_C, cards_D, cards_S)
  #
  # sapply(1:nrow(pick_combinations), function(i) {
  #   B_k <- pick_combinations[i, "B_pick"]
  #   C_k <- pick_combinations[i, "C_pick"]
  #   D_k <- pick_combinations[i, "D_pick"]
  #   S_k <- pick_combinations[i, "S_pick"]
  #   B_k_correct <- max(0, B_k - 1) # count 1 trajectory for 0 cards
  #   C_k_correct <- max(0, C_k - 1)
  #   D_k_correct <- max(0, D_k - 1)
  #   S_k_correct <- max(0, S_k - 1)
  #
  #   expected_primiera_player <- 2^(B_k_correct + C_k_correct + D_k_correct + S_k_correct)/(2^cards_to_be_decided)*sum(
  #     max(remaining_primiera_of_interest_B[B_k],
  #         max_player_B),
  #     max(remaining_primiera_of_interest_C[C_k],
  #         max_player_C),
  #     max(remaining_primiera_of_interest_D[D_k],
  #         max_player_D),
  #     max(remaining_primiera_of_interest_S[S_k],
  #         max_player_S))
  #   expected_primiera_other
  # }) %>%
  #   sum()

  if (cards_B + cards_C + cards_D + cards_S == 0) return(sign(CountPrimiera(stack_player) > CountPrimiera(stack_other)))
  # look for all combinations of repartitions between player 1 and 2 of the remaining cards

  B_combinations <- lapply(1:cards_B, function(i) 1:2)
  names(B_combinations) <- paste0("B", 1:cards_B)
  if (cards_B == 0) {
    B_combinations <- 10
  }

  C_combinations <- lapply(1:cards_C, function(i) 1:2)
  names(C_combinations) <- paste0("C", 1:cards_C)
  if (cards_C == 0) {
    C_combinations <- 10
  }

  D_combinations <- lapply(1:cards_D, function(i) 1:2)
  names(D_combinations) <- paste0("D", 1:cards_D)
  if (cards_D == 0) {
    D_combinations <- 10
  }

  S_combinations <- lapply(1:cards_S, function(i) 1:2)
  names(S_combinations) <- paste0("S", 1:cards_S)
  if (cards_S == 0) {
    S_combinations <- 10
  }

  pick_combinations <- expand.grid(c(B_combinations, C_combinations, D_combinations, S_combinations),
                                   stringsAsFactors = F)

  sapply(1:nrow(pick_combinations), function(i) {
    pick_B <- pick_combinations[i, grep("B", colnames(pick_combinations))]
    pick_C <- pick_combinations[i, grep("C", colnames(pick_combinations))]
    pick_D <- pick_combinations[i, grep("D", colnames(pick_combinations))]
    pick_S <- pick_combinations[i, grep("S", colnames(pick_combinations))]

    pick_B_other <- 3-pick_B
    pick_C_other <- 3-pick_C
    pick_D_other <- 3-pick_D
    pick_S_other <- 3-pick_S

    picked_B_value <- ifelse(test = any(pick_B == 1),
                             yes = remaining_primiera_of_interest_B[max(which(pick_B == 1))],
                             no = 0)
    other_B_value <- ifelse(test = any(pick_B_other == 2),
                             yes = remaining_primiera_of_interest_B[max(which(pick_B_other == 2))],
                             no = 0)

    picked_C_value <- ifelse(test = any(pick_C == 1),
                             yes = remaining_primiera_of_interest_C[max(which(pick_C == 1))],
                             no = 0)
    other_C_value <- ifelse(test = any(pick_C_other == 2),
                            yes = remaining_primiera_of_interest_C[max(which(pick_C_other == 2))],
                            no = 0)

    picked_D_value <- ifelse(test = any(pick_D == 1),
                             yes = remaining_primiera_of_interest_D[max(which(pick_D == 1))],
                             no = 0)
    other_D_value <- ifelse(test = any(pick_D_other == 2),
                            yes = remaining_primiera_of_interest_D[max(which(pick_D_other == 2))],
                            no = 0)

    picked_S_value <- ifelse(test = any(pick_S == 1),
                             yes = remaining_primiera_of_interest_S[max(which(pick_S == 1))],
                             no = 0)
    other_S_value <- ifelse(test = any(pick_S_other == 2),
                            yes = remaining_primiera_of_interest_S[max(which(pick_S_other == 2))],
                            no = 0)

    primiera_player_this_pick <- sum(
      max(max_player_B, picked_B_value),
      max(max_player_C, picked_C_value),
      max(max_player_D, picked_D_value),
      max(max_player_S, picked_S_value)
    )

    primiera_other_this_pick <- sum(
      max(max_other_B, other_B_value),
      max(max_other_C, other_C_value),
      max(max_other_D, other_D_value),
      max(max_other_S, other_S_value)
    )

    return(sign(primiera_player_this_pick - primiera_other_this_pick))

  }) %>%
    sum()/nrow(pick_combinations)
}



#' Give Expected Sette Bello Score For A Player
#' Consider the differential score between player 1 and 2
#' Should be 1 if player 1 has Sette Bello,
#' -1 if player 2 has it,
#' 0 otherwise,
#' @param stack_player A vector
#' @param stack_other A vector
#'
GiveSetteBelloExpectedScoreForAPlayer <- function(stack_player, stack_other) {
  GiveSetteBelloScoreForAPlayer(stack_player) - GiveSetteBelloScoreForAPlayer(stack_other)
}
