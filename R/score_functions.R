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
    GiveSetteBelloScoreForAPlayer(player_data$hand),
    GivePrimieraScoreForAPlayer(player_data$hand, other_data$hand),
    GiveCardsScoreForAPlayer(player_data$hand, other_data$hand),
    GiveDenariScoreForAPlayer(player_data$hand, other_data$hand)
  )
}
