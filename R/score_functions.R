# use dictionnaries to get info from cards' names ------

#' Get The Colours Of Cards:
#' Use the dictionary to transform a set of cards into their colours ("D", "C", "B", "S)
#' @param cards
#'
#' @return
#'
#' @examples
#' GetColoursOfCards(c("D7", "C8"))
GetColoursOfCards <- function(cards) {
  sapply(deck_dict[cards], "[[", 1)
}

#' Get The Values Of Cards:
#' Use the dictionary to transform a set of cards into their values
#' @param cards
#'
#' @return
#'
#' @examples
#' GetValuesOfCards(c("D7", "C8"))
GetValuesOfCards <- function(cards) {
  sapply(deck_dict[cards], "[[", 2)
}

#' Get The Primiera Equivalent Of Cards:
#' Use the dictionary to transform a set of cards into their Primiera scores
#' @param cards
#'
#' @return
#'
#' @examples
#' GetPrimieraValuesOfCards(c("D7", "C8"))
GetPrimieraValuesOfCards <- function(cards) {
  if (length(cards) == 0) return(0)
  primiera_dict[GetValuesOfCards(cards)]
}

# manipulate cards ------
#' Subset One Colour In Cards:
#' Keep only the cards belonging to a given colour
#' @param cards
#'
#' @return
#'
#' @examples
#' SubsetOneColourInCards(c("D7", "C8"), to_subset = "C")
SubsetOneColourInCards <- function(cards, to_subset = "D") {
  cards[GetColoursOfCards(cards) == to_subset]
}

#' Subset One Value In Cards:
#' Keep only the cards with a given value (from 1 to 10)
#' @param cards
#'
#' @return
#'
#' @examples
#' SubsetOneValueInCards(c("D7", "C8"), to_subset = 8)
SubsetOneValueInCards <- function(cards, to_subset = 7) {
  cards[GetValuesOfCards(cards) == to_subset]
}

# count cards ----------
#' Count Denari Number:
#' Count the number of Denari cards within a set of cards
#' The higher number of Denari cards gets one point
#' @param cards
#'
#' @return
#'
#' @examples
#' CountDenariNumber(c("D7", "C8"))
CountDenariNumber <- function(cards) {
  length(SubsetOneColourInCards(cards))
}

#' Count Cards Number:
#' Count the number of cards within a set of cards
#' The higher number of cards gets one point
#' @param cards
#'
#' @return
#'
#' @examples
#' CountCardsNumber(c("D7", "C8"))
CountCardsNumber <- function(cards) {
  length(cards)
}

#' Count Seven Number:
#' Count the number of Seven cards within a set of cards.
#' It is usualy a good proxy of who will win the Primiera point
#' @examples
#' CountSevenNumber(c("D7", "C8"))
CountSevenNumber <- function(cards) {
  length(SubsetOneValueInCards(cards))
}

#' Count Primiera:
#' Compute the Primiera score which consist of the best hand of 4 cards,
#' each of one colour, with a specific scoring for each value (the 7 is the best)
#' If some colours are missing, they will contribute 0
#' The best Primiera score gets one point
#' @param cards
#'
#' @return
#'
#' @examples
#' CountPrimiera(c("D7", "C8", "S1", "B2", "D1))
CountPrimiera <- function(cards) {
  sum(max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "D"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "B"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "S"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "C"))))
}

# compute scores ------
#' Give Denari Score For Player 1
#' Who has the most Denari?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GiveDenariScoreForPlayer1 <- function(stack1, stack2) {
  if (CountDenariNumber(stack1) > CountDenariNumber(stack2)) return(1)
  return(0)
}

#' Give Denari Score For Player 2
#' Who has the most Denari?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GiveDenariScoreForPlayer2 <- function(stack1, stack2) {
  GiveDenariScoreForPlayer1(stack2, stack1)
}

#' Give Cards Score For Player 1
#' Who has the most cards?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GiveCardsScoreForPlayer1 <- function(stack1, stack2) {
  if (CountCardsNumber(stack1) > CountCardsNumber(stack2)) return(1)
  return(0)
}

#' Give Cards Score For Player 2
#' Who has the most cards?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GiveCardsScoreForPlayer2 <- function(stack1, stack2) {
  GiveCardsScoreForPlayer1(stack2, stack1)
}

#' Give Primiera Score For Player 1
#' Who has the best Primiera?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GivePrimieraScoreForPlayer1 <- function(stack1, stack2) {
  if (CountPrimiera(stack1) > CountPrimiera(stack2)) return(1)
  return(0)
}

#' Give Primiera Score For Player 2
#' Who has the best Primiera?
#' @param stack1
#' @param stack2
#'
#' @return
#'
#' @examples
GivePrimieraScoreForPlayer2 <- function(stack1, stack2) {
  GivePrimieraScoreForPlayer1(stack2, stack1)
}

#' Give Sette Bello Score For Player 1
#' Who has the sette bello (7 of Dinero)?
#' @param stack1
#'
#' @return
#'
#' @examples
GiveSetteBelloScoreForPlayer1 <- function(stack1) {
  if ("D7" %in% stack1) return(1)
  return(0)
}

#' Give Sette Bello Score For Player 2
#' Who has the sette bello (7 of Dinero)?
#' @param stack1
#'
#' @return
#'
#' @examples
GiveSetteBelloScoreForPlayer2 <- function(stack2) {
  if ("D7" %in% stack2) return(1)
  return(0)
}
