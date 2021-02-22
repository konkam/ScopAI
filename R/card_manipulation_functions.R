# use dictionnaries to get info from cards' names ------

#' Get The Colours Of Cards:
#' Use the dictionary to transform a set of cards into their colours ("D", "C", "B", "S)
#' @param cards
#'
#' @return
#'
#' @examples
#' ScopAI:::GetColoursOfCards(c("D7", "C8"))
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
#' ScopAI:::GetValuesOfCards(c("D7", "C8"))
GetValuesOfCards <- function(cards) {
  sapply(deck_dict[cards], "[[", 2) # Maybe faster
  # sapply(X = cards, GetValueOfCard)
}

GetValueOfCard <- function(card) {
  deck_dict[[card]][["value"]]
}

GetSumValuesOfCards <- function(cards) {
  if (length(cards) == 0) {
    return(0)
  } else {
    return(GetValuesOfCards(cards) %>% sum())
  }
}

# manipulate cards ------
#' Subset One Colour In Cards:
#' Keep only the cards belonging to a given colour
#' @param cards
#'
#' @return
#'
#' @examples
#' ScopAI:::SubsetOneColourInCards(c("D7", "C8"), to_subset = "C")
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
#' ScopAI:::SubsetOneValueInCards(c("D7", "C8"), to_subset = 8)
SubsetOneValueInCards <- function(cards, to_subset = 7) {
  cards[GetValuesOfCards(cards) == to_subset]
}
