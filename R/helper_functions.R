
#' Title
#'
#' @param letter First letter of the colour
#'
#' @return
#'
GiveFullColourName <- function(letter) {
  return(colours_dict[letter])
}

SwitchPlayer <- function(player) {
  return(ifelse(test = player == 1, yes = 2, no = 1))
}
GetPlayerName <- function(player) {
  return(ifelse(test = player == 1, yes = "player1", no = "player2"))
}

GetPlayerHand <- function(game_state, player) {
  game_state[[GetPlayerName(player)]]$hand
}

GetPlayerStack <- function(game_state, player) {
  game_state[[GetPlayerName(player)]]$stack
}

GetPlayerScope <- function(game_state, player) {
  game_state[[GetPlayerName(player)]]$scope
}

AllSubsetsWithCombn <- function(cards, boundary = length(cards)) {
  if (length(cards) == 0) {
    return(return(character(0)))
  }
  else {
    purrr::flatten(lapply(1:boundary, FUN = function(n) combn(cards, m = n, simplify = F))) %>%
      c(list(character(0)))
  }
}


#' Title
#' Found on http://rsnippets.blogspot.com/2012/04/generating-all-subsets-of-set.html, credits to Bogumił Kamiński
#' @param set A vector from which to generate all subsets
#'
#'
AllSubsetsWithGenerator <- function(set) {
  n <- length(set)
  if (n == 0) {
    return(character(0))
  }
  bin <- vector(mode = "list", length = n)
  for (i in 1L:n) {
    bin[[i]] <- rep.int(
      c(
        rep.int(F, 2L^(i - 1L)),
        rep.int(T, 2L^(i - 1L))
      ),
      2L^(n - i)
    )
  }
  apply(do.call(cbind, bin), 1L, function(x) {
    set[x]
  })
}

#' Computes all subsets of cards from a list of cards
#'
#' Switches between the two functions according to the number of cards, as benchmarking with inputs such as c("B1", "B2", "B3", "B4", "B5", "B6", "B9", "B10", "B11") showed that speed differs among the two methods
#'
#' @param cards A vector of cards from which to generate all subsets
#'
#'
AllSubsets <- function(cards, boundary = length(cards)) {
  n <- length(cards)
  if (n == 0) {
    return(character(0))
  }
  else if (n <= 6) {
    return(AllSubsetsWithGenerator(cards))
  }
  else {
    return(AllSubsetsWithCombn(cards, boundary))
  }
}



TakeableCardsOnBoardBruteForce <- function(card, board, boundary = length(board)) {
  val <- GetValueOfCard(card)
  boardvals <- GetValuesOfCards(board)
  if (val %in% boardvals) {
    return(board[val == boardvals] %>% as.list())
  }
  else {
    board_subsets <- AllSubsets(board[boardvals < val], boundary)
    subsets_sum_value <- board_subsets %>% sapply(GetSumValuesOfCards)
    allowed_subsets_mask <- subsets_sum_value == val
    if (all(!allowed_subsets_mask)) {
      return(list(NULL))
    }
    else {
      return(board_subsets[allowed_subsets_mask])
    }
  }
}

#' Takeable Cards On Board Optimized
#' Take advantage of the play / take dictionary to give a quick list of
#' possible takes considering a played card and the board
#' Warning: the order of the possible takes may be different from the order of the board since it uses the dictionnary
#'
#' @param card
#' @param board
#'
#'
TakeableCardsOnBoardOptimized <- function(card, board) {
  board_values <- GetValuesOfCards(board)
  card_value <- GetValueOfCard(card)

  # check if one card of the board matches the value of the card played
  matching_value <- which(board_values == card_value)
  if (length(matching_value)) {
    return(as.list(board[matching_value]))
  }
  # restrict to cards with lower values
  board <- board[board_values < card_value]
  if (length(board) == 0) return("none")

  # restrict to possible takes with this played card (with the dictionnary)
  restricted_takes <- play_take_dict[[card]]
  for (i in length(restricted_takes):1) { # count backwards because you remove some parts of the list
    if (any(!restricted_takes[[i]] %in% board)) restricted_takes <- restricted_takes[-i]
  }
  if (length(restricted_takes) == 0) return("none")
  return(restricted_takes)
}

# It is indeed quicker -> look at this example with the played being a 10 and the board being the whole deck without the 10s
# TakeableCardsOnBoardOptimized("D10", setdiff(ordered_deck, paste0(c("C", "D", "B", "S"), 10)))
# TakeableCardsOnBoardBruteForce("D10", setdiff(ordered_deck, paste0(c("C", "D", "B", "S"), 10)), boundary = 6)

#' List All Possible Decisions
#'
#' @param game_state
#' @param player
#'
ListAllPossibleDecisions <- function(game_state = InitialiseGameState(seed = 1),
                                     player = 1) {
  cards <- GetPlayerHand(game_state = game_state, player = player)
  board <- game_state$board
  possible_decision <- list()
  for (card in cards) { # maybe it can be optimized with vectorization instead of list
    possible_decision <- c(possible_decision, lapply(TakeableCardsOnBoardOptimized(card, board), function(l)
      list(Play = card, Take = l)))
  }
  return(possible_decision)
}


