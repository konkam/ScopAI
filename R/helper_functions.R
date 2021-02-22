
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
  if(n==0) return(character(0))
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
AllSubsets = function(cards, boundary = length(cards)){
  n = length(cards)
  if (n == 0) {
    return(character(0))
  }
  else if(n<=6){
    return(AllSubsetsWithGenerator(cards))
  }
  else{
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
    board_subsets <- AllSubsets(board[boardvals<val], boundary)
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


LookWhichCardsYouCanGetOnBoard <- function(one_card, board) {
  one_card_value <- GetValuesOfCards(one_card)
  board_with_values <- setNames(
    object = GetValuesOfCards(board),
    nm = board
  )
  # don't consider cards on the board that are higher than your card
  board_with_values <- board_with_values[board_with_values <= one_card_value]
  take_opportunities <- vector()
  for (i in 1:length(board_with_values)) {
    for (combination in combn(board_with_values, i, simplify = F)) {
      if (sum(combination) == one_card_value) {
        take_opportunities[[i]] <- names(combination)
      }
    }
  }
  return(take_opportunities)
}
