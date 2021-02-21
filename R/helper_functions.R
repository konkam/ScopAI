
#' Title
#'
#' @param letter
#'
#' @return
#' @export
#'
#' @examples
GiveFullColourName = function(letter) {
  return(colours_dict[letter])
}

SwitchPlayer = function(player){
  return(ifelse(test = player==1, yes = 2, no = 1))
}
GetPlayerName = function(player){
  return(ifelse(test = player==1, yes = "player1", no = "player2"))
}

GetPlayerHand = function(game_state, player){
  game_state[[GetPlayerName(player)]]$hand
}

AllSubsets = function(cards){
  cards %>%
    (function(cards) lapply(1:length(cards), FUN = function(n) combn(cards, m = n, simplify = F))) %>%
    purrr::flatten() %>%
    c(list(c()))
}


#' Title
#' Found on http://rsnippets.blogspot.com/2012/04/generating-all-subsets-of-set.html
#' @param set
#'
#' @return
#' @export
#'
#' @examples
AllSubsetsFast <- function(set) {
  n <- length(set)
  bin <- vector(mode = "list", length = n)
  for (i in 1L:n) {
    bin[[i]] <- rep.int(c(rep.int(F, 2L ^ (i - 1L)),
                          rep.int(T, 2L ^ (i - 1L))),
                        2L ^ (n - i))
  }
  apply(do.call(cbind, bin), 1L, function(x) { set[x] } )
}

TakeableCardsOnBoardBruteForce = function(card, board){
  board_subsets = AllSubsets(board)
  val = GetValueOfCard(card)
  subsets_sum_value = board_subsets %>% sapply(GetSumValuesOfCards)
  allowed_subsets_mask = subsets_sum_value==val
  return(board_subsets[allowed_subsets_mask])
}


LookWhichCardsYouCanGetOnBoard <- function(one_card, board) {
  one_card_value <- GetValuesOfCards(one_card)
  board_with_values <- setNames(object = GetValuesOfCards(board),
                                nm = board)
  # don't consider cards on the board that are higher than your card
  board_with_values <- board_with_values[board_with_values <= one_card_value]
  take_opportunities <- vector()
  for (i in 1:length(board_with_values)) {
    for (combination  in combn(board_with_values, i, simplify = F)) {
      if (sum(combination) == one_card_value) {
        take_opportunities[[i]] <- names(combination)
      }
    }
  }
  return(take_opportunities)
}

