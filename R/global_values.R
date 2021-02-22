# Define values that will be the same for everyone
colours <- c("B", "C", "D", "S")

colours_dict <- setNames(
  object = c("Spade", "Coppe", "Denari", "Bastoni"),
  nm = c("S", "C", "D", "B")
)

values_dict <- setNames(
  object = 8:10,
  nm = c("Fante", "Cavallo", "Re")
)

primiera_dict <- setNames(
  object = c(16, 12, 13, 14, 15, 18, 21, 10, 10, 10),
  nm = 1:10
)

deck_dict <- expand.grid(val = 1:10, col = colours, stringsAsFactors = F) %>%
  (function(df) {
    mapply(
      FUN = function(x, y) list(colour = x, value = y),
      df$col, df$val, SIMPLIFY = F
    )
  })
names(deck_dict) <- paste0(rep(colours, each = 10), rep(1:10, 4))

ordered_deck <- names(deck_dict)

deck_as_named_values <- setNames(
  object = GetValuesOfCards(ordered_deck),
  nm = ordered_deck
)

sum_dict <- vector(mode = "list", length = 5)
sum_dict[[1]] <- list(one_card = list(1))
sum_dict[[2]] <- list(
  one_card = list(2),
  two_cards = list(c(1, 1))
)
sum_dict[[3]] <- list(
  one_card = list(3),
  two_cards = list(c(1, 2)),
  three_cards = list(c(1, 1, 1))
)
sum_dict[[4]] <- list(
  one_card = list(4),
  two_cards = list(c(1, 3), c(2, 2)),
  three_cards = list(c(1, 1, 2)),
  four_cards = list((c(1, 1, 1, 1)))
)
sum_dict[[5]] <- list(
  one_card = list(5),
  two_cards = list(c(1, 4), c(2, 3)),
  three_cards = list(c(1, 1, 3), c(1, 2, 2)),
  four_cards = list((c(1, 1, 1, 2)))
)

# PickACardBasedOnItsValue <- function(cards, value) {
#   wanted_GetValuesOfCards(cards)
#
# }
# loop on card value
# played_card <- "D4"
# played_card_value <- GetValuesOfCards(played_card)
# TakeableCardsOnBoardBruteForce(card = "D4", board = c("D3", "C1", "C2", "C4"))
# # don't consider cards on the board that are higher than your card
# reduced_board <- deck_as_named_values[deck_as_named_values <= played_card_value]
# reduced_board <- reduced_board[names(reduced_board) != played_card]
# dict_for_this_value <- sum_dict[[played_card_value]]
# for (n_cards in 1:length(dict_for_this_value)) {
#   combinations_for_n <- dict_for_this_value[[n_cards]]
#   for (i in 1:length(combinations_for_n)) {
#     value_combination <- combinations_for_n[[i]]
#     reduced_board_for_this_combination <- reduced_board[reduced_board %in% value_combination]
#     for (value in value_combination) {
#       # dfg
#     }
#     reduced_board[reduced_board]
#   }
#   reduced_board[dict_for_this_value]
# }
# take_opportunities <- vector()
# for (i in 1:min(played_card_value, 6)) {
#   for (combination  in combn(board_with_values, i, simplify = F)) {
#     if (sum(combination) == one_card_value) {
#       take_opportunities[[i]] <- names(combination)
#     }
#   }
# }
#


# hello world

