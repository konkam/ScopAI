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

play_take_dict <- vector(mode = "list", length = length(ordered_deck))
names(play_take_dict) <- ordered_deck

# I've commented all the following part because when I install the package
# there is a problem with the function TakeableCardsOnBoardBruteForce not being defined (don't understand why)
# eventually we should run the commented script and then save the dictionary somewhere,
# and reload it when loading the package (it is 5 Mb large)

#' @include helper_functions.R
for (played_card in ordered_deck) {
  played_card_value <- GetValuesOfCards(played_card)
  # don't consider cards on the board that are higher than your card
  reduced_board <- deck_as_named_values[deck_as_named_values <= played_card_value]
  reduced_board <- reduced_board[names(reduced_board) != played_card]
  # partition on the highest card
  for (highest_card in 1:played_card_value) {
    board_highest_bool <- reduced_board == highest_card
    board_complement_bool <- reduced_board <= min(played_card_value - highest_card,
                                                  highest_card)
    board_considered <- reduced_board[board_highest_bool | board_complement_bool]
    boundary_of_combinations <- min(played_card_value - highest_card + 1, 6)
    combinations_with_this_highest_card <- TakeableCardsOnBoardBruteForce(
      played_card,
      names(board_considered),
      boundary = boundary_of_combinations)
    any(duplicated(combinations_with_this_highest_card))
    play_take_dict[[played_card]] <- c(play_take_dict[[played_card]],
                                       combinations_with_this_highest_card)
  }
  play_take_dict[[played_card]] <- c("none", play_take_dict[[played_card]])
  play_take_dict[[played_card]] <- play_take_dict[[played_card]] %>%
    .[!duplicated(.)]
  # there are duplicated because the partition is not perfect
  # indeed we don't force the takeables cards to include the highest
}
#
# # what if you take only into consideration Denari or not (use sort to avoid redondance)
# play_take_dict_denari_or_not <- lapply(play_take_dict, function(card)
#   unique(lapply(card, function(take) sort(sub("[BCS]", "no_D", take)))))
#
# # what if you take only into consideration the values
# play_take_dict_only_values <- lapply(play_take_dict, function(card)
#   unique(lapply(card, function(take) sort(sub("[BCDS]", "", take)))))
#
#
# if (F) {
#   # number of combinations play / take
#   sapply(play_take_dict, length) # max is 1699 for Re (10)
#   sapply(play_take_dict, length) %>% sum() # 16208
#
#   sapply(play_take_dict_denari_or_not, length) # max is 188 for Re (10)
#   sapply(play_take_dict_denari_or_not, length) %>% sum() # 2214
#
#   sapply(play_take_dict_only_values, length) # max is 35 for Re (10)
#   sapply(play_take_dict_only_values, length) %>% sum() # 532
# }
#
