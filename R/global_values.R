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
B_cards <- ordered_deck[1:10]
C_cards <- ordered_deck[11:20]
D_cards <- ordered_deck[21:30]
S_cards <- ordered_deck[31:40]

deck_as_named_values <- setNames(
  object = GetValuesOfCards(ordered_deck),
  nm = ordered_deck
)


# what if you take only into consideration Denari or not (use sort to avoid redondance)
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
#   sapply(play_take_dict, length) # max is 1698 for Re (10)
#   sapply(play_take_dict, length) %>% sum() # 16184
#
#   sapply(play_take_dict_denari_or_not, length) # max is 187 for Re (10)
#   sapply(play_take_dict_denari_or_not, length) %>% sum() # 2190
#
#   sapply(play_take_dict_only_values, length) # max is 34 for Re (10)
#   sapply(play_take_dict_only_values, length) %>% sum() # 508
# }

