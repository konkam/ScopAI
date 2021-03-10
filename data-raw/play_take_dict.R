## code to prepare `play_take_dict` dataset goes here
create_play_take_dict = function(){
  play_take_dict <- vector(mode = "list", length = length(ordered_deck))
  names(play_take_dict) <- ordered_deck
  
  # eventually we should run the commented script and then save the dictionary somewhere,
  # and reload it when loading the package (it is 5 Mb large)
  
  for (played_card in ordered_deck) {
    played_card_value <- ScopAI:::GetValuesOfCards(played_card)
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
      combinations_with_this_highest_card <- ScopAI:::TakeableCardsOnBoardBruteForce(
        played_card,
        names(board_considered),
        boundary = boundary_of_combinations)
      any(duplicated(combinations_with_this_highest_card))
      play_take_dict[[played_card]] <- c(play_take_dict[[played_card]],
                                         combinations_with_this_highest_card)
    }
    play_take_dict[[played_card]] <- c(list(NULL), play_take_dict[[played_card]])
    play_take_dict[[played_card]] <- play_take_dict[[played_card]] %>%
      .[!duplicated(.)]
    # there are duplicated because the partition is not perfect
    # indeed we don't force the takeables cards to include the highest
  }
  return(play_take_dict)
}

play_take_dict = create_play_take_dict()

usethis::use_data(play_take_dict, overwrite = TRUE)
