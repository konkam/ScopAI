#' Title
#'
#' @param starting_player 1 or 2 according to which player starts
#' @param DecisionFunction A function DecisionFunction(game_state, current_player) which returns a list (play = card_played, take = cards_taken)
#' @param seed Specify a seed if you want a repeatable game, by default the RNG is not seeded.
#' 
#' In the future, DecisionFunction(game_state, current_player) may be modified to take history into account as DecisionFunction(game_states, current_player)
#'
#' The game state is a list with elements:
#' deck (card to draw)
#' hand1
#' hand2
#' stack1
#' stack2
#' board
#' turn
#' last_taker
#'
#' @export
RunGame <- function(starting_player, DecisionFunction, seed = NULL) {
  RunGameWithDifferentStrategies(starting_player = starting_player, DecisionFunction, DecisionFunction, seed = seed) 
    }

# Below is the previous version in case I made a mistake !!
# RunGame <- function(starting_player, DecisionFunction, seed = NULL) {
#   game_state <- InitialiseGameState(seed = seed)
#   current_player <- starting_player
#   game_states <- list()
#   game_states[[game_state$turn]] <- game_state
#   while (length(game_state$deck) >= 6 && game_state$turn <= 36) {
#     # print(length(game_state$deck))
#     
#     while (length(GetPlayerHand(game_state, current_player)) > 0) {
#       game_state <- PlayCard(game_state = game_state,
#                              player = current_player,
#                              decision = DecisionFunction(game_state, current_player))
#       current_player <- SwitchPlayer(current_player)
#       game_states[[game_state$turn]] <- game_state
#     }
#     game_state <- DealPlayersCards(game_state = game_state, starting_player = starting_player)
#   }
#   # print(length(game_state$deck))
#   game_state <- FinishGame(game_state = game_state)
#   # At the end of the game, people have NAs in their hands
#   return(list(score_player1 = GiveScoreFromStateForAPlayer(game_state, player = 1),
#               score_player2 = GiveScoreFromStateForAPlayer(game_state, player = 2),
#               game_history = game_states))
# }

RunGameWithDifferentStrategies <- function(starting_player = 1,
                                           DecisionFunction1,
                                           DecisionFunction2 = DecisionFunction1,
                                           seed = NULL) {
  deck <- ShuffleNewDeck(seed)
  RunGameWithDeckWithDifferentStrategies(starting_player = starting_player, DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, deck = deck)
}

RunGameWithDeckWithDifferentStrategies <- function(starting_player = 1,
                                                   DecisionFunction1,
                                                   DecisionFunction2 = DecisionFunction1, deck = ShuffleNewDeck(seed)) {
  game_state <- InitialiseGameStateWithDeck(deck = deck, starting_player = starting_player)
  current_player <- starting_player
  game_states <- list()
  game_states[[game_state$turn]] <- game_state
  while (length(game_state$deck) >= 6) { # Dealing 6 cards each time, stops when deck is empty
    # print(length(game_state$deck))
    if (game_state$turn > 1) game_state <- DealPlayersCards(game_state = game_state, starting_player = starting_player) # At first turn, cards have already been dealt with InitialiseGameState

    while (length(GetPlayerHand(game_state, current_player)) > 0) {
      if (current_player == 1) {
        game_state <- PlayCard(
          game_state = game_state,
          player = current_player,
          decision = DecisionFunction1(game_state, current_player)
        )
      } else {
        game_state <- PlayCard(
          game_state = game_state,
          player = current_player,
          decision = DecisionFunction2(game_state, current_player)
        )
      }

      current_player <- SwitchPlayer(current_player)
      game_states[[game_state$turn]] <- game_state
    }
  }
  # print(length(game_state$deck))
  game_state <- FinishGame(game_state = game_state)
  game_states[[game_state$turn]] <- game_state

  # At the end of the game, people have NAs in their hands
  return(list(
    score_player1 = GiveScoreFromStateForAPlayer(game_state, player = 1),
    score_player2 = GiveScoreFromStateForAPlayer(game_state, player = 2),
    game_history = game_states
  ))
}

#' Title
#'
#' @param game_state A list containing the game state at this turn
#' @param starting_player 1 or 2 according to which player starts
#'
#' @return
#'
DealPlayersCards <- function(game_state, starting_player) {
  if (starting_player != 1 && starting_player != 2) {
    stop(print("starting_player should be 1 or 2"))
  }
  if (length(game_state$deck) < 6) {
    stop("The deck does not have enough cards left to deal")
  }
  if (starting_player == 1) {
    game_state$player1$hand <- c(game_state$player1$hand, game_state$deck[1:3])
    game_state$player2$hand <- c(game_state$player2$hand, game_state$deck[4:6])
  }
  else {
    game_state$player1$hand <- c(game_state$player1$hand, game_state$deck[4:6])
    game_state$player2$hand <- c(game_state$player2$hand, game_state$deck[1:3])
  }
  if (length(game_state$deck) == 6) {
    game_state$deck <- c()
  }
  game_state$deck <- game_state$deck[7:length(game_state$deck)]
  return(game_state)
}

#' Title
#'
#' @param game_state A list containing the game state at this turn
#' @param player 1 or 2 according to which player plays
#' @param decision A list (play = card_played, take = cards_taken)
#' @param check_for_validity TRUE or FALSE. Adds a check for validity of the played card
#'
#' A decision is a list(play = card played, a single card, take = cards taken.)
#'
#' @return
#'
PlayCard <- function(game_state, player, decision, check_for_validity =  F, check_card_numbers = F) {
  if (player != 1 && player != 2) {
    stop(print("starting_player should be 1 or 2"))
  }

  if (length(GetPlayerHand(game_state, player)) < length(GetPlayerHand(game_state, SwitchPlayer(player))) &
      check_card_numbers) {
    stop(print("the current player should have at least as much cards as the other player"))
  }

  if (check_for_validity) IsADecisionValid(game_state, player, decision)

  pla <- GetPlayerName(player)
  game_state[[pla]]$hand <- game_state[[pla]]$hand[game_state[[pla]]$hand != decision$play]
  if (length(decision$take) > 0) {
    game_state[[pla]]$stack <- c(game_state[[pla]]$stack, decision$play, decision$take) %>% SortAccordingToGame()
    game_state$board <- game_state$board[!game_state$board %in% decision$take]
    game_state$last_taker <- player
    if (length(game_state$board) == 0 & game_state$turn < 36) game_state[[pla]]$scope <- game_state[[pla]]$scope + 1
  } else {
    game_state$board <- c(game_state$board, decision$play) %>% SortAccordingToGame()
  }

  game_state$turn <- game_state$turn + 1
  return(game_state)
}

PlayARandomCard <- function(game_state, player) {
  PlayCard(game_state, player, RandomDecision(game_state, player))
}


FinishGame <- function(game_state) {
  if (game_state$last_taker == 1) {
    game_state$player1$stack <- c(game_state$player1$stack, game_state$board)
  }
  else if (game_state$last_taker == 2) {
    game_state$player2$stack <- c(game_state$player2$stack, game_state$board)
  }
  else {
    stop(print("game_state$last_taker should be 1 or 2"))
  }
  game_state$board <- NULL
  return(game_state)
}

CheckAtFinish <- function(game_state) {
  if (game_state$turn != 36) {
    stop("Trying to finish the game too early")
  }
  if (length(game_state$deck) != 0) {
    stop("Trying to finish but the deck is not empty")
  }
}
