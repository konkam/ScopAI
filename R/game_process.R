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
#'
RunGame <- function(starting_player, DecisionFunction, seed = NULL) {
  game_state <- InitialiseGameState(seed = seed)
  current_player <- starting_player
  game_states <- list()
  game_states[[game_state$turn]] <- game_state
  while (length(game_state$deck) >= 6) { #Dealing 6 cards each time, stops when deck is empty
    # print(length(game_state$deck))
    if (game_state$turn > 1) game_state <- DealPlayersCards(game_state = game_state, starting_player = starting_player) # At first turn, cards have already been dealt with InitialiseGameState
    
    while (length(GetPlayerHand(game_state, current_player)) > 0) {
      game_state <- PlayCard(game_state = game_state,
                             player = current_player,
                             decision = DecisionFunction(game_state, current_player))
      current_player <- SwitchPlayer(current_player)
      game_states[[game_state$turn]] <- game_state
    }
  }
  # print(length(game_state$deck))
  game_state <- FinishGame(game_state = game_state)
  # At the end of the game, people have NAs in their hands
  return(list(score_player1 = GiveScoreFromStateForAPlayer(game_state, player = 1),
              score_player2 = GiveScoreFromStateForAPlayer(game_state, player = 2),
              game_history = game_states))
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
  game_state <- InitialiseGameState(seed = seed)
  current_player <- starting_player
  game_states <- list()
  game_states[[game_state$turn]] <- game_state
  while (length(game_state$deck) >= 6) { #Dealing 6 cards each time, stops when deck is empty
    # print(length(game_state$deck))
    if (game_state$turn > 1) game_state <- DealPlayersCards(game_state = game_state, starting_player = starting_player) # At first turn, cards have already been dealt with InitialiseGameState
    
    while (length(GetPlayerHand(game_state, current_player)) > 0) {
      if (current_player == 1) {
        game_state <- PlayCard(game_state = game_state,
                               player = current_player,
                               decision = DecisionFunction1(game_state, current_player))
      } else {
        game_state <- PlayCard(game_state = game_state,
                               player = current_player,
                               decision = DecisionFunction2(game_state, current_player))
      }

      current_player <- SwitchPlayer(current_player)
      game_states[[game_state$turn]] <- game_state
    }
  }
  # print(length(game_state$deck))
  game_state <- FinishGame(game_state = game_state)
  game_states[[game_state$turn]] <- game_state
  
  # At the end of the game, people have NAs in their hands
  return(list(score_player1 = GiveScoreFromStateForAPlayer(game_state, player = 1),
              score_player2 = GiveScoreFromStateForAPlayer(game_state, player = 2),
              game_history = game_states))
}


# If we want to understand more, we should detail the different scores for each fight (Denari, cards, primiera, scope, ...)
Compare2DecisionStrategies <- function(DecisionFunction1,
                                       DecisionFunction2,
                                       n_eval = 100,
                                       seed_used = 1:10,
                                       starting_players = 1:n_eval %% 2 + 1) {
  number_of_seeds <- length(seed_used)
  seeds <- rep(seed_used, ceiling(n_eval/number_of_seeds)) %>% sample()
  starting_players <- sample(starting_players)
  fights <- data.frame(test_number = 1:n_eval, score1 = NA, score2 = NA)
  for (i in 1:n_eval) {
    fights[i, -1] <- RunGameWithDifferentStrategies(starting_player = starting_players[i],
                                                    seed = seeds[i],
                                                    DecisionFunction1 = DecisionFunction1,
                                                    DecisionFunction2 = DecisionFunction2)[1:2] %>% unlist()

  }
  fights %>%
    dplyr::mutate(winner = ifelse(score1 > score2, 1, ifelse(score2 > score1, 2, 0)),
                  starting = starting_players,
                  seed_used = seeds) %>%
    dplyr::group_by(seed_used) %>%
    dplyr::summarise(number_of_eval = length(seed_used),
                     player1_started = sum(starting == 1),
                     player2_started = sum(starting == 2),
                     score1_sum = sum(score1),
                     score2_sum = sum(score2),
                     n_wins_for_1 = sum(score1 > score2),
                     n_wins_for_2 = sum(score2 > score1),
                     n_ties = sum(score1 == score2)) %T>% print(.) %>%
    dplyr::ungroup() %>%
    dplyr::summarise_all(list(~sum(.))) %>%
    dplyr::mutate(seed_used = "all")

}
# Compare2DecisionStrategies(RandomDecision, OptimizedDecision, seed_used = 11:20)

PlotTheEvolutionOfAGame <- function(game_states) {
  if (length(game_states) != 37) stop("the game_states should contain 37 items")
  score_df <- data.frame(turn = 1:37,
                         scopa = sapply(game_states, function(g_s) length(g_s$board) == 0),
                         settebello_1 = 1*sapply(game_states, function(g_s) "D7" %in% GetPlayerStack(g_s, 1)),
                         settebello_2 = 1*sapply(game_states, function(g_s) "D7" %in% GetPlayerStack(g_s, 2)),
                         p1_cards = sapply(game_states, function(g_s) CountCardsNumber(GetPlayerStack(g_s, 1))),
                         p2_cards = sapply(game_states, function(g_s) CountCardsNumber(GetPlayerStack(g_s, 2))),
                         p1_denari = sapply(game_states, function(g_s) CountDenariNumber(GetPlayerStack(g_s, 1))),
                         p2_denari = sapply(game_states, function(g_s) CountDenariNumber(GetPlayerStack(g_s, 2))),
                         p1_primiera = sapply(game_states, function(g_s) CountPrimiera(GetPlayerStack(g_s, 1))),
                         p2_primiera = sapply(game_states, function(g_s) CountPrimiera(GetPlayerStack(g_s, 2)))) %>%
    mutate(scopa = factor(((turn%%2)+1)*scopa, levels = 0:2),
           settebello_1 = settebello_1 - lag(settebello_1, default = 0),
           settebello_2 = settebello_2 - lag(settebello_2, default = 0),
           settebello = factor(2*settebello_2 + settebello_1, levels = 0:2))
  final_game_state <- game_states[[37]]
  scope_1 <- GetPlayerScope(final_game_state, 1)
  scope_2 <- GetPlayerScope(final_game_state, 2)
  stack_1 <- GetPlayerStack(final_game_state, 1)
  stack_2 <- GetPlayerStack(final_game_state, 2)
  denari_score_1 <- GiveDenariScoreForAPlayer(stack_1, stack_2)
  denari_score_2 <- GiveDenariScoreForAPlayer(stack_2, stack_1)
  cards_score_1 <- GiveCardsScoreForAPlayer(stack_1, stack_2)
  cards_score_2 <- GiveCardsScoreForAPlayer(stack_2, stack_1) 
  primiera_score_1 <- GivePrimieraScoreForAPlayer(stack_1, stack_2)
  primiera_score_2 <- GivePrimieraScoreForAPlayer(stack_2, stack_1)
  sb_1 <- GiveSetteBelloScoreForAPlayer(stack_1)
  sb_2 <- GiveSetteBelloScoreForAPlayer(stack_2)
  score_1 <- scope_1 + denari_score_1 + cards_score_1 + primiera_score_1 + sb_1
  score_2 <- scope_2 + denari_score_2 + cards_score_2 + primiera_score_2 + sb_2
  
  score_glue <- glue::glue("Final score is {score_1} - {score_2}")
  
  gg_base <- ggplot(score_df, aes(x = turn)) +
    theme_bw() +
    theme(legend.position = "none")
    
  
  gg_cards <- gg_base +
    geom_line(aes(y = p1_cards), color = "red") +
    geom_line(aes(y = p2_cards), color = "blue") +
    scale_y_continuous("Cards") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_cards, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_cards, label = "Player 2"),
                             color = "blue", nudge_y = 1) +
    geom_hline(yintercept = 20, linetype = "dashed") +
    geom_text(data = filter(score_df, turn == 3), aes(label = "Win cards point", y = 20.5)) +
    ggtitle(score_glue)
  
  gg_primiera <- gg_base +
    geom_line(aes(y = p1_primiera), color = "red") +
    geom_line(aes(y = p2_primiera), color = "blue") + 
    scale_y_continuous("Primiera") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_primiera, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_primiera, label = "Player 2"),
                             color = "blue", nudge_y = 1) 
  
  gg_denari <- gg_base +
    geom_line(aes(y = p1_denari), color = "red") +
    geom_line(aes(y = p2_denari), color = "blue") + 
    scale_y_continuous("Denari") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_denari, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_denari, label = "Player 2"),
                             color = "blue", nudge_y = 1) +
    geom_hline(yintercept = 5, linetype = "dashed") +
    geom_text(data = filter(score_df, turn == 3), aes(label = "Win Denari point", y = 5.2))
  
  gg_oners <- gg_base +
    scale_x_continuous(limits = c(1, 37)) +
    geom_segment(data = filter(score_df, settebello != 0), aes(y = 1, yend = 0, xend = turn, color = settebello)) +
    ggrepel::geom_label_repel(data = filter(score_df, settebello != 0),
                              aes(y = 1, label = "Settebello!", color = settebello), nudge_y = .5)  +
    scale_color_manual(limits = factor(1:2), values = c("red", "blue")) +
    scale_y_continuous("Scope / Settebello", labels = 0:2, breaks = 0:2, limits = c(0, 2))
  
  if (any(score_df$scopa != 0)) gg_oners <- gg_oners + 
    geom_segment(data = filter(score_df, scopa != 0), aes(y = 1, yend = 0, xend = turn, color = scopa)) +
    ggrepel::geom_label_repel(data = filter(score_df, scopa != 0),
                             aes(y = 1, label = "Scopa!", color = scopa), nudge_y = .5)

  return(list(cards_evolution = gg_cards,
              denari_evolution = gg_denari,
              primiera_evolution = gg_primiera,
              settebello_scope_evolution = gg_oners))
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
    if (length(game_state$board) == 0 & game_state$turn <= 36) game_state[[pla]]$scope <- game_state[[pla]]$scope + 1
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
