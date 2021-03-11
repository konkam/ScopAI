#' Run several games among two players and extract scores
#'
#' Games a run in parallel, and random numbers are dealt with correctly among the different workers. Note that the code does not run in parallel on Windows at the moment.
#'
#' @param DecisionFunction1
#' @param DecisionFunction2
#' @param seed
#' @param n_pair_games
#' @param n_procs
#' @param detailed_output
#'
#' @return
#'
#' @examples
RunManyGamesparallel <- function(DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, n_games = 7 * 4, n_procs = 7, detailed_output = FALSE) {
  if (is_odd(n_games)) stop("Better to provide an even number of games to offset possible effects of play order advantage")
  n_games <- n_games / 2
  if (!is.null(seed)) set.seed(seed, kind = "L'Ecuyer-CMRG")
  parallel::mclapply(
    X = 1:n_games,
    FUN = function(x) dplyr::bind_rows(RunOneGame(starting_player = 1, DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, detailed_output = detailed_output), RunOneGame(starting_player = 2, DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, detailed_output = detailed_output)),
    mc.set.seed = T,
    mc.preschedule = T,
    mc.cores = n_procs
  ) %>%
    dplyr::bind_rows()
}

#' Title
#'
#' @param DecisionFunction1
#' @param DecisionFunction2
#' @param seed
#' @param n_pair_games
#' @param n_procs
#' @param detailed_output
#'
RunManyPairedGamesparallel <- function(DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, n_pair_games = 7 * 3, n_procs = 7, detailed_output = FALSE) {
  if (!is.null(seed)) set.seed(seed, kind = "L'Ecuyer-CMRG")
  parallel::mclapply(
    X = 1:n_pair_games,
    FUN = function(x) {
      RunTwoGamesSameDeck(DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, seed = NULL, detailed_output = detailed_output) %>% dplyr::mutate(deck_id = x)
    },
    mc.set.seed = T,
    mc.preschedule = T,
    mc.cores = n_procs
  ) %>%
    dplyr::bind_rows()
}

#' Run one game among two players and extract scores
#'
#' @param starting_player
#' @param DecisionFunction1
#' @param DecisionFunction2
#' @param seed
#' @param detailed_output
#' @param deck
#'
#' @return
#'
#' @examples
#' ScopAI:::RunOneGame(starting_player = 1, DecisionFunction1 = ScopAI:::RandomDecision)
RunOneGame <- function(starting_player = 1, DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, detailed_output = FALSE, deck = ShuffleNewDeck(seed)) {
  g <- RunGameWithDeckWithDifferentStrategies(starting_player = starting_player, DecisionFunction1, DecisionFunction2 = DecisionFunction2, deck = deck)
  if (detailed_output) {
    detail_score_player_1 <- GiveScoreDetailFromStateForAPlayer(g$game_history %>% last(), player = 1)
    detail_score_player_2 <- GiveScoreDetailFromStateForAPlayer(g$game_history %>% last(), player = 2)
    tibble::tibble(
      starting_player = starting_player, score_player_1 = g$score_player1, score_player_2 = g$score_player2,
      scope_player1 = detail_score_player_1$scope, settebello_player_1 = detail_score_player_1$settebello, primiera_player1 = detail_score_player_1$primiera, cards_player1 = detail_score_player_1$cards, denari_player1 = detail_score_player_1$denari,
      scope_player2 = detail_score_player_2$scope, settebello_player_2 = detail_score_player_2$settebello, primiera_player2 = detail_score_player_2$primiera, cards_player2 = detail_score_player_2$cards, denari_player2 = detail_score_player_2$denari
    )
  }
  else {
    tibble::tibble(starting_player = starting_player, score_player_1 = g$score_player1, score_player_2 = g$score_player2)
  }
}

RunTwoGamesSameDeck <- function(DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, detailed_output = FALSE) {
  deck <- ShuffleNewDeck(seed)
  res <- dplyr::bind_rows(
    RunOneGame(starting_player = 1, DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, seed = NULL, detailed_output = detailed_output),
    RunOneGame(starting_player = 2, DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, seed = NULL, detailed_output = detailed_output),
  )
  tibble::tibble(player = c(1, 2), 
                 score_starting = c(res$score_player_1[1], res$score_player_2[2]), 
                 score_finishing = c(res$score_player_1[2], res$score_player_2[1]),
                 victory_starting = c(res$score_player_1[1] > res$score_player_2[1], res$score_player_2[2] > res$score_player_1[2]),
                 victory_finishing = c(res$score_player_1[2] > res$score_player_2[2], res$score_player_2[1] > res$score_player_1[1])
                 )
}


CompareTwoPlayers <- function(DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, n_games = 10, paired_comparison = TRUE, n_procs = 7, detailed_output = F) {
  if (is_odd(n_games)) stop("Better to provide an even number of games to offset possible effects of play order advantage")

  n_games <- n_games / 2
  matches <- RunManyPairedGamesparallel(DecisionFunction1 = DecisionFunction1, DecisionFunction2 = DecisionFunction2, seed = seed, n_procs = n_procs, detailed_output = detailed_output, n_pair_games = n_games)


  return(tibble::tibble(
    player = c(1, 2),
    average_starting_score = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = mean(score_starting)) %>% .$s,
    average_starting_score_infCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(score_starting, probs = 0.025)) %>% .$s,
    average_starting_score_supCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(score_starting, probs = 0.975)) %>% .$s,
    average_finishing_score = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = mean(score_finishing)) %>% .$s,
    average_finishing_score_infCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(score_finishing, probs = 0.025)) %>% .$s,
    average_finishing_score_supCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(score_finishing, probs = 0.975)) %>% .$s,
    victories_starting = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = mean(victory_starting)) %>% .$s,
    victories_starting_infCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(victory_starting, probs = 0.025)) %>% .$s,
    victories_starting_supCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(victory_starting, probs = 0.975)) %>% .$s,
    victories_finishing = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = mean(victory_finishing)) %>% .$s,
    victories_finishing_infCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(victory_finishing, probs = 0.025)) %>% .$s,
    victories_finishing_supCI = matches %>% dplyr::group_by(player) %>% dplyr::summarise(s = quantile(victory_finishing, probs = 0.975)) %>% .$s,
  ))
}



# If we want to understand more, we should detail the different scores for each fight (Denari, cards, primiera, scope, ...)
Compare2DecisionStrategies <- function(DecisionFunction1,
                                       DecisionFunction2,
                                       n_eval = 100,
                                       seed_used = 1:10,
                                       starting_players = 1:n_eval %% 2 + 1) {
  number_of_seeds <- length(seed_used)
  seeds <- rep(seed_used, ceiling(n_eval / number_of_seeds)) %>% sample()
  starting_players <- sample(starting_players)
  fights <- data.frame(test_number = 1:n_eval, score1 = NA, score2 = NA)
  for (i in 1:n_eval) {
    fights[i, -1] <- RunGameWithDifferentStrategies(
      starting_player = starting_players[i],
      seed = seeds[i],
      DecisionFunction1 = DecisionFunction1,
      DecisionFunction2 = DecisionFunction2
    )[1:2] %>% unlist()
  }
  fights %>%
    dplyr::mutate(
      winner = ifelse(score1 > score2, 1, ifelse(score2 > score1, 2, 0)),
      starting = starting_players,
      seed_used = seeds
    ) %>%
    dplyr::group_by(seed_used) %>%
    dplyr::summarise(
      number_of_eval = length(seed_used),
      player1_started = sum(starting == 1),
      player2_started = sum(starting == 2),
      score1_sum = sum(score1),
      score2_sum = sum(score2),
      n_wins_for_1 = sum(score1 > score2),
      n_wins_for_2 = sum(score2 > score1),
      n_ties = sum(score1 == score2)
    ) %T>% print(.) %>%
    dplyr::ungroup() %>%
    dplyr::summarise_all(list(~ sum(.))) %>%
    dplyr::mutate(seed_used = "all")
}
# Compare2DecisionStrategies(RandomDecision, OptimizedDecision, seed_used = 11:20)
