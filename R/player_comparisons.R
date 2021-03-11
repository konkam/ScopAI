#' Run several games among two players and extract scores
#' 
#' Games a run in parallel, and random numbers are dealt with correctly among the different workers. Note that the code does not run in parallel on Windows at the moment.
#'
#' @param DecisionFunction1 
#' @param DecisionFunction2 
#' @param seed 
#' @param n_pair_games 
#' @param n_procs 
#'
#' @return
#'
#' @examples
CompareTwoPlayers <- function(DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL, n_pair_games = 7 * 3, n_procs = 7) {
  if (!is.null(seed)) set.seed(seed, kind = "L'Ecuyer-CMRG")
  parallel::mclapply(
    X = 1:n_pair_games,
    FUN = function(x) dplyr::bind_rows(RunOneGame(starting_player = 1, DecisionFunction1, DecisionFunction2), RunOneGame(starting_player = 2, DecisionFunction1, DecisionFunction2)),
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
#'
#' @return
#'
#' @examples
#' ScopAI:::RunOneGame(starting_player = 1, DecisionFunction1 = ScopAI:::RandomDecision)
RunOneGame <- function(starting_player = 1, DecisionFunction1, DecisionFunction2 = DecisionFunction1, seed = NULL) {
  g <- RunGameWithDifferentStrategies(starting_player = starting_player, DecisionFunction1, DecisionFunction2 = DecisionFunction2, seed = seed)
  tibble::tibble(starting_player = starting_player, score_player_1 = g$score_player1, score_player_2 = g$score_player2)
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
