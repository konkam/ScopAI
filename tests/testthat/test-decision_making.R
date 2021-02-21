test_that("Random decision works", {
  set.seed(1)
  for (i in 1:10){
    g = InitialiseGameState()
    decision = RandomDecision(g, player = 1)
    expect_true(GetValueOfCard(decision$play)==GetSumValuesOfCards(decision$take)|is.null(decision$take))
  }

  game = RunGame(seed = 1, starting_player = 1, DecisionFunction = RandomDecision)
  expect_false(any(is.na(game$score_player1)))
  expect_false(any(is.na(game$score_player2)))
  hands_player1 = game$game_history %>% lapply(FUN = function(x) x$player1$hand)
  expect_equal(unique(hands_player1 %>% lapply(length)) %>% unlist %>% sort, 0:3)
  expect_false(any(hands_player1 %>% unlist %>% is.na()))
  hands_player2 = game$game_history %>% lapply(FUN = function(x) x$player2$hand)
  expect_equal(unique(hands_player2 %>% lapply(length)) %>% unlist %>% sort, 0:3)
  expect_false(any(hands_player2 %>% unlist %>% is.na()))
})
