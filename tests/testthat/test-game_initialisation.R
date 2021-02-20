test_that("initialisation works", {
  game_state = InitialiseGameState(seed = 1, starting_player = 1)
  expect_equal(game_state$deck %>% length(), 30)
  expect_equal(game_state$hand1 %>% length(), 3)
  expect_equal(game_state$hand2 %>% length(), 3)
  expect_equal(game_state$board %>% length(), 4)
})
