test_that("Dealing cards execute correctly", {
  game_state = list(deck=deck, hand1 = list(), hand2 = list())
  g = DealCards(game_state, 1)
  expect_equal(length(g$hand1), 3)
  expect_equal(length(g$hand2), 3)
  expect_equal(length(g$deck), length(game_state$deck)-6)
})
