test_that("Dealing cards execute correctly", {
  game_state = list(deck=ordered_deck, hand1 = list(), hand2 = list())
  g = DealPlayersCards(game_state, 1)
  expect_equal(length(g$hand1), 3)
  expect_equal(length(g$hand2), 3)
  expect_equal(length(g$deck), length(game_state$deck)-6)
  g2 = DealBoardCards(g)
  expect_equal(length(g2$board), 4)
  expect_equal(length(g2$deck), length(g$deck)-4)
})
test_that("Playing cards execute correctly", {
  game_state = list(deck=ordered_deck, hand1 = list(), hand2 = list())
  g = DealPlayersCards(game_state, 1) %>% DealBoardCards
  g2 = PlayCard(g, decision = list(play = "B2", take = c("B9", "B10")), player = 1)
  expect_equal(length(g2$hand1), 2)
  expect_equal(length(g2$hand2), 3)
  expect_equal(length(g2$board), length(g2$board)-2)
})
