test_that("Always 10 Denari per deck", {
  expect_equal(CountDenariNumber(ordered_deck), 10)
})

test_that("Always 40 cards per deck", {
  expect_equal(CountCardsNumber(ordered_deck), 40)
})

test_that("Always 4 sevens per deck", {
  expect_equal(CountSevenNumber(ordered_deck), 4)
})

test_that("Primiera for all cards is 84 (4 sevens)", {
  expect_equal(CountPrimiera(ordered_deck), 84)
})

test_that("Expected Denari score is 0 if both players have the same number of Denaris", {
  expect_equal(GiveDenariExpectedScoreForAPlayer(c("C1"), c("C2")), 0)
})

test_that("Expected Denari score is 1 if player 1 has more than 5 Denaris", {
  expect_equal(GiveDenariExpectedScoreForAPlayer(paste0("D", 1:6), c("C2")), 1)
})

test_that("Expected Denari score is -1 if player 2 has more than 5 Denaris", {
  expect_equal(GiveDenariExpectedScoreForAPlayer(c("C2"), paste0("D", 1:6)), -1)
})

test_that("Expected cards score is 0 if both players have the same number of cards", {
  expect_equal(GiveCardsExpectedScoreForAPlayer(ordered_deck[1:5], ordered_deck[6:10]), 0)
})

test_that("Expected cards score is 1 if one player reaches 21 cards", {
  expect_equal(GiveCardsExpectedScoreForAPlayer(ordered_deck[1:21], ordered_deck[22:37]), 1)
})

test_that("Expected SetteBello score is -1 if player 2 has the sette bello", {
  expect_equal(GiveSetteBelloExpectedScoreForAPlayer(setdiff(ordered_deck, "D7"), "D7"), -1)
})
