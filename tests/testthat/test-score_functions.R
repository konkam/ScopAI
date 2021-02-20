test_that("colours in a full deck", {
  expect_equal(as.vector(GetColorsOfCards(ordered_deck)),
               rep(colours, each = 10))
})
test_that("values in a full deck", {
  expect_equal(as.vector(GetValuesOfCards(ordered_deck)),
               rep(1:10, 4))
})

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
