test_that("colours in a full deck", {
  expect_equal(as.vector(GetColoursOfCards(ordered_deck)),
               rep(colours, each = 10))
})
test_that("values in a full deck", {
  expect_equal(as.vector(GetValuesOfCards(ordered_deck)),
               rep(1:10, 4))
})
test_that("sum of cards values work",{
  expect_equal(c("B4", "S9", "B1") %>% GetSumValuesOfCards, 14)
})
