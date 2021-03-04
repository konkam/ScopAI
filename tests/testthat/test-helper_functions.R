test_that("give me that vitamin D", {
  expect_equal(as.vector(GiveFullColourName("D")), "Denari")
})

test_that("get the right hand", {
  g <- InitialiseGameState(seed = 1)
  expect_equal(GetPlayerHand(game_state = g, player = 1), c("B1", "B4", "S9"))
  expect_equal(GetPlayerHand(game_state = g, player = 2), c("C4", "D3", "S4"))
})

test_that("Combination functions work", {
  for (n in 1:4) {
    cards <- sample(ordered_deck, size = n)
    expect_equal(AllSubsets(cards) %>% length(), 2^n)
  }
})

test_that("Cards allowed for the taking", {
  expect_equal(TakeableCardsOnBoardBruteForce("B5", c("B4", "S9", "B1")) %>% unlist(), c("B4", "B1"))
  expect_equal(TakeableCardsOnBoardBruteForce("B9", c("B4", "S9", "B1")) %>% unlist(), c("S9"))
  expect_equal(TakeableCardsOnBoardBruteForce("B9", c("D9", "S9", "B1")) %>% unlist(), c("D9", "S9"))
  expect_equal(TakeableCardsOnBoardBruteForce("B10", c("D9", "S9", "B2")) %>% unlist(), NULL)
  expect_equal(TakeableCardsOnBoardBruteForce("B4", c("D1", "S1", "B1", "C1")) %>% unlist(), c("D1", "S1", "B1", "C1"))
})

test_that("Cards allowed for the taking with the quick function", {
  expect_equal(TakeableCardsOnBoardOptimized("B5", c("B4", "S9", "B1")) %>% unlist(), c("B1", "B4"))
  expect_equal(TakeableCardsOnBoardOptimized("B9", c("B4", "S9", "B1")) %>% unlist(), c("S9"))
  expect_equal(TakeableCardsOnBoardOptimized("B9", c("D9", "S9", "B1")) %>% unlist(), c("D9", "S9"))
  expect_equal(TakeableCardsOnBoardOptimized("B10", c("D9", "S9", "B2")) %>% unlist(), NULL)
  expect_equal(TakeableCardsOnBoardOptimized("B4", c("D1", "S1", "B1", "C1")) %>% unlist(), c("B1", "C1", "D1", "S1"))
})

test_that("The function to switch players works", {
  expect_equal(SwitchPlayer(1), 2)
  expect_equal(SwitchPlayer(2), 1)
})

test_that("For any game, there are 5456 possibilities for a player's hand if you don't know it", {
  expect_equal(length(GetPossibleHandOfAPlayer(InitialiseGameState(), 2)), 5456)
})
