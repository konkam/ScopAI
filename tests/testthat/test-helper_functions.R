test_that("give me that vitamin D", {
  expect_equal(as.vector(GiveFullColourName("D")), "Denari")
})

test_that("get the right hand", {
  g = InitialiseGameState(seed = 1)
  expect_equal(GetPlayerHand(game_state = g, player = 1), c("B4", "S9", "B1"))
  expect_equal(GetPlayerHand(game_state = g, player = 2), c("S4", "D3", "C4"))
})

test_that("Combination functions work",{
  for(n in 1:4){
    cards = sample(ordered_deck, size = n)
    expect_equal(AllSubsets(cards) %>% length, 2^n)
  }
})
