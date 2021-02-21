test_that("give me that vitamin D", {
  expect_equal(as.vector(GiveFullColourName("D")), "Denari")
})

test_that("get the right hand", {
  g = InitialiseGameState()
  expect_equal(GetPlayerHand(game_state = g, player = 1))
})
