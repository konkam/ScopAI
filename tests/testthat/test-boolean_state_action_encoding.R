test_that("Boolean state encoding works", {
  g <- InitialiseGameState()
  g2 <- ScopAI:::PlayCard(g, decision = list(play = "B2", take = c("B9", "B10")), player = 1)
  res = ScopAI:::EncodeStateAsBinaryVector(g2)
  expect_type(object = res, type = "double")
})

test_that("Boolean action encoding works", {
  g <- InitialiseGameState()
  decision <- ScopAI:::RandomDecision(g, player = 1)
  res = ScopAI:::EncodeActionAsBinaryVector(decision)
  expect_type(object = res, type = "double")
})
