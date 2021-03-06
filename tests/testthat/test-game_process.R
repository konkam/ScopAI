test_that("Dealing cards executes correctly", {
  game_state <- list(deck = ordered_deck)
  g <- DealPlayersCards(game_state, 1)
  expect_equal(length(g$player1$hand), 3)
  expect_equal(length(g$player2$hand), 3)
  expect_equal(length(g$deck), length(game_state$deck) - 6)
  g2 <- DealBoardCards(g)
  expect_equal(length(g2$board), 4)
  expect_equal(length(g2$deck), length(g$deck) - 4)
})
# the following test has a problem since the game_state defined is not actually a game_state (it has no turn, no stacks, no hands...)
# test_that("Playing cards executes correctly", {
#   game_state <- list(deck = ordered_deck)
#   g <- DealPlayersCards(game_state, 1) %>% DealBoardCards()
#   g2 <- PlayCard(g, decision = list(play = "B2", take = c("B9", "B10")), player = 1)
#   expect_equal(length(g2$player1$hand), 2)
#   expect_equal(length(g2$player2$hand), 3)
#   expect_equal(length(g2$board), length(g$board) - 2)
#   expect_equal(g2$last_taker, 1)
#   g2 <- PlayCard(g, decision = list(play = "B2", take = NULL), player = 1)
#   expect_equal(length(g2$player1$hand), 2)
#   expect_equal(length(g2$player2$hand), 3)
#   expect_equal(length(g2$board), length(g$board) + 1)
# })
test_that("Finishing game executes correctly", {
  g <- InitialiseGameState()
  g2 <- PlayCard(g, decision = list(play = "B2", take = c("B9", "B10")), player = 1)
  g3 <- FinishGame(game_state = g2)
  expect_equal(length(g3$board), 0)
})
test_that("Game with similar player runs correctly", {
  game <- RunGame(seed = 1, starting_player = 1, DecisionFunction = RandomDecision)
  expect_false(any(is.na(game$score_player1)))
  expect_false(any(is.na(game$score_player2)))
  hands_player1 <- game$game_history %>% lapply(FUN = function(x) x$player1$hand)
  expect_equal(length(hands_player1), 37) #Right number of turns in a game
  expect_equal(unique(hands_player1 %>% lapply(length)) %>% unlist() %>% sort(), 0:3)
  expect_false(any(hands_player1 %>% unlist() %>% is.na()))
  hands_player2 <- game$game_history %>% lapply(FUN = function(x) x$player2$hand)
  expect_equal(unique(hands_player2 %>% lapply(length)) %>% unlist() %>% sort(), 0:3)
  expect_false(any(hands_player2 %>% unlist() %>% is.na()))
  
  for (t in game$game_history){ #The number of cards is conserved
    expect_equal(length(t$deck) + length(t$board) + length(t$player1$hand) + length(t$player1$stack) + length(t$player2$hand) + length(t$player2$stack), 40)
  }
  
  for (t in game$game_history){ #The number of cards is conserved and cards are not duplicated
    expect_equal(unique(c(t$deck, t$board, t$player1$hand, t$player1$stack, t$player2$hand, t$player2$stack)) %>% length(), 40)
  }
  
  expect_equal(game$game_history %>% lapply(FUN = function(x) x$deck %>% length) %>% unique() %>% unlist() %>% sort, seq(0,30, by = 6))
})
test_that("Game with two players runs correctly", {
  game <- RunGameWithDifferentStrategies(seed = 1, starting_player = 1, DecisionFunction1 = RandomDecision)
  expect_false(any(is.na(game$score_player1)))
  expect_false(any(is.na(game$score_player2)))
  hands_player1 <- game$game_history %>% lapply(FUN = function(x) x$player1$hand)
  expect_equal(length(hands_player1), 37) #Right number of turns in a game
  expect_equal(unique(hands_player1 %>% lapply(length)) %>% unlist() %>% sort(), 0:3)
  expect_false(any(hands_player1 %>% unlist() %>% is.na()))
  hands_player2 <- game$game_history %>% lapply(FUN = function(x) x$player2$hand)
  expect_equal(unique(hands_player2 %>% lapply(length)) %>% unlist() %>% sort(), 0:3)
  expect_false(any(hands_player2 %>% unlist() %>% is.na()))
  
  for (t in game$game_history){ #The number of cards is conserved
    expect_equal(length(t$deck) + length(t$board) + length(t$player1$hand) + length(t$player1$stack) + length(t$player2$hand) + length(t$player2$stack), 40)
  }
  
  for (t in game$game_history){ #The number of cards is conserved and cards are not duplicated
    expect_equal(unique(c(t$deck, t$board, t$player1$hand, t$player1$stack, t$player2$hand, t$player2$stack)) %>% length(), 40)
  }
  
  expect_equal(game$game_history %>% lapply(FUN = function(x) x$deck %>% length) %>% unique() %>% unlist() %>% sort, seq(0,30, by = 6))
  
  expect_equal(game$game_history %>% .[[2]] %>% .$player1 %>% .$hand %>% length, 2)
  expect_equal(game$game_history %>% .[[2]] %>% .$player2 %>% .$hand %>% length, 3)
  game <- RunGameWithDifferentStrategies(seed = 1, starting_player = 2, DecisionFunction1 = RandomDecision)
  expect_equal(game$game_history %>% .[[2]] %>% .$player1 %>% .$hand %>% length, 3)
  expect_equal(game$game_history %>% .[[2]] %>% .$player2 %>% .$hand %>% length, 2)
})