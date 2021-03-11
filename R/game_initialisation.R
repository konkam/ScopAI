

#' Title
#'
#' @return
#'
ShuffleNewDeck <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  s_deck <- sample(ordered_deck, size = length(ordered_deck), replace = F)
    c(SortAccordingToGame(s_deck[1:3]), SortAccordingToGame(s_deck[4:6]), # first deal
      SortAccordingToGame(s_deck[7:10]), # first board
      SortAccordingToGame(s_deck[11:13]), SortAccordingToGame(s_deck[14:16]), # second deal
      SortAccordingToGame(s_deck[17:19]), SortAccordingToGame(s_deck[20:22]), # third deal
      SortAccordingToGame(s_deck[23:25]), SortAccordingToGame(s_deck[26:28]), # fourth deal
      SortAccordingToGame(s_deck[29:31]), SortAccordingToGame(s_deck[32:34]), # fifth deal
      SortAccordingToGame(s_deck[35:37]), SortAccordingToGame(s_deck[38:40])) # last deal
}

#' Title
#'
#' @param game_state A list containing the game state at this turn
#'
#' @return
#'
DealBoardCards <- function(game_state) {
  game_state$board <- game_state$deck[1:4]
  game_state$deck <- game_state$deck[5:length(game_state$deck)]
  return(game_state)
}

#' Initialise a game and distribute the deck
#'
#' @param seed Starting seed
#' @param starting_player 1 or 2 according to which player starts
#'
InitialiseGameState <- function(seed = NULL, starting_player = 1) {
  deck = ShuffleNewDeck(seed)
  InitialiseGameStateWithDeck(deck, starting_player)
}

#' Initialise a game and distribute the deck, given an input deck
#'
#' @param deck Input deck
#' @param starting_player 
#'
InitialiseGameStateWithDeck <- function(deck, starting_player = 1) {
  game_state <- list(
    deck = deck,
    player1 = list(hand = c(), stack = c(), scope = 0),
    player2 = list(hand = c(), stack = c(), scope = 0),
    turn = 1, last_taker = NULL
  ) %>%
    DealPlayersCards(starting_player = starting_player) %>%
    DealBoardCards()
  
  return(game_state)
}
