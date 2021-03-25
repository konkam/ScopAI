
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(glue)
library(dplyr)
library(ScopAI)

# code to be run only once, when the app is loaded -----
# directories ------

# load data -------


# Define UI ----
ui <- fluidPage(
  titlePanel("Shiny Scopa"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 6,
               radioButtons("decision_type", h4("Choose your opponent"),
                            choices = list("Random Player" = "Random Player",
                                           "Optimizer" = "Optimizer",
                                           "Anticipator" = "Anticipator"),
                            selected = "Optimizer"),
               conditionalPanel(condition = "input.decision_type == 'Anticipator'",
                                radioButtons("anticipator_parameter", h4("How to anticipate"),
                                             choices = list("Cheater" = "cheater",
                                                            "Worst Case Scenario" = "worst_case_scenario",
                                                            "Select card at random" = "random_play",
                                                            "True calculus" = "true_calculus",
                                                            "Ponderated Scenario" = "ponderated_scenario",
                                                            "True If Not Too Much" = "true_if_not_too_much"),
                                             selected = "cheater")),
               textInput("seed_entered", h4("Choose a seed"), value = 1, width = "100%"),
               radioButtons("starting_player", h4("Choose starting player"),
                            choices = list("You" = 1,
                                           "Your opponent" = 2),
                            selected = 2),
               actionButton("newgame", "New Game", class = "btn-lg"),
               sliderInput("pixel_cards",
                           label = "Zoom on cards",
                           value = 50,
                           min = 0,
                           max = 100)
        ) # end of first column
      ), # end of fluidrow
      
    ), # end of sidebarPanel
    mainPanel(
      useShinyjs(),
      h4(textOutput("game_parameters"), style = "color:grey"),
      h4(textOutput("display_hand_other"), style = "color:red"),
      fluidRow(
        column(2, uiOutput("opponent_hand_0"),
               uiOutput("opponent_hand_1"),
               uiOutput("opponent_hand_2"),
               uiOutput("opponent_hand_3")),
        column(2, uiOutput("opponent_play"))),
      h4(em(textOutput("display_last_action_opponent"), style = "color:red")),
      h4(textOutput("display_board"), style = "color:green"),
      fluidRow(
        column(1, uiOutput("board_1")),
        column(1, uiOutput("board_2")),
        column(1, uiOutput("board_3")),
        column(1, uiOutput("board_4")),
        column(1, uiOutput("board_5")),
        column(1, uiOutput("board_6")),
        column(1, uiOutput("board_7")),
        column(1, uiOutput("board_8")),
        column(1, uiOutput("board_9")),
        column(1, uiOutput("board_10"))),
      fluidRow(column(2, h4(textOutput("remaining_deck_text"))),
               column(2, uiOutput("remaining_deck")),
               column(5, h4(em(textOutput("display_deal_info"))))),
      h4(textOutput("display_hand"), style = "color:blue"),
      fluidRow(
        column(1, uiOutput("your_hand_1")),
        column(1, uiOutput("your_hand_2")),
        column(1, uiOutput("your_hand_3")),
        column(1, uiOutput("your_hand_0"))),
      h3(textOutput("end_game"), style = "color:brown"),
      h4(em(textOutput("display_your_last_action"), style = "color:blue")),
      fluidRow(
        column(1, actionButton("next_play", "Next", class = "btn-lg")),
        column(6, radioButtons("decision",
                   label = h3("Choice", style = "color:blue"),
                   choices = list("This is a mock" = "mock"), 
                   selected = "mock")))
    ) # end of mainPanel
  ) # end of sidebarLayout
) # end of fluidpage

# Define server logic ----
server <- function(input, output, session) {
  # get the seed
  values <- reactiveValues(
    current_player = NULL,  
    game_state = NULL,  
    wait_the_next = NULL,
    last_action_other = NULL,
    last_action_you = NULL,
    deal_action = NULL,
    endgame = NULL,
    your_decision = NULL,
    opponent_decision = NULL,
    you_played = NULL,
    opponent_played = NULL,
    parameters = NULL,
    pixel_for_cards = NULL
  )
  
  # New game button is clicked (this also runs when the app first loads)
  observeEvent(input$newgame, ignoreNULL = FALSE, {
    values$current_player <- input$starting_player
    values$game_state <- InitialiseGameState(seed = input$seed_entered, starting_player = values$current_player)
    values$wait_the_next <- 1
    values$deal_action <- "The first cards have been dealt. Click on the Next button to start playing"
    values$last_action_other <- ""
    values$last_action_you <- ""
    values$your_decision <- NULL
    values$opponent_decision <- NULL
    values$opponent_played <- F
    values$you_played <- F
    values$endgame <- ""
    values$pixel_for_cards <- input$pixel_cards
    showElement("next_play", time = 0)
    showElement("board_1", time = 0)
    showElement("board_2", time = 0)
    showElement("board_3", time = 0)
    showElement("board_4", time = 0)
    showElement("board_5", time = 0)
    showElement("board_6", time = 0)
    showElement("board_7", time = 0)
    showElement("board_8", time = 0)
    showElement("board_9", time = 0)
    showElement("board_10", time = 0)
    hideElement("decision", time = 0)
    hideElement("opponent_play", time = 0)
    updateRadioButtons(session, "decision",
                       choices = list("This is a mock" = "mock"),
                       selected = "mock")
    values$parameters <- glue("Parameters of the game: your opponent is the {input$decision_type}, 
    the seed used is {input$seed_entered},
               the first player is {c('you', 'your opponent')[as.numeric(input$starting_player)]}.
               Press New Game if you want to change the parameters")
  })
  
  observeEvent(input$next_play, {
    if (values$wait_the_next == 1) {
      values$wait_the_next <- 2
    }
    if (values$wait_the_next == 2) {
      values$wait_the_next <- 3
    }
  })
  
  observeEvent(input$next_play, {
    if (values$wait_the_next == 1) {
      values$wait_the_next <- 2
    }
    if (values$wait_the_next == 2) {
      values$wait_the_next <- 3
    }
  })
  
  observeEvent(input$next_play, ignoreNULL = FALSE, {
    if (values$wait_the_next == 3) {
      showElement("decision", time = 0)
      if (values$game_state$turn == 37) {
        hideElement("decision", time = 0)
        hideElement("next_play", time = 0)
        hideElement("opponent_play", time = 0)
        hideElement("board_1", time = 0)
        hideElement("board_2", time = 0)
        hideElement("board_3", time = 0)
        hideElement("board_4", time = 0)
        hideElement("board_5", time = 0)
        hideElement("board_6", time = 0)
        hideElement("board_7", time = 0)
        hideElement("board_8", time = 0)
        hideElement("board_9", time = 0)
        hideElement("board_10", time = 0)
        values$endgame <- glue("This was the last card! The remaining board {ShowCards(values$game_state$board)}
        went to {c('you', 'your opponent')[values$game_state$last_taker]}. The party is over.
        Your score is {ScopAI:::GiveScoreDetailFromStateForAPlayerForHuman(values$game_state, 1)} for a total of
        {ScopAI:::GiveScoreFromStateForAPlayer(values$game_state, 1)}.
        Your opponent score is {ScopAI:::GiveScoreDetailFromStateForAPlayerForHuman(values$game_state, 2)} for a total of
        {ScopAI:::GiveScoreFromStateForAPlayer(values$game_state, 2)}.
                               Press New Game to play again.")
        values$game_state <- ScopAI:::FinishGame(values$game_state)
        values$last_action_other <- ""
        values$last_action_you <- ""
        values$deal_action <- ""
        values$wait_the_next <- 4 # at the end of the game, a next is doing nothing
      } else { # else of the if about turn 37
        if (values$game_state$turn %in% (6*1:5 + 1) & values$deal_action == "") {
          hideElement("decision", time = 0)
          values$game_state <- ScopAI:::DealPlayersCards(game_state = values$game_state, starting_player = input$starting_player)
          values$deal_action <- "New cards have been dealt"
          if (values$game_state$turn == 31) values$deal_action <- "The last cards have been dealt!"
          values$last_action_other <- ""
          values$last_action_you <- ""
        } else { # else of if about the need to deal new cards
          if (values$current_player == 2) {
            updateRadioButtons(session, "decision",
                               choices = list("This is a mock" = "mock"),
                               selected = "mock")
            hideElement("decision", time = 0)
            values$last_action_you <- ""
            values$deal_action <- " " # should not be "", otherwise if turn is a multiple of 6 you will deal cards twice
            
            if (!values$opponent_played) {
              if (input$decision_type == "Random Player") values$opponent_decision <- ScopAI:::RandomDecision(values$game_state, 2)
              if (input$decision_type == "Optimizer") values$opponent_decision <- ScopAI:::OptimizedDecision(values$game_state, 2)
              if (input$decision_type == "Anticipator") values$opponent_decision <- ScopAI:::OptimizedDecisionNPlus1(values$game_state, 2, input$anticipator_parameter)
              showElement("opponent_play", time = 0)
              values$opponent_played <- T
            } else {
              hideElement("opponent_play", time = 0)
              
              values$game_state <- ScopAI:::PlayCard(values$game_state, 2, decision = values$opponent_decision)
              values$deal_action <- "" # now you can have "" because the turn has been updated
              print("a card was played by the opponent")
              values$last_action_other <- glue("Your opponent has just played {values$opponent_decision$play} and taken
                                 {ShowCards(values$opponent_decision$take)}, press Next to play!")
              values$current_player <- 1
              values$wait_the_next <- 2
              values$opponent_played <- F
              values$opponent_decision$play <- "calculating"
              values$opponent_decision$take <- NULL
            }
            
          } else { # else of the if player is player 2
            hideElement("opponent_play", time = 0)
            possible_decisions <- ScopAI:::ListAllPossibleDecisions(values$game_state, 1)
            names(possible_decisions) <- sapply(possible_decisions, function(dec)
              paste("Play", dec$play, "and take", ShowCards(dec$take)))
            
            updateRadioButtons(session, "decision",
                               choices = names(possible_decisions))
            if (input$decision != "mock" & values$wait_the_next == 3) {
              if (!values$you_played) {
                values$your_decision <- possible_decisions[[input$decision]]
                hideElement("decision", time = 0)
                print("a card was played by you")
                values$last_action_you <- glue("You've chosen to play {values$your_decision$play} and take
                                 {ShowCards(values$your_decision$take)}")
                values$last_action_other <- ""
                values$deal_action <- " " # should not be "", otherwise if turn is a multiple of 6 you will deal cards twice
                values$you_played <- T
              } else {
                hideElement("decision", time = 0)
                values$game_state <- ScopAI:::PlayCard(values$game_state, 1, decision = values$your_decision)
                values$deal_action <- "" # now you can have "" because the turn has been updated
                values$current_player <- 2
                values$wait_the_next <- 2
                values$last_action_you <- glue("You've just played {values$your_decision$play} and taken
                                 {ShowCards(values$your_decision$take)}, press Next to let your opponent play!")
                values$you_played <- F
              }
            } # end of if a decision was made
          } # end of the if player is player 2 
        } # end of if about the need to deal new cards
      } # end of the if about turn 37
    } # end of the if about the next button (value = 3)
    print(paste("game turn is ", values$game_state$turn))
  })
  
  observeEvent(input$pixel_cards, {
    values$pixel_for_cards <- input$pixel_cards
  })
  
  output$display_hand_other <- renderPrint({
    print(glue("Opponent hand:")) 
  })
  
  output$opponent_hand_0 <- renderUI({
    if (length(ScopAI:::GetPlayerHand(values$game_state, 2)) - 1*values$opponent_played == 0) img(src = "zero_card.png", 
                                                              height = paste0(values$pixel_for_cards*3, "px"), alt = "0 card")
  })
  
  output$opponent_hand_1 <- renderUI({
    if (length(ScopAI:::GetPlayerHand(values$game_state, 2)) - 1*values$opponent_played == 1) img(src = "one_card.png", 
                                                              height = paste0(values$pixel_for_cards*3, "px"), alt = "1 card")
  })
  
  output$opponent_hand_2 <- renderUI({
    if (length(ScopAI:::GetPlayerHand(values$game_state, 2)) - 1*values$opponent_played == 2) img(src = "two_cards.png",
                                                              height = paste0(values$pixel_for_cards*3, "px"), alt = "2 cards")
  })
  
  
  output$opponent_hand_3 <- renderUI({
    if ((length(ScopAI:::GetPlayerHand(values$game_state, 2)) - 1*values$opponent_played) == 3) img(src = "three_cards.png", 
                                                              height = paste0(values$pixel_for_cards*3, "px"), alt = "3 cards")
  })
  
  output$opponent_play <- renderUI({
    card <- values$opponent_decision$play
    take_suffix <- ifelse(is.null(values$opponent_decision$take), "_board", "_other")
    img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  
  output$display_board <- renderPrint({
    if (length(values$game_state$board) == 0 & values$game_state$turn <= 36) {
      print(glue("SCOPA!"))
    } else {
      print(glue("Board: {ShowCards(values$game_state$board)}"))
    }
  })
  output$board_1 <- renderUI({
    card <- values$game_state$board[1]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_2 <- renderUI({
    card <- values$game_state$board[2]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_3 <- renderUI({
    card <- values$game_state$board[3]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_4 <- renderUI({
    card <- values$game_state$board[4]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_5 <- renderUI({
    card <- values$game_state$board[5]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_6 <- renderUI({
    card <- values$game_state$board[6]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_7 <- renderUI({
    card <- values$game_state$board[7]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_8 <- renderUI({
    card <- values$game_state$board[8]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_9 <- renderUI({
    card <- values$game_state$board[9]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$board_10 <- renderUI({
    card <- values$game_state$board[10]
    take_suffix <- ifelse(card %in% values$opponent_decision$take, "_other", ifelse(card %in% values$your_decision$take, "_you", ""))
    if (values$game_state$turn == 37 & !values$you_played & !values$opponent_played) take_suffix <- c('_you', '_other')[values$game_state$last_taker]
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  
  
  output$remaining_deck_text <- renderPrint({
    print(glue("Remaining deck: {length(values$game_state$deck)} cards left")) 
  })
  output$remaining_deck <- renderUI({
    remaining_n <- length(values$game_state$deck)
    if (remaining_n > 0) img(src = "deck.png", height = paste0(values$pixel_for_cards, "px"), alt = "remaining_deck")
  })
  
  output$your_hand_0 <- renderUI({
    if (length(ScopAI:::GetPlayerHand(values$game_state, 1)) == 0) img(src = "zero_card.png", 
                                                              height = paste0(values$pixel_for_cards*3, "px"), alt = "0 card")
  })
  output$your_hand_1 <- renderUI({
    card <- ScopAI:::GetPlayerHand(values$game_state, 1)[1]
    take_suffix <- ""
    if (values$you_played & card %in% values$your_decision$play) take_suffix <- ifelse(is.null(values$your_decision$take), "_board", "_you")
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$your_hand_2 <- renderUI({
    card <- ScopAI:::GetPlayerHand(values$game_state, 1)[2]
    take_suffix <- ""
    if (values$you_played & card %in% values$your_decision$play) take_suffix <- ifelse(is.null(values$your_decision$take), "_board", "_you")
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  output$your_hand_3 <- renderUI({
    card <- ScopAI:::GetPlayerHand(values$game_state, 1)[3]
    take_suffix <- ""
    if (values$you_played & card %in% values$your_decision$play) take_suffix <- ifelse(is.null(values$your_decision$take), "_board", "_you")
    if (!is.na(card)) img(src = paste0(card, take_suffix, ".png"), height = paste0(values$pixel_for_cards*3, "px"), alt = card)
  })
  
  output$display_hand <- renderPrint({
    print(glue("Your hand: {ShowCards(ScopAI:::GetPlayerHand(values$game_state, 1))}")) 
  })
  # output$display_last_action_opponent <- renderPrint({
  #   print(glue("{values$last_action_other}"))
  # })
  # output$display_your_last_action <- renderPrint({
  #   print(glue("{values$last_action_you}"))
  # })
  output$display_deal_info <- renderPrint({
    print(glue("{values$deal_action}"))
  })
  output$game_parameters <- renderPrint({
    print(glue("{values$parameters}"))
  })
  
  output$end_game <- renderText({
    print(glue("{values$endgame}"))
  })
  

}
  # Run the app ----
  shinyApp(ui = ui, server = server)
  