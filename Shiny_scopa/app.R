library(shiny)
library(shinyWidgets)
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
                            selected = "Random Player"),
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
               actionButton("newgame", "New Game", class = "btn-lg")
        ) # end of first column
      ), # end of fluidrow
      
    ), # end of sidebarPanel
    mainPanel(
      h5(textOutput("game_parameters"), style = "color:grey"),
      h4(textOutput("display_hand_other"), style = "color:red"),
      h5(em(textOutput("display_last_action_opponent"), style = "color:red")),
      h4(textOutput("display_board"), style = "color:green"),
      h4(textOutput("display_hand"), style = "color:blue"),
      h3(textOutput("end_game"), style = "color:brown"),
      h5(em(textOutput("display_your_last_action"), style = "color:blue")),
      h5(em(textOutput("display_general_info"))),
      radioButtons("decision",
                   label = h5("Choice"),
                   choices = list("This is a mock" = "mock"), 
                   selected = "mock"),
      actionButton("next_play", "Next", class = "btn-lg")
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
    last_action_general = NULL,
    endgame = NULL,
    your_decision = NULL,
    parameters = NULL
  )
  
  # New game button is clicked (this also runs when the app first loads)
  observeEvent(input$newgame, ignoreNULL = FALSE, {
    values$current_player <- input$starting_player
    values$game_state <- InitialiseGameState(seed = input$seed_entered, starting_player = values$current_player)
    values$wait_the_next <- 1
    values$last_action_general <- "The first cards have been dealt. Click on the Next button to start playing"
    values$last_action_other <- ""
    values$last_action_you <- ""
    values$endgame <- ""
    updateRadioButtons(session, "decision",
                       choices = list("This is a mock" = "mock"),
                       selected = "mock")
    values$dealt <- F
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
  
  observeEvent(input$next_play, ignoreNULL = FALSE, {
    if (values$wait_the_next == 3) {
      if (values$game_state$turn == 37) {
        values$endgame <- glue("That was the last card! The remaining board {ShowCards(values$game_state$board)}
        went to {c('you', 'your opponent')[values$game_state$last_taker]}. The party is over, the score
        is {GiveScoreFromStateForAPlayer(values$game_state, 1)} for you
        and {GiveScoreFromStateForAPlayer(values$game_state, 2)} for your opponent. Press New Game to play again.")
        values$game_state <- FinishGame(values$game_state)
        values$last_action_other <- ""
        values$last_action_you <- ""
        values$last_action_general <- ""
        values$wait_the_next <- 4 # at the end of the game, a next is doing nothing
      } else { # else of the if about turn 37
        if (values$game_state$turn %in% (6*1:5 + 1) & values$last_action_general == "") {
          values$game_state <- DealPlayersCards(game_state = values$game_state, starting_player = input$starting_player)
          values$last_action_general <- "New cards have been dealt"
          if (values$game_state$turn == 31) values$last_action_general <- "The last cards have been dealt!"
          values$last_action_other <- ""
          values$last_action_you <- ""
        } else { # else of if about the need to deal new cards
          if (values$current_player == 2) {
            if (input$decision_type == "Random Player") opponent_decision <- RandomDecision(values$game_state, 2)
            if (input$decision_type == "Optimizer") opponent_decision <- OptimizedDecision(values$game_state, 2)
            if (input$decision_type == "Anticipator") opponent_decision <- OptimizedDecisionNPlus1(values$game_state, 2, input$anticipator_parameter)
            
            values$game_state <- PlayCard(values$game_state, 2, decision = opponent_decision)
            print("a card was played by the opponent")
            values$last_action_other <- glue("Your opponent has just played {opponent_decision$play} and taken
                                 {ShowCards(opponent_decision$take)}, press Next to play!")
            values$last_action_you <- ""
            values$last_action_general <- ""
            values$current_player <- 1
            values$wait_the_next <- 2
            updateRadioButtons(session, "decision",
                               choices = list("This is a mock" = "mock"),
                               selected = "mock")
          } else { # else of the if player is player 2
            possible_decisions <- ListAllPossibleDecisions(values$game_state, 1)
            names(possible_decisions) <- sapply(possible_decisions, function(dec)
              paste("Play", dec$play, "and take", ShowCards(dec$take)))
            
            updateRadioButtons(session, "decision",
                               choices = names(possible_decisions))
            if (input$decision != "mock" & values$wait_the_next == 3) {
              values$your_decision <- possible_decisions[[input$decision]]
              values$game_state <- PlayCard(values$game_state, 1, decision = values$your_decision)
              print("a card was played by you")
              values$last_action_other <- ""
              values$last_action_general <- ""
              values$last_action_you <- glue("You've just played {values$your_decision$play} and taken
                                 {ShowCards(values$your_decision$take)}, press Next to let your opponent play!")
              values$current_player <- 2
              values$wait_the_next <- 2
            } # end of if a decision was made
          } # end of the if player is player 2 
        } # end of if about the need to deal new cards
      } # end of the if about turn 37
    } # end of the if about the next button (value = 3)
    print(paste("game turn is ", values$game_state$turn))
  })
  
  
  output$display_hand_other <- renderPrint({
    print(glue("Opponent hand: {length(GetPlayerHand(values$game_state, 2))} cards"))
  })
  output$display_board <- renderPrint({
    if (length(values$game_state$board) == 0 & values$game_state$turn <= 36) {
      print(glue("SCOPA!"))
    } else {
      print(glue("Board: {ShowCards(values$game_state$board)}"))
    }
  }) 
  output$display_hand <- renderPrint({
    print(glue("Your hand: {ShowCards(GetPlayerHand(values$game_state, 1))}"))
  })
  output$display_last_action_opponent <- renderPrint({
    print(glue("{values$last_action_other}"))
  })
  output$display_your_last_action <- renderPrint({
    print(glue("{values$last_action_you}"))
  })
  output$display_general_info <- renderPrint({
    print(glue("{values$last_action_general}"))
  })
  output$game_parameters <- renderPrint({
    print(glue("{values$parameters}"))
  })
  
  output$end_game <- renderPrint({
    print(glue("{values$endgame}"))
  })
  
  
  }
  # Run the app ----
  shinyApp(ui = ui, server = server)
  