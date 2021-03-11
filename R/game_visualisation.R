PlotTheEvolutionOfAGame <- function(game_states) {
  if (length(game_states) != 37) stop("the game_states should contain 37 items")
  score_df <- data.frame(turn = 1:37,
                         scopa = sapply(game_states, function(g_s) length(g_s$board) == 0),
                         settebello_1 = 1*sapply(game_states, function(g_s) "D7" %in% GetPlayerStack(g_s, 1)),
                         settebello_2 = 1*sapply(game_states, function(g_s) "D7" %in% GetPlayerStack(g_s, 2)),
                         p1_cards = sapply(game_states, function(g_s) CountCardsNumber(GetPlayerStack(g_s, 1))),
                         p2_cards = sapply(game_states, function(g_s) CountCardsNumber(GetPlayerStack(g_s, 2))),
                         p1_denari = sapply(game_states, function(g_s) CountDenariNumber(GetPlayerStack(g_s, 1))),
                         p2_denari = sapply(game_states, function(g_s) CountDenariNumber(GetPlayerStack(g_s, 2))),
                         p1_primiera = sapply(game_states, function(g_s) CountPrimiera(GetPlayerStack(g_s, 1))),
                         p2_primiera = sapply(game_states, function(g_s) CountPrimiera(GetPlayerStack(g_s, 2)))) %>%
    mutate(scopa = factor(((turn%%2)+1)*scopa, levels = 0:2),
           settebello_1 = settebello_1 - lag(settebello_1, default = 0),
           settebello_2 = settebello_2 - lag(settebello_2, default = 0),
           settebello = factor(2*settebello_2 + settebello_1, levels = 0:2))
  final_game_state <- game_states[[37]]
  scope_1 <- GetPlayerScope(final_game_state, 1)
  scope_2 <- GetPlayerScope(final_game_state, 2)
  stack_1 <- GetPlayerStack(final_game_state, 1)
  stack_2 <- GetPlayerStack(final_game_state, 2)
  denari_score_1 <- GiveDenariScoreForAPlayer(stack_1, stack_2)
  denari_score_2 <- GiveDenariScoreForAPlayer(stack_2, stack_1)
  cards_score_1 <- GiveCardsScoreForAPlayer(stack_1, stack_2)
  cards_score_2 <- GiveCardsScoreForAPlayer(stack_2, stack_1) 
  primiera_score_1 <- GivePrimieraScoreForAPlayer(stack_1, stack_2)
  primiera_score_2 <- GivePrimieraScoreForAPlayer(stack_2, stack_1)
  sb_1 <- GiveSetteBelloScoreForAPlayer(stack_1)
  sb_2 <- GiveSetteBelloScoreForAPlayer(stack_2)
  score_1 <- scope_1 + denari_score_1 + cards_score_1 + primiera_score_1 + sb_1
  score_2 <- scope_2 + denari_score_2 + cards_score_2 + primiera_score_2 + sb_2
  
  score_glue <- glue::glue("Final score is {score_1} - {score_2}")
  
  gg_base <- ggplot(score_df, aes(x = turn)) +
    theme_bw() +
    theme(legend.position = "none")
  
  
  gg_cards <- gg_base +
    geom_line(aes(y = p1_cards), color = "red") +
    geom_line(aes(y = p2_cards), color = "blue") +
    scale_y_continuous("Cards") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_cards, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_cards, label = "Player 2"),
                             color = "blue", nudge_y = 1) +
    geom_hline(yintercept = 20, linetype = "dashed") +
    geom_text(data = filter(score_df, turn == 3), aes(label = "Win cards point", y = 20.5)) +
    ggtitle(score_glue)
  
  gg_primiera <- gg_base +
    geom_line(aes(y = p1_primiera), color = "red") +
    geom_line(aes(y = p2_primiera), color = "blue") + 
    scale_y_continuous("Primiera") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_primiera, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_primiera, label = "Player 2"),
                             color = "blue", nudge_y = 1) 
  
  gg_denari <- gg_base +
    geom_line(aes(y = p1_denari), color = "red") +
    geom_line(aes(y = p2_denari), color = "blue") + 
    scale_y_continuous("Denari") +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_denari, label = "Player 1"),
                             color = "red", nudge_y = 1) +
    ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_denari, label = "Player 2"),
                             color = "blue", nudge_y = 1) +
    geom_hline(yintercept = 5, linetype = "dashed") +
    geom_text(data = filter(score_df, turn == 3), aes(label = "Win Denari point", y = 5.2))
  
  gg_oners <- gg_base +
    scale_x_continuous(limits = c(1, 37)) +
    geom_segment(data = filter(score_df, settebello != 0), aes(y = 1, yend = 0, xend = turn, color = settebello)) +
    ggrepel::geom_label_repel(data = filter(score_df, settebello != 0),
                              aes(y = 1, label = "Settebello!", color = settebello), nudge_y = .5)  +
    scale_color_manual(limits = factor(1:2), values = c("red", "blue")) +
    scale_y_continuous("Scope / Settebello", labels = 0:2, breaks = 0:2, limits = c(0, 2))
  
  if (any(score_df$scopa != 0)) gg_oners <- gg_oners + 
    geom_segment(data = filter(score_df, scopa != 0), aes(y = 1, yend = 0, xend = turn, color = scopa)) +
    ggrepel::geom_label_repel(data = filter(score_df, scopa != 0),
                              aes(y = 1, label = "Scopa!", color = scopa), nudge_y = .5)
  
  return(list(cards_evolution = gg_cards,
              denari_evolution = gg_denari,
              primiera_evolution = gg_primiera,
              settebello_scope_evolution = gg_oners))
}