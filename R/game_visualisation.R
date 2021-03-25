ggFormat <- function(gg) ggplot_gtable(ggplot_build(gg + theme(plot.margin = unit(c(0, 0, 0, 0),"cm"))))


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
  score_df[37, "scopa"] <- NA
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
  
  final_score_glue <- glue::glue("Fianl score: <span style='color:blue;'>**Player1** has {score_1}</span>   /   <span style='color:red;'>**Player2** has {score_2}</span>")
  
  cards_score_glue <- glue::glue("No point for cards (20 / 20)")
  if (cards_score_1 == 1) cards_score_glue <- glue::glue("<span style='color:blue;'>Cards score goes to player 1</span>")
  if (cards_score_2 == 1) cards_score_glue <- glue::glue("<span style='color:red;'>Cards score goes to player 2</span>")
  denari_score_glue <- glue::glue("No point for Denari (5 / 5)")
  if (denari_score_1 == 1) denari_score_glue <- glue::glue("<span style='color:blue;'>Denari score goes to player 1</span>")
  if (denari_score_2 == 1) denari_score_glue <- glue::glue("<span style='color:red;'>Denari score goes to player 2</span>")
  primiera_score_glue <- glue::glue("No point for primiera")
  if (primiera_score_1 == 1) primiera_score_glue <- glue::glue("<span style='color:blue;'>Primiera score goes to player 1</span>")
  if (primiera_score_2 == 1) primiera_score_glue <- glue::glue("<span style='color:red;'>Primiera score goes to player 2</span>")
  
  gg_base <- ggplot(score_df, aes(x = turn)) +
    theme_bw() +
    theme(legend.position = "none")
  
  
  gg_cards <- gg_base +
    geom_line(aes(y = p1_cards), color = "blue") +
    geom_line(aes(y = p2_cards), color = "red") +
    scale_y_continuous("Cards") +
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_cards, label = "Player 1"),
    #                          color = "blue", nudge_y = 1) +
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_cards, label = "Player 2"),
    #                          color = "red", nudge_y = 1) +
    geom_hline(yintercept = 20, linetype = "dashed") +
    # geom_text(data = filter(score_df, turn == 3), aes(label = "Win cards point", y = 20.5)) +
    labs(title = final_score_glue,
         subtitle = cards_score_glue) +
    theme(plot.title = ggtext::element_markdown(),
          plot.subtitle = ggtext::element_markdown())
  
  gg_primiera <- gg_base +
    geom_line(aes(y = p1_primiera), color = "blue") +
    geom_line(aes(y = p2_primiera), color = "red") + 
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_primiera, label = "Player 1"),
    #                          color = "blue", nudge_y = 1) +
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_primiera, label = "Player 2"),
    #                          color = "red", nudge_y = 1) +
    scale_y_continuous("Primiera") +
    labs(subtitle = primiera_score_glue) +
    theme(plot.subtitle = ggtext::element_markdown())
    
  gg_denari <- gg_base +
    geom_line(aes(y = p1_denari), color = "blue") +
    geom_line(aes(y = p2_denari), color = "red") + 
    scale_y_continuous("Denari") +
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p1_denari, label = "Player 1"),
    #                          color = "blue", nudge_y = 1) +
    # ggrepel::geom_text_repel(data = filter(score_df, turn == 37), aes(y = p2_denari, label = "Player 2"),
    #                          color = "red", nudge_y = 1) +
    # geom_text(data = filter(score_df, turn == 3), aes(label = "Win Denari point", y = 5.2)) +
    geom_hline(yintercept = 5, linetype = "dashed") +
    labs(subtitle = denari_score_glue) +
    theme(plot.subtitle = ggtext::element_markdown())
  
  gg_oners <- gg_base +
    scale_x_continuous(limits = c(1, 37)) 
  
  if (any(score_df$scopa != 0)) gg_oners <- gg_oners + 
    geom_segment(data = filter(score_df, scopa != 0), aes(y = 1, yend = 0, xend = turn, color = scopa)) +
    ggrepel::geom_label_repel(data = filter(score_df, scopa != 0),
                              aes(y = 1, label = "Scopa!", color = scopa), nudge_y = .5)
    gg_oners <- gg_oners +
      geom_segment(data = filter(score_df, settebello != 0), aes(y = 1, yend = 0, xend = turn, color = settebello)) +
      ggrepel::geom_label_repel(data = filter(score_df, settebello != 0),
                                aes(y = 1, label = "Settebello!", color = settebello), nudge_y = .5)  +
      scale_color_manual(limits = factor(1:2), values = c("blue", "red")) +
      scale_y_continuous("Scope / Settebello", labels = 0:2, breaks = 0:2, limits = c(0, 2))
  
      gg_cards <- gg_cards + theme(axis.title.x = element_blank())
      gg_denari <- gg_denari  + theme(axis.title.x = element_blank())
      gg_primiera <- gg_primiera  + theme(axis.title.x = element_blank())
      
      gridExtra::grid.arrange(ggFormat(gg_cards),
                              ggFormat(gg_denari),
                              ggFormat(gg_primiera),
                              ggFormat(gg_oners),
                              ncol = 1, heights = c(35, 30, 30, 23))
      
}


PlotPlayerComparison = function(player_comparison = CompareTwoPlayers(DecisionFunction1 = RandomDecision, n_procs = 1)){
  player_comparison %>% 
    (function(df){
      df %>% 
        select(-ends_with("CI")) %>% 
        gather(variable, estimate, -player) %>% 
        left_join(df %>% 
                    select(player, ends_with("infCI")) %>% 
                    gather(variable, infCI, -player) %>% 
                    mutate(variable = gsub("_infCI","", variable))) %>% 
        left_join(df %>% 
                    select(player, ends_with("supCI")) %>% 
                    gather(variable, supCI, -player) %>% 
                    mutate(variable = gsub("_supCI","", variable)))
    }) %>% 
    ggplot(aes(x=as.character(player))) +
    theme_bw() +
    facet_wrap(~variable, scales = "free_y") +
    geom_point(aes(y = estimate)) + 
    geom_segment(aes(xend = player, y = infCI, yend = supCI)) + 
    ylab("Estimate (95 CI)") + 
    xlab("Player")
    
}