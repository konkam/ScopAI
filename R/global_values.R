# Define values that will be the same for everyone
colours <- c("B", "C", "D", "S")

colours_dict <- setNames(object = c("Spade", "Coppe", "Denari", "Bastoni"),
                         nm = c("S", "C", "D", "B"))

values_dict <- setNames(object = 8:10,
                         nm = c("Fante", "Cavallo", "Re"))

deck <- expand.grid(val = 1:10, col = colours, stringsAsFactors = F) %>%
  (function(df) mapply(FUN = function(x,y) list(colour = x, value = y),
                       df$col, df$val, SIMPLIFY = F))

names(deck) <- paste0(rep(colours, each = 10), rep(1:10, 4))

values_for_primiera <- setNames(object = c(16, 12, 13, 14, 15, 18, 21, 10, 10, 10),
                                nm = 1:10)
