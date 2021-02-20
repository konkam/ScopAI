# Define values that will be the same for everyone
colours <- c("B", "D", "S", "C")

colours_dict <- setNames(object = c("Spade", "Coppe", "Denari", "Bastoni"),
                         nm = c("S", "C", "D", "B"))

deck <- expand.grid(val = 1:10, col = colours, stringsAsFactors = F) %>%
  (function(df) mapply(FUN = function(x,y) list(colour = x, value = y),
                       df$col, df$val, SIMPLIFY = F))

names(deck) <- paste0(rep(colours, each = 10), rep(1:10, 4))
