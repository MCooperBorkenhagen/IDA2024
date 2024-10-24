dwell_at_closing = 30


max_PC1 = max(pcs$PC1) + 1
max_PC2 = max(pcs$PC2) + 1
min_PC1 = min(pcs$PC1) - 1
min_PC2 = min(pcs$PC2) - 1


### All the words
for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e)
  
  plot = this_epoch %>% 
    ggplot(aes(PC1, PC2, label = word)) +
    geom_label(size = 2) +
    ylim(c(min_PC2, max_PC2)) +
    xlim(c(min_PC1, max_PC1)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    theme_apa() %>% 
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  filename = str_c("outputs/all_words/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/all_words/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
      
      
    }
  }  
  
}

targets_list = list()

### "ing" words
targets = tibble(word = c("things", "wings", "wing", "sings", "sing", "swings", "swing", "ding", "sting", "king", "ping"),
                 condition = "ing") %>% 
  rbind(tibble(word = c("sly", "rings", "yuck", "wrapped", "took", "troops", "trout", "screen", "woke", "fate"),
               condition = "control"))

targets_list[["ing"]] = targets

for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e) %>% 
    right_join(targets)
  
  plot = this_epoch %>% 
    ggplot(aes(PC1, PC2, label = word, fill = condition)) +
    geom_label(size = 2) +
    ylim(c(min_PC2, max_PC2)) +
    xlim(c(min_PC1, max_PC1)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    scale_fill_manual(values = c("white", "goldenrod")) +
    theme_apa() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  filename = str_c("outputs/ing/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/ing/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
      
      
    }
  }  
}



### "br" words
targets = tibble(word = c("brush", "brushed", "breeze", "breed", "bricks", "brick", "breed", "breeze", "broil", "broad"),
                 condition = "br") %>% 
  rbind(tibble(word = c("sly", "rings", "yuck", "wrapped", "took", "troops", "trout", "screen", "woke", "fate"),
               condition = "control"))

targets_list[["br"]] = targets %>% 
  filter(condition == "br")

for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e) %>% 
    right_join(targets)
  
  plot = this_epoch %>% 
    ggplot(aes(PC1, PC2, label = word, fill = condition)) +
    geom_label(size = 2) +
    ylim(c(min_PC2, max_PC2)) +
    xlim(c(min_PC1, max_PC1)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    scale_fill_manual(values = c("coral", "white")) +
    theme_apa() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  
  
  filename = str_c("outputs/br/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/br/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
    }}}


### "oi" words
targets = tibble(word = c("coin", "oil", "soil", "boil", "coin", "boiled", "point", "points", "spoiled", "spoils", "foil"),
                 condition = "oi") %>% 
  rbind(tibble(word = c("sly", "rings", "yuck", "wrapped", "took", "troops", "trout", "screen", "woke", "fate"),
               condition = "control"))


targets_list[["oi"]] = targets %>% 
  filter(condition == "oi")

for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e) %>% 
    right_join(targets)
  
  plot = this_epoch %>% 
    ggplot(aes(PC1, PC2, label = word, fill = condition)) +
    geom_label(size = 2) +
    ylim(c(min_PC2, max_PC2)) +
    xlim(c(min_PC1, max_PC1)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    scale_fill_manual(values = c("white", "steelblue2")) +
    theme_apa() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  filename = str_c("outputs/oi/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/oi/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
    }}}



### "ave" words
ave_words = c("have", "save", "cave", "gave", "pave")
targets = tibble(word = ave_words, 
                 condition = "ave") %>% 
  mutate(is_have = case_when(word == "have" ~ T,
                             T ~ F))

for (e in sort(unique(pcs$epoch))){
  
  this_epoch = pcs %>% 
    filter(epoch == e) %>% 
    right_join(targets)
  
  plot = this_epoch %>% 
    ggplot(aes(PC4, PC8, label = word, fill = is_have)) + #1, 4, 8
    geom_label(size = 2.5) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    ylim(c(min(pcs$PC8), max(pcs$PC8))) +
    xlim(c(min(pcs$PC4), max(pcs$PC4))) +
    scale_fill_manual(values = c("white", "pink")) +
    theme_apa() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  filename = str_c("outputs/ave/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/ave/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
    }}}


### Combined "ing", "oi", and "br" words

