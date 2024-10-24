


for (e in sort(unique(pcs_animals$epoch))){
  
  this_epoch = pcs_animals %>% 
    filter(epoch == e)
  
  plot = this_epoch %>% 
    left_join(animal_images) %>% 
    #filter(animal == "cat") %>% 
    filter(animal %nin% c('salamander', 'alligator')) %>% 
    ggplot(aes(PC1, PC2, label = animal)) +
    #geom_image(aes(image = image), size = .35) +
    geom_label(size = 2) +
    ylim(c(-15, 15)) +
    xlim(c(-15, 15)) +
    labs(x = "Dimension 1",
         y = "Dimension 2") +
    theme_apa()
  
  filename = str_c("outputs/animals/img/plot_", e, ".png")
  
  print(paste("Plot", e, "...done"))
  ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
  
  if (e == max((pcs_animals$epoch))){
    
    for (i in seq(dwell_at_closing)){
      
      filename = str_c("outputs/animals/img/plot_", e + i, ".png")
      
      print(paste("Plot", e + i, "...done"))
      ggsave(filename, plot = plot, width = 6, height = 4, dpi = 300)
      
      
    }
  }  
  
}