#=============#
#### Theme ####
#=============#

cat(note("\n[CONFIG] Setting custom ggplot theme ...\n"))

invis_custom <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    ## Legend
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

custom_light <- invis_custom + 
  ggplot2::theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    ## Titles
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    ## Legend
    legend.title = element_text(face = "bold"),
    ## Facets
    strip.background = element_rect(fill = "#ffed75"),
    strip.text = element_text(size = 10, face = "bold"),
    ## Axes
    axis.title.x = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(colour = "black"),
    text = element_text(colour = "black")
  )

ggplot2::theme_set(custom_light)