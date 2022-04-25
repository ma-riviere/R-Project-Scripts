#=============#
#### Theme ####
#=============#

invis_custom <- ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    ## Legend
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

custom_light <- invis_custom + 
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
    ## Titles
    plot.title = ggtext::element_markdown(size = 12, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 11, face = "italic"),
    ## Legend
    legend.title = ggplot2::element_text(face = "bold"),
    ## Facets
    strip.background = ggplot2::element_rect(fill = "#ffed75"),
    strip.text = ggplot2::element_text(size = 10, face = "bold"),
    ## Axes
    axis.title.x = ggtext::element_markdown(face = "bold", hjust = 0.5),
    axis.title.y = ggtext::element_markdown(face = "bold", hjust = 0.5),
    axis.text = ggplot2::element_text(color = "black"),
    text = ggplot2::element_text(color = "black")
  )

ggplot2::theme_set(custom_light)