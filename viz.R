#======================#
#### Visualizations ####
#======================#

#--------------#
#### Colors ####
#--------------#

#### TODO: define lisa:: colors in theme ?

qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#-------------#
#### Theme ####
#-------------#

theme_invis <- ggplot2::theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    ## Legend
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

my_theme_light <- theme_invis + 
  theme(
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

# my_theme_dark <- theme_invis + theme(
#   panel.border = element_rect(fill = NA, colour = "white"),
#   ## Titles
#   plot.title = element_text(size = 12, face = "bold", colour = "white"),
#   plot.subtitle = element_text(size = 11, face = "italic", colour = "white"),
#   ## Legend
#   legend.title = element_text(face = "bold", colour = "white"),
#   ## Facets
#   strip.background = element_rect(fill = "#ffed75"),
#   strip.text = element_text(size = 10, face = "bold"),
#   ## Axes
#   axis.title.x = element_text(face = "bold", hjust = 0.5),
#   axis.title.y = element_text(face = "bold", hjust = 0.5),
#   axis.text = element_text(colour = "white"),
#   text = element_text(colour = "white")
# )

ggplot2::theme_set(my_theme_light)

#--------------#
#### Tables ####
#--------------#

#### TODO: Use gt

latex_table <- function(.data, .title = "Table caption", .type = "html") {
  kable(
    .data, 
    format = .type,
    col.names = gsub("[.]", " ", names(.data)), 
    booktabs = T, 
    align = "c", 
    digits = 3, 
    format.args = list(big.mark = ",", scientific = FALSE), 
    escape = F, 
    caption = .title, 
    linesep = c("")
  ) %>%
    kable_styling(
      font_size = 12, 
      latex_options = c("striped", "scale_down", "hold_position"), 
      bootstrap_options = c("striped", "condensed", "responsive"), 
      position = "center",
      full_width = T
      # htmltable_class = "lightable-material"
    ) %>%
    row_spec(0, background = "#d4dbde", bold = T, color = "#2b4894")
}

table.DT <- function(.data, .title = "My Table", .digits = 3) {
  .cols.to.format <- .data %>%
    select(where(is.double)) %>%
    colnames()
  
  datatable(
    .data,
    extensions = c("Buttons"),
    class = "cell-border stripe compact",
    filter = "none",
    options = list(
      dom = "Bfrtip",
      buttons = c("excel", "pdf"),
      pageLength = 10,
      autoWidth = T
    ),
    caption = .title,
    autoHideNavigation = T
  ) %>% formatRound(.cols.to.format, .digits)
}

table.slide <- function(.data, .digits = 3) {
  .format.digits <- .data %>%
    select(where(is.double)) %>%
    colnames()
  
  .format.p <- .data %>%
    select(where(is.double)) %>%
    select(matches("[.]p$|^p[.]")) %>% select(-matches("SW.p")) %>% #TODO: remove later
    colnames()
  
  .res <- datatable(
    .data,
    extensions = c("Buttons"),
    class = "cell-border stripe compact",
    fillContainer = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons = c("excel", "pdf"),
      pageLength = 9,
      autoWidth = F
    ),
  )
  
  if (length(.format.digits) > 0) .res %<>% formatRound(.format.digits, .digits)
  
  if (length(.format.p) > 0) {
    .res %<>% formatStyle(
      .format.p,
      color = styleInterval(c(alpha, 0.1), c("green", "#C4DC17", "gray")),
      fontWeight = styleInterval(alpha, c("bold", "normal"))
    )
  }
  return(.res)
}