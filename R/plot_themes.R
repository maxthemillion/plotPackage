##### complete themes #####

#' applies custom modifications to ggplot object
#'
#' @param p A ggplot object
#' @param color_neutral A boolean indicating whether a neutral palette should be applied
#' @return The modified ggplot object
#'
clean_out_plot <- function(p, color_palette = "msg_standard"){
  res <- p %>%
    theme_remove_grid() %>%
    theme_remove_background() %>%
    theme_apply_colors(color_palette = color_palette) %>%
    theme_show_axes() %>%
    theme_set_font_size

  return(res)
}

##### color palettes #####
  get_custom_palette <- function(name = "grey3", named = F){

  palettes = list(
    grey3 = c(grey1 = "grey14",
              grey2 = "grey30",
              grey3 = "grey40"),

    msg_neutral = c(
      teal1 = rgb(96, 163, 188, maxColorValue = 255),
      teal2 = rgb(63, 126, 150, maxColorValue = 255),
      teal3 = rgb(42, 84, 100, maxColorValue = 255),
      teal4 = rgb(160, 200, 215, maxColorValue = 255),
      grey1 = rgb(203, 203, 203, maxColorValue = 255),
      grey2 = rgb(127, 127, 127, maxColorValue = 255),
      grey3 = rgb(63, 63, 63, maxColorValue = 255),
      grey4 = rgb(234, 234, 234, maxColorValue = 255),
      black = rgb(0, 0, 0, maxColorValue = 255),
      red = rgb(132, 20, 57, maxColorValue = 255)
    ),

    msg_standard = c(
      black = rgb(0, 0, 0, maxColorValue = 255),
      red = rgb(132, 20, 57, maxColorValue = 255),
      teal1 = rgb(96, 163, 188, maxColorValue = 255),
      teal2 = rgb(63, 126, 150, maxColorValue = 255),
      teal3 = rgb(42, 84, 100, maxColorValue = 255),
      teal4 = rgb(160, 200, 215, maxColorValue = 255),
      grey1 = rgb(203, 203, 203, maxColorValue = 255),
      grey2 = rgb(127, 127, 127, maxColorValue = 255),
      grey3 = rgb(63, 63, 63, maxColorValue = 255),
      grey4 = rgb(234, 234, 234, maxColorValue = 255)
    )
  )

  res <- palettes[name]

  if(!named){
    res <- unname(res)
  }

  return(res)
}



#' returns the msg color palette
#'
#'
#'
get_msg_palette <- function(neutral = T, named = F){

    if(neutral){
    msg_palette <- c(
      teal1 = rgb(96, 163, 188, maxColorValue = 255),
      teal2 = rgb(63, 126, 150, maxColorValue = 255),
      teal3 = rgb(42, 84, 100, maxColorValue = 255),
      teal4 = rgb(160, 200, 215, maxColorValue = 255),
      grey1 = rgb(203, 203, 203, maxColorValue = 255),
      grey2 = rgb(127, 127, 127, maxColorValue = 255),
      grey3 = rgb(63, 63, 63, maxColorValue = 255),
      grey4 = rgb(234, 234, 234, maxColorValue = 255),
      black = rgb(0, 0, 0, maxColorValue = 255),
      red = rgb(132, 20, 57, maxColorValue = 255)
    )

  } else {
    msg_palette <- c(
      black = rgb(0, 0, 0, maxColorValue = 255),
      red = rgb(132, 20, 57, maxColorValue = 255),
      teal1 = rgb(96, 163, 188, maxColorValue = 255),
      teal2 = rgb(63, 126, 150, maxColorValue = 255),
      teal3 = rgb(42, 84, 100, maxColorValue = 255),
      teal4 = rgb(160, 200, 215, maxColorValue = 255),
      grey1 = rgb(203, 203, 203, maxColorValue = 255),
      grey2 = rgb(127, 127, 127, maxColorValue = 255),
      grey3 = rgb(63, 63, 63, maxColorValue = 255),
      grey4 = rgb(234, 234, 234, maxColorValue = 255)
    )
  }

  if(!named){
    msg_palette <- unname(msg_palette)
  }

  return(msg_palette)
}

#' returns a specific msg color from the palette
#'
#' @param color A color name or index
#'
#'
msg_color <- function(color = "red") {
  palette <- get_msg_palette(named = T)

  if (is.numeric(color)) palette[color]
  else palette[tolower(color)]

}

##### modifications ####

#' applies the msg color palette
#'
#' @param p A ggplot object
#' @return The modified ggplot object
#'
theme_apply_colors <- function(p, color_palette){
  if(color_palette == "standard"){return(p)}

  if (color_palette == "brewer1"){
    res <- p +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2")

  } else if (color_palette == "brewer2"){
    res <- p +
      scale_fill_brewer(palette = "Paired") +
      scale_color_brewer(palette = "Paired")
  } else {
    res <- p +
      scale_fill_manual(values = get_custom_palette(name = color_palette, named = F)) +
      scale_color_manual(values = get_custom_palette(name = color_palette, named = F))
  }

return(res)
}


#' Show axes as solid black lines
#'
#' @param p a ggplot object
#' @return The modified ggplot object
#'
theme_show_axes <- function(p){
  res <- p %>%
    theme_show_axis_y() %>%
    theme_show_axis_x()
  return(res)
}

#' show x axis as solid black line
#'
#' @inheritParams theme_show_axes
#'
#'
theme_show_axis_x <- function(p){
  res <- p +
    theme(axis.line.x = element_line(color="black", size = 0.25))
  return(res)
}

#' show y axis as solid black line
#'
#' @inheritParams theme_show_axes
#'
theme_show_axis_y <- function(p){
  res <- p +
    theme(axis.line.y = element_line(color="black", size = 0.25))
  return(res)
}

#' rotate x axis labels
#'
#'  @inheritParams clean_out_plot
#' @param angle The angle by which labels should be rotated. Defaults to 90.
#'
theme_rotate_labels_x <- function(p, angle = 90){
  res <- p +
    theme(axis.text.x = element_text(angle = angle, vjust = 0.5))
  return(res)
}

#' resets axis rotation to 0
#'
#' @inheritParams clean_out_plot
#'
theme_rotate_labels_x_reset <- function(p){
  res <- p %>%
    theme_rotate_labels_x(angle = 0)
  return(res)
}

#' remove y gridlines from ggplot object
#'
#' @inheritParams clean_out_plot
#'
theme_remove_y_grid <- function(p){
  res <- p +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

  return(res)
}

#' remove y gridlines from ggplot object
#'
#' @inheritParams clean_out_plot
#'
theme_remove_x_grid <- function(p){
  res <- p +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  return(res)
}

#' remove all gridlines from ggplot object
#'
#' @param p A ggplot object
#'
theme_remove_grid <- function(p){
  res <- p %>%
    theme_remove_y_grid() %>%
    theme_remove_x_grid()
  return(res)
}

#' set blank background on ggplot object
#'
#' @inheritParams clean_out_plot
#'
theme_remove_background <- function(p){
  res <- p +
    theme(
      panel.background = element_rect(fill = NA)
    )
}

theme_set_font_size <- function(p){
  res <- p +
    theme(title = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8)
          )
}


