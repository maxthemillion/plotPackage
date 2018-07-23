##### complete themes #####

#' applies custom modifications to ggplot object
#'
#' @param p A ggplot object
#' @return The modified ggplot object
#'
clean_out_plot <- function(p){
  res <- p %>%
    theme_remove_grid() %>%
    theme_remove_background()

  return(res)
}

##### single modifications ####

#' rotate x axis labels
#'
#'  @inheritParams clean_out_plot
#' @param angle The angle by which labels should be rotated. Defaults to 90.
#'
theme_rotate_labels_x <- function(p, angle = 90){
  res <- p %>%
    theme(axis.text.x = element_text(angle = angle, vjust = 0.5))
  return(res)
}

#' resets axis rotation to 0
#'
#' @inheritParams clean_out_plot
#'
theme_rotate_labels_x_reset <- function(p){
  res <- p %>%
    theme_rotate_labels_x(p, angle = 0)
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
