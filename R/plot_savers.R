#' Saves a plot
#'
#'
#'
#'
plot_save <- function(plot,
                      name,
                      path = paste(getwd(), "export", "plots", sep = "/"),
                      width = 10,
                      height = 5,
                      units = "cm",
                      dpi = "print"){

  ggplot2::ggsave(filename = name,
                  plot = plot,
                  path = path,
                  width = width,
                  height = height,
                  units = units,
                  dpi = dpi)

  return(plot)
}


plot_save_ppt <- function(plot,
                      name,
                      path = paste(getwd(), "export", "plots", sep = "/"),
                      hf,
                      wf){

  full_height = 12
  full_width = 25.4
  plot_save(
    plot,
    name,
    path,
    width = full_width * wf,
    height = full_height * hf
  )
}
