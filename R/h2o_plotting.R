
#' plots test and validation error extracted from an h2o object
#'
#' @param mod An h2o.ai model
#' @return A ggplot2
#'
plot_h2o_classification_error <- function(mod){

  c_error <- h2o.scoreHistory(mod)[, c("validation_classification_error",
                                       "training_classification_error")]

  c_error$idx <- as.numeric(row.names(c_error))
  c_error <- melt(c_error, id.vars = "idx")

  c_error$variable <- as.factor(c_error$variable) %>%
    plyr::revalue(c("validation_classification_error" = "validation",
                    "training_classification_error" = "training"))

  p <- ggplot(c_error, aes(x = idx, y = value, color = variable)) +
    geom_line() +
    labs(title = "Classification error",
         x = "Number of trees",
         y = "Classification Error",
         color = "Type of error") +
    ylim(0, min(round(max(0.11), digits = 1) + 0.05, 1)) +
    scale_x_continuous(breaks = pretty_breaks())

  print(p)

  return(p)
}
