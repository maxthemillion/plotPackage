
#' plots test and validation error extracted from an h2o object
#'
#' @param mod An h2o.ai model
#'
plot_h2o_classification_error <- function(mod){

  c_error <- h2o.scoreHistory(mod)[, c("validation_classification_error",
                                       "training_classification_error")]

  c_error$idx <- as.numeric(row.names(c_error))
  c_error <- melt(c_error, id.vars = "idx")

  c_error <- as.factor(c_error$variable) %>%
    plyr::revalue(c("validation_classification_error" = "validation",
                    "training_classification_error" = "training"))

  levels(c_error$variable) <- c("validation", "training")

  p <- ggplot(c_error, aes(x = idx, y = value, color = variable)) +
    geom_line() +
    labs(title = "Classification error",
         x = "Number of trees",
         y = "Classification Error",
         color = "Type of error") +
    ylim(0, 1) +

    scale_x_continuous(breaks = pretty_breaks())

  print(p)
}