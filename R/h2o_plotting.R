###### HELPER FUNCTIONS ######
#' extracts classification errors from h2o.scoreHistory()
#'
#' @param mod An h2o classification problem
#' @return Classification errors
#'
h2o_extract_classification_errors <- function(mod){

  colnames <- c("validation_classification_error",
                "training_classification_error")

  if(!colnames %in% names(h2o.scoreHistory(mod))){
    stop("Model provided is not a classification problem")
  }

  c_error <- h2o.scoreHistory(mod)[, colnames]

  c_error$idx <- as.numeric(row.names(c_error))

  return(c_error)
  }

###### PLOTTING FUNCTIONS ######
#' plots test and validation error extracted from an h2o classification object
#'
#' @param mod An h2o.ai model
#' @return A ggplot2 object
#'
plot_h2o_classification_error <- function(mod){

  c_error <- melt(h2o_extract_classification_errors(mod),
                  id.vars = "idx")

  c_error$variable <- as.factor(c_error$variable) %>%
    plyr::revalue(c("validation_classification_error" = "validation",
                    "training_classification_error" = "training"))

  p <- ggplot(c_error, aes(x = idx, y = value, color = variable)) +
    geom_line() +
    labs(title = "Classification error",
         x = "Number of trees",
         y = "Classification Error",
         color = "Type of error") +
    ylim(0, min(round(max(c_error$value), digits = 1) + 0.05, 1)) +
    scale_x_continuous(breaks = pretty_breaks())

  print(p)

  return(p)
}

