###### HELPER FUNCTIONS ######
#' extracts classification errors from h2o.scoreHistory()
#'
#' @param mod An h2o classification problem
#' @return Classification errors
#'
h2o_extract_classification_errors <- function(mod){

  colnames <- c("validation_classification_error",
                "training_classification_error")

#  if(!colnames %in% names(h2o::h2o.scoreHistory(mod))){
#    stop("Model provided is not a classification problem")
#  }

  c_error <- h2o::h2o.scoreHistory(mod)[, colnames]

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
  c_error <- reshape2::melt(h2o_extract_classification_errors(mod),
                  id.vars = "idx")

  c_error$variable <- as.factor(c_error$variable) %>%
    plyr::revalue(c("validation_classification_error" = "validation",
                    "training_classification_error" = "training"))

  p <- ggplot2::ggplot(c_error, aes(x = idx, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste("Classification errors of", mod@model_id, sep = " "),
         x = "Number of trees",
         y = "Classification Error",
         color = "Type of error") +
    ggplot2::ylim(0, min(round(max(c_error$value), digits = 1) + 0.05, 1)) +
    ggplot2::scale_x_continuous(breaks = pretty_breaks()) +
    ggplot2::geom_hline(aes(yintercept= 0.1),
                        color = "grey",
                        linetype= "dashed")

  return(clean_out_plot(p, color_palette = "standard"))
}

plot_h2o_classification_error <- function(...){
  mods = list(...)

  for (elem in mods){
    c_error[elem@model_id] = reshape2::melt(h2o_extract_classification_errors(mod),
                                            id.vars = "idx")
  }
}


#' plots ROC from one or multiple fitted h2o models
#'
#'
#' @param ... one or more trained h2o models
#' @return A ggplot2 object
plot_h2o_ROC <- function(...){
  l <- list(...)
  names <- sapply(l, function(x) x@model_id)

  #auc <- sapply(l, function(x) round(h2o::h2o.auc(x), digits = 4))
  #annotations <- data.frame(models = names, AUC = auc)

  d <- l %>%
    # map a function to each element in the list
    purrr::map(function(x) x %>% h2o.performance(valid=T) %>%
          # from all these 'paths' in the object
          .@metrics %>% .$thresholds_and_metric_scores %>%
          # extracting true positive rate and false positive rate
          .[c('tpr','fpr')] %>%
          # add (0,0) and (1,1) for the start and end point of ROC curve
            tibble::add_row(tpr=0,fpr=0,.before=T) %>%
            tibble::add_row(tpr=0,fpr=0,.before=F)) %>%
    # add a column of model name for future grouping in ggplot2
    purrr::map2(names,
         function(x,y) x %>%
           tibble::add_column(Model=y)) %>%
    # reduce four data.frame to one
    purrr::reduce(rbind)

  d$Model <- factor(d$Model, levels = rev(names))

  p <- d %>%
    # plot fpr and tpr, map model to color as grouping
    ggplot2::ggplot(aes(fpr,tpr,col=Model)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey') +
    ggplot2::labs(title = "Receiver Operating Characteristic (ROC)",
                  x = "False Positive Rate",
                  y = "True Positive Rate") +
    ggplot2::theme(legend.position = c(0.8, 0.2))

  return(clean_out_plot(p, color_palette = "standard"))
}
