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


#' plots ROC from one or multiple fitted h2o models
#'
#'
#' @param ... one or more trained h2o models
#' @return A ggplot2 object
plot_h2o_ROC <- function(...){

  l <- list(...)
  names <- sapply(l, function(x) x@model_id)

  p <- l %>%
    # map a function to each element in the list
    map(function(x) x %>% h2o.performance(valid=T) %>%
          # from all these 'paths' in the object
          .@metrics %>% .$thresholds_and_metric_scores %>%
          # extracting true positive rate and false positive rate
          .[c('tpr','fpr')] %>%
          # add (0,0) and (1,1) for the start and end point of ROC curve
          add_row(tpr=0,fpr=0,.before=T) %>%
          add_row(tpr=0,fpr=0,.before=F)) %>%
    # add a column of model name for future grouping in ggplot2
    map2(names,
         function(x,y) x %>%
           add_column(Model=y)) %>%
    # reduce four data.frame to one
    reduce(rbind) %>%
    # plot fpr and tpr, map model to color as grouping
    ggplot(aes(fpr,tpr,col=Model))+
    geom_point(alpha = 0.5, shape = 1)+
    geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
    xlab('False Positive Rate')+
    ylab('True Positive Rate')+
    ggtitle('ROC comparing models')

  print(p)

  return(p)
}
