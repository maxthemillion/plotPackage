library(h2o)
library(proto)
library(ggplot2)
library(reshape2)
library(scales)

h2o.init()

setup <- function(){

  data(mtcars)
  mtcars$vs <- as.factor(mtcars$vs)
  mtcars$am <- as.factor(mtcars$am)

  h2o_hex <- as.h2o(mtcars)
  d.splits <- h2o.splitFrame(h2o_hex,
                             ratios = c(0.6, 0.2),
                             seed = 1234)

  train <- h2o.assign(d.splits[[1]], "train.hex")
  valid <- h2o.assign(d.splits[[2]], "valid.hex")
  test <- h2o.assign(d.splits[[3]], "test.hex")

  rf_test <- h2o.randomForest(
    training_frame = train,
    validation_frame = valid,
    x = c(2, 3, 5, 7, 8),
    y = 9,
    ntrees = 10,
    max_depth = 20,
    seed = 2020202
  )

  return(rf_test)
}

rf_test <- setup()
