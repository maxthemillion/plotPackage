
library(h2o)
library(proto)
library(ggplot2)
library(reshape2)
library(scales)

###### define setup and teardown ####
setup <- function(){
  h2o.init()

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

teardown <- function(){
  h2o.shutdown(F)
}


##### setup #####
rf_test <- setup()


##### tests ######
context("plot_h2o_classification_error")

test_that("h2o_extract_classification_errors fails on wrong input", {
  df <- data.frame(col1 = c(1,2), col2 = c("x", "y"))

  expect_error(h2o_extract_classification_errors())
})

test_that("h2o_extract_classification_errors delivers correct output", {
  expected_names <- c(
    "validation_classification_error",
    "training_classification_error",
    "idx")

  c_error <- h2o_extract_classification_errors(rf_test)

  expect_equal(length(names(c_error)), 3)
  expect_equal(names(c_error), expected_names)

})

test_that("h2o_plot_classification_error returns ggplot2 geom_line",
          {
            p <- plot_h2o_classification_error(rf_test)

            expect_identical(
              sapply(p$layers, function(x) class(x$geom)[[1]]),
              "GeomLine")
          })

## ROC

context("plot_h2o_ROC")

test_that("h2o_plot_ROC returns ggplot2 geom_line on first layer",
          {
            p <- h2o_plot_ROC(rf_test)

            expect_identical(
              sapply(p$layers, function(x) class(x$geom)[[1]]),
              "GeomLine")
          })


#### clean up ####

teardown()
