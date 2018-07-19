context("h2o plots: classification error")

test_that("h2o_plot_classification_error returns ggplot2",
          {
            p <- plot_h2o_classification_error(rf_test)

            expect_identical(
              sapply(p$layers, function(x) class(x$geom)[[1]]),
              "GeomLine")

          })
