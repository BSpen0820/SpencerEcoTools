.fake_metchamber_result <- function(scenario_names) {
  hp <- data.frame(MET.W. = seq(10, 20, length.out = 10), TAIR = seq(-20, 30, length.out = 10))
  hourplot <- stats::setNames(rep(list(hp), length(scenario_names)), scenario_names)
  structure(
    list(
      hourplot   = hourplot,
      dimensions = data.frame(),
      target_rmr = list(trgt = 15, err = 0.1),
      log        = data.frame()
    ),
    class = "metchamber_result"
  )
}

test_that("plot.metchamber_result builds both charts when all 4 scenarios are present", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- .fake_metchamber_result(.mc_scenario_ids)
  plots <- plot(result)
  expect_named(plots, c("variable_temp", "constant_temp"))
  expect_s3_class(plots$variable_temp, "ggplot")
  expect_s3_class(plots$constant_temp, "ggplot")
})

test_that("plot.metchamber_result builds only the available pair", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- .fake_metchamber_result(c("standing_variable", "curled_variable"))
  plots <- plot(result)
  expect_named(plots, "variable_temp")
})

test_that("plot.metchamber_result messages and returns NULL when no pair is complete", {
  result <- .fake_metchamber_result("standing_variable")
  expect_message(plots <- plot(result), "no complete")
  expect_null(plots)
})
