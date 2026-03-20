context("Test that trafo to recurrent works")

primary   <- "Deer"
secondary <- "Coyote"
tertiary  <- c("Fawn", "Bear", "Bobcat", "Human", "Motorized")
end_date  <- max(murphy$DateTime)

test_that("Trafo works with correct inputs (default: days)", {

  recu = ct_to_recurrent(
    murphy,
    primary,
    secondary,
    tertiary        = tertiary,
    survey_end_date = end_date,
    survey_duration = 30)

  expect_equal(dim(recu), c(221, 11))
  expect_set_equal(
    colnames(recu),
    c("Site", "survey_id", "datetime_primary", "primary", "secondary", "DateTime",
      "t.start", "t.stop", "event", "status", "enum")
  )
  expect_equal(
    as.data.frame(recu[1:2, c("t.start", "t.stop", "event", "enum")]),
    data.frame(t.start = c(0, 10.78403), t.stop = c(10.78403, 30), event = c(1, 0), enum = c(1L, 2L)),
    tolerance = 1e-5
  )

})

test_that("time_unit = 'hours' gives t.start/t.stop scaled by 24 vs days", {

  recu_days = ct_to_recurrent(
    murphy, primary, secondary,
    tertiary        = tertiary,
    survey_end_date = end_date,
    survey_duration = 30,
    time_unit       = "days")

  recu_hours = ct_to_recurrent(
    murphy, primary, secondary,
    tertiary        = tertiary,
    survey_end_date = end_date,
    survey_duration = 30 * 24,
    time_unit       = "hours")

  # Same surveys and events selected
  expect_equal(nrow(recu_days), nrow(recu_hours))
  expect_equal(recu_days$event, recu_hours$event)
  expect_equal(recu_days$survey_id, recu_hours$survey_id)

  # t.start / t.stop should differ by factor 24
  expect_equal(recu_hours$t.start, recu_days$t.start * 24, tolerance = 1e-5)
  expect_equal(recu_hours$t.stop,  recu_days$t.stop  * 24, tolerance = 1e-5)

})

test_that("time_unit = 'mins' gives t.start/t.stop scaled by 1440 vs days", {

  recu_days = ct_to_recurrent(
    murphy, primary, secondary,
    tertiary        = tertiary,
    survey_end_date = end_date,
    survey_duration = 30,
    time_unit       = "days")

  recu_mins = ct_to_recurrent(
    murphy, primary, secondary,
    tertiary        = tertiary,
    survey_end_date = end_date,
    survey_duration = 30 * 24 * 60,
    time_unit       = "mins")

  expect_equal(nrow(recu_days), nrow(recu_mins))
  expect_equal(recu_days$event, recu_mins$event)
  expect_equal(recu_mins$t.start, recu_days$t.start * 1440, tolerance = 1e-5)
  expect_equal(recu_mins$t.stop,  recu_days$t.stop  * 1440, tolerance = 1e-5)

})

test_that("invalid time_unit throws an error", {
  expect_error(
    ct_to_recurrent(murphy, primary, secondary, time_unit = "years"),
    regexp = "time_unit"
  )
})

test_that("non-numeric survey_duration throws an error", {
  expect_error(
    ct_to_recurrent(murphy, primary, secondary, survey_duration = "30"),
    regexp = "survey_duration"
  )
})
