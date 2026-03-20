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

test_that("no duplicate censoring rows when secondary is followed by a censoring species", {
  # Minimal synthetic case: Deer -> Coyote -> Bear (tertiary)
  # The old lead()-based bind_rows approach produced a spurious extra "CT removed"
  # row for the censoring row added by "Following Censoring Species", because
  # the newly added row had species=Coyote and lead()=NA in the modified data.
  # Fix A (precomputing next_species/next_time on a clean snapshot) prevents this.
  dat <- data.frame(
    Site     = "A",
    Species  = c("Deer", "Coyote", "Bear"),
    DateTime = as.POSIXct(c("2020-01-01 08:00:00",
                             "2020-01-03 08:00:00",
                             "2020-01-05 08:00:00")),
    stringsAsFactors = FALSE
  )

  recu <- ct_to_recurrent(
    dat,
    primary         = "Deer",
    secondary       = "Coyote",
    tertiary        = "Bear",
    survey_duration = 30
  )

  # Should have exactly 2 rows: 1 event (Coyote) + 1 censoring (Bear stops survey)
  expect_equal(nrow(recu), 2L)
  expect_equal(sum(recu$event), 1L)

  # No duplicate rows
  expect_equal(nrow(unique(recu)), nrow(recu))
})
