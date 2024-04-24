context("Test that trafo to recurrent works")

test_that("Trafo works with correct inputs", {

  primary = c("Deer")
  # Define the secondary species (affected by the primary species, only one)
  secondary = c("Coyote")
  # Define the tertiary species (affecting the secondary species, censoring the survey if observed but not being the focus of the model)
  tertiary = c("Fawn", "Bear", "Bobcat", "Human", "Motorized")

  # Define the end of study
  end_date = max(murphy$DateTime)

  # Convert into recurrent event
  recu = ct_to_recurrent(
    murphy,
    primary,
    secondary,
    tertiary = tertiary,
    survey_end_date = end_date,
    survey_duration = 30)

  expect_equal(dim(recu),  c(221, 11))
  expect_set_equal(
    colnames(recu),
    c("Site", "survey_id", "datetime_primary", "primary", "secondary", "DateTime",
      "t.start", "t.stop", "event", "status", "enum")
  )
  expect_equal(
    as.data.frame(recu[1:2, c("t.start", "t.stop", "event", "enum")]),
    data.frame(t.start = c(0, 10.78403), t.stop = c(10.78403, 30), event = c(1, 0), enum = c(1L,2L)),
    tolerance = 1e-5
  )


})
