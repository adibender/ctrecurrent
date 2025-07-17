
test_that("ct_to_recurrent (refactored) gives identical output", {
  golden <- readRDS("recu_golden.rds")
  
  primary = c("Deer")
  secondary = c("Coyote")
  
  # Define the tertiary species (affecting the secondary species, censoring the survey if observed but not being the focus of the model)
  tertiary = c("Fawn", "Bear", "Bobcat", "Human", "Motorized")
  
  # Define the end of study
  end_date = max(murphy$DateTime)
  
  new <- ct_to_recurrent(
    data            = murphy,
    primary         = primary,
    secondary       = secondary,
    tertiary        = tertiary,
    datetime_var    = "DateTime",
    species_var     = "Species",
    site_var        = "Site",
    survey_end_date = end_date,
    survey_duration = 30
  )
  
  # For data‐frames, the simplest is strict equality:
  expect_identical(new, golden)
  
  # If you want to allow minor differences in attributes,
  # you can do:
  # expect_equal(new, golden, check.attributes = FALSE)
})