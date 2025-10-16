

#' Convert camera trap data into a recurrent event format
#'
#' Takes a camera trap dataset as input (site ID, timestamp and species name) and return a recurrent event dataset.
#' The function creates a survey after each observation of the primary species for a defined maximum duration.
#' Within each survey, all the observations of the secondary species are converted into recurrent events.
#' The returned dataframe can be directly used for recurrent event analysis.
#'
#'
#' @param data A dataframe containing the site, the timestamp and the species information
#' @param primary character A vector of one or several species names considered to affect the secondary species
#' @param secondary character(1) Name of the species affected by the primary (only one name allowed).
#' @param tertiary character Names of the species in \code{species_var} that should not be considere primary or secondary.
#' @param survey_end_date Date Date of study end (e.g. "01-01-2001")
#' @param survey_duration integer(1) Maximum duration of the survey (in days, e.g. "7")
#' @param species_var character(1) Name of the variable that contains species information.
#' @param datetime_var Name of the variable that contains date time information.
#' @param site_var Name of the variable that contains Site ID.
#'
#'
#' @return A dataframe containing:
#' \describe{
#'   \item{Site}{Camera-trap site ID}
#'   \item{ID}{Survey ID}
#'   \item{primary}{primary species starting the survey}
#'   \item{datetime_primary}{Timestamp of the start of the survey (i.e. of the primary species observation)}
#'   \item{DateTime}{Timestamp of the recurrent event (i.e. of the secondary species observation)}
#'   \item{t.start}{Time since the start of the survey}
#'   \item{t.stop}{Time since the last event, which can be the primary species or a previous secondary species}
#'   \item{event}{Binary, 1 = recurrent event, 0 = censoring event}
#'   \item{enum}{Event number within the survey}
#'   \item{Event_type}{Either the recurrent event, giving the name of the secondary species, or a censoring event and the reason of censoring}
#'   \item{...}{Any covariate attached to the dataset}
#' }
#'
#' @import magrittr
#' @import dplyr
#' @import checkmate
#' @importFrom lubridate ymd_hms
#' @importFrom rlang .data :=
#'
#' @export
#'
ct_to_recurrent = function(
  data,
  primary,
  secondary,
  survey_duration = 10,
  datetime_var    = "DateTime",
  species_var     = "Species",
  site_var        = "Site",
  tertiary        = NULL,
  survey_end_date = NULL) {
  
  # Check inputs
  checkmate::assert_data_frame(data)
  checkmate::assert_character(primary, any.missing = FALSE, min.len = 1, unique = TRUE)
  checkmate::assert_string(secondary)
  checkmate::assert_integerish(survey_duration, len = 1, any.missing = FALSE)
  checkmate::assert_string(datetime_var)
  checkmate::assert_class(data[[datetime_var]], "POSIXt")
  checkmate::assert_string(species_var)
  checkmate::assert_string(site_var)
  
  if (is.null(tertiary)) {
    tertiary = setdiff(unique(data[[species_var]]), c(primary, secondary))
  }

  if (is.null(survey_end_date)) {
    survey_end_date = max(data[[datetime_var]])
  }
  
  data = data %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(
      .data[[species_var]] %in% c(primary, secondary, tertiary) &
      .data[[datetime_var]] <= survey_end_date) %>%
    group_by(across(all_of(site_var))) %>%
    arrange(.data[[datetime_var]], .by_group = TRUE) %>%

    # ID of the primary event (Site + Primary event number)
    mutate(PrimaryObs = ifelse(.data[[species_var]] %in% c(primary), 1, 0) %>% cumsum(),
           CensoringObs =  ifelse(.data[[species_var]] %in% c(primary, tertiary), 1, 0) %>% cumsum(),
           survey_id = paste0(.data[[site_var]], "-", .data[["CensoringObs"]])) %>%
    filter(.data[["PrimaryObs"]] != 0) %>% # Remove secondary events before the first primary events
    mutate(Event_type = .data[[species_var]]) %>% # Event type (now = species, later will be change by the type of censoring event if needed)

    dplyr::select(all_of(c(site_var, "PrimaryObs", "survey_id", datetime_var, species_var,
      "Event_type"))) %>%
    arrange(.data[[site_var]], .data[[datetime_var]])

  # After secondary (needs an additional row for the censoring event)
  ## Another censoring species stops the survey
  data <- data %>% {bind_rows(.,
    filter(.,
      .data[[species_var]] %in% secondary &
      lead(.data[[species_var]]) %in% c(primary, tertiary)) %>%
    mutate(Event_type = "Censoring event - Following Censoring Species"))} %>%
    arrange(.data[[site_var]], .data[[datetime_var]])
  # No censoring event (i.e. ending date not correctly defined for the site)
  data <- data %>%  {bind_rows(.,  filter(., .data[[species_var]] %in% secondary & is.na(lead(.data[[species_var]]))) %>%
                          mutate(Event_type = "Censoring event - CT removed"))} %>%
    arrange(.data[[site_var]], .data[[datetime_var]])
  
  # After primary (no additional row, just change the event_type status)
  data <- data %>% group_by(across(all_of(site_var))) %>% 
    mutate(Event_type = ifelse(
      .data[[species_var]] %in% primary &
        lead(.data[[species_var]]) %in% c(primary, tertiary), "Censoring event - No Secondary", .data[["Event_type"]])) %>% ## Another censoring species, but no secondary events in-between
    mutate(Event_type = ifelse(.data[[species_var]] %in% primary &
      is.na(lead(.data[[species_var]])), "Censoring event - CT removed", .data[["Event_type"]])) %>% ## No censoring event (i.e. ending date not correctly defined for the site)

    # Add event information (1 = event, 0 = censoring)
    mutate(event = ifelse(.data[["Event_type"]] %in% secondary, 1, 0),
           status = 0) %>%
    group_by(across(all_of("survey_id")))


  # Add primary event information
  ## Species name starting the survey
  CensoringInfo = data %>%
    group_by(across(all_of("survey_id"))) %>%
    slice(1) %>%
    dplyr::select(all_of(c("survey_id", species_var))) %>%
    dplyr::rename(StartingSurvey = !!rlang::sym(species_var))
  
  ## Primary species DateTime
  data = left_join(data, CensoringInfo) %>%
    mutate(datetime_primary = .data[[datetime_var]][1])

  data <- data %>% group_by(across(all_of(site_var))) %>%
    mutate(!!rlang::sym(datetime_var) := if_else(.data[["Event_type"]] == "Censoring event - Following Censoring Species",  lead(.data[[datetime_var]]), .data[[datetime_var]])) %>%
    filter(.data[["StartingSurvey"]] %in% primary) %>% # Keep only post_primary survey
    group_by(across(all_of("survey_id"))) %>%
    filter(any(.data[["event"]]==1)) %>% # Keep only survey with at least one secondary event
    mutate(t.stop = ifelse(is.na(lag(.data[[datetime_var]])), 0, # Primary event: t.stop = 0
                           ifelse(lag(.data[[datetime_var]]) == .data[[datetime_var]], difftime(.data[[datetime_var]]+1, lag(.data[[datetime_var]]), units  = "days"), # Ties forbidden -> add 1 sec
                                  difftime(.data[[datetime_var]], lag(.data[[datetime_var]]), units  = "days"))) %>% cumsum(), # Secondary events: t.stop = cumulative time between previous events
           t.start = lag(.data[["t.stop"]])) %>%
    filter(!(.data[[species_var]] %in% primary)) %>% # Remove primary events
    mutate(enum = row_number()) # Secondary event number

  # Remove event after survey duration limit
  data <- data %>% filter(.data[["t.start"]] < survey_duration) %>%
    mutate(event  = ifelse(.data[["t.stop"]] > survey_duration, 0, .data[["event"]]),
           Event_type = ifelse(.data[["t.stop"]] > survey_duration, "Censoring event - Survey end", .data[["Event_type"]]),  # Censoring event
           t.stop = ifelse(.data[["t.stop"]] > survey_duration, survey_duration, .data[["t.stop"]])) %>%  # t.stop of censoring event = survey duration

    # Column information
    select(all_of(c(site_var, "survey_id", "datetime_primary", "StartingSurvey", species_var, datetime_var, "t.start", "t.stop", "event", "status", "enum"))) %>%
    rename(primary = !!rlang::sym("StartingSurvey"),
           secondary = !!rlang::sym(species_var))

  return(data)

}
