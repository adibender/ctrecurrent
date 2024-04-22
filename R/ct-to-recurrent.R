

#' Convert camera trap data into a recurrent event format
#'
#' Takes a camera trap dataset as input (site ID, timestamp and species name) and return a recurrent event dataset.
#' The function creates a survey after each observation of the primary species for a defined maximum duration.
#' Within each survey, all the observations of the secondary species are converted into recurrent events.
#' The returned dataframe can be directly used for recurrent event analysis.
#'
#'
#' @param data A dataframe containing the site, the timestamp and the species information
#' @param primary A pool of one or several species names considered to affect the secondary species
#' @param secondary Name of the species affected by the primary (only one name allowed).
#' @param survey_end_date Date of study end (e.g. "01-01-2001")
#' @param survey_duration Maximum duration of the survey (in days, e.g. "7")
#' @param species_var Name of the variable that contains species information.
#' @param datetime_var Name of the variable that contains date time information.
#' @param site_var Name of the variable that contains Site ID.
#'
#'
#' @return A dataframe containing:
#' \describe{
#'   \item{Site}{Camera-trap site ID}
#'   \item{ID}{Survey ID}
#'   \item{primary}{primary species starting the survey}
#'   \item{DateTime_Primary}{Timestamp of the start of the survey (i.e. of the primary species observation)}
#'   \item{DateTime}{Timestamp of the recurrent event (i.e. of the secondary species observation)}
#'   \item{t.start}{Time since the start of the survey}
#'   \item{t.stop}{Time since the last event, which can be the primary species or a previous secondary species}
#'   \item{event}{Binary, 1 = recurrent event, 0 = censoring event}
#'   \item{enum}{Event number within the survey}
#'   \item{Event_type}{Either the recurrent event, giving the name of the secondary species, or a censoring event and the reason of censoring}
#'   \item{DateTime_Primary}{Timestamp of survey start (primary observation)}
#'   \item{...}{Any covariate attached to the dataset}
#' }
#'
#' @import magrittr
#' @import dplyr
#' @import checkmate
#' @importFrom lubridate ymd_hms
#'
#' @export
#'
ct_to_recurrent = function(
  data,
  primary,
  secondary,
  survey_end_date,
  tertiary        = NULL,
  datetime_var    = "DateTime",
  species_var     = "Species",
  site_var        = "Site",
  survey_duration = 10) {

  data = data %>%
    mutate_if(is.factor, as.character) %>%
    filter(.data[[species_var]] %in% c(primary, secondary, tertiary) & DateTime <= survey_end_date) %>%
    group_by(Site) %>%
    arrange(DateTime, .by_group = TRUE) %>%

    # ID of the primary event (Site + primary event number)
    mutate(PrimaryObs = ifelse(.data[[species_var]] %in% c(primary), 1, 0) %>% cumsum(),
           CensoringObs =  ifelse(.data[[species_var]] %in% c(primary, tertiary), 1, 0) %>% cumsum(),
           SurveyId = paste0(Site, "-", CensoringObs)) %>%

    # Remove secondary events before the first primary events
    filter(PrimaryObs != 0) %>%

    # Event type (now = species, later will be change by the type of censoring event if needed)
    mutate(Event_type = .data[[species_var]]) %>%

    dplyr::select(all_of(c(site_var, "PrimaryObs", "SurveyId", datetime_var, species_var, "Event_type"))) %>%
    arrange(.data[[site_var]], .data[[datetime_var]])

  # After secondary (needs an additional row for the censoring event)
  ## Another censoring species stops the survey
  data %<>% {bind_rows(.,  filter(., Species %in% secondary & lead(Species) %in% c(primary, tertiary)) %>%
                         mutate(Event_type = "Censoring event - Following Censoring Species"))} %>%
    arrange(Site, DateTime)
  # No censoring event (i.e. ending date not correctly defined for the site)
  data %<>%  {bind_rows(.,  filter(., Species %in% secondary & is.na(lead(Species))) %>%
                          mutate(Event_type = "Censoring event - CT removed"))} %>%
    arrange(Site, DateTime)

  # After primary (no additional row, just change the event_type status)
  data %<>% group_by(Site) %>%
    ## Another censoring species, but no secondary events in-between (censoring event = "No secondary")
    mutate(Event_type = ifelse(Species %in% primary & lead(Species) %in% c(primary, tertiary), "Censoring event - No secondary", Event_type)) %>%
    ## No censoring event (i.e. ending date not correctly defined for the site)
    mutate(Event_type = ifelse(Species %in% primary & is.na(lead(Species)), "Censoring event - CT removed", Event_type)) %>%

    # Add event information (1 = event, 0 = censoring)
    mutate(event = ifelse(Event_type %in% secondary, 1, 0),
           status = 0) %>%
    group_by(SurveyId)


  # Add primary event information
  ## Species name starting the survey
  CensoringInfo = data %>%
    group_by(SurveyId) %>%
    slice(1) %>%
    dplyr::select(SurveyId, Species) %>%
    dplyr::rename(StartingSurvey = Species)
  ## primary species DateTime (required for PlotEvent function in calendar time)
  data = left_join(data, CensoringInfo) %>%
    mutate(DateTime_Primary = DateTime[1])

  data %<>% group_by(Site) %>%
    ## Censoring DateTime (required to calculate t.stop afterwards)
    mutate(DateTime = if_else(Event_type == "Censoring event - Following Censoring Species",  lead(DateTime), DateTime)) %>%
    # Keep only post_primary survey
    filter(StartingSurvey == primary) %>%
    # Keep only survey with at least one secondary event
    group_by(SurveyId) %>%
    filter(any(event==1)) %>%
    # Calculate t.start and t.stop
    mutate(t.stop = ifelse(is.na(lag(DateTime)), 0, # primary event: t.stop = 0
                           ifelse(lag(DateTime) == DateTime, difftime(DateTime+1, lag(DateTime), units  = "days"), # Ties forbidden -> add 1 sec
                                  difftime(DateTime, lag(DateTime), units  = "days"))) %>% cumsum(),               # secondary events: t.stop = cumulative time between previous events
           t.start = lag(t.stop)) %>%
    # Remove primary events
    subset(!(Species %in% primary))%>% droplevels() %>%
    # secondary event number
    mutate(enum = row_number())

  # Remove event after survey duration limit
  data %<>% filter(t.start < survey_duration) %>%
    mutate(event  = ifelse(t.stop > survey_duration, 0, event),
           Event_type = ifelse(t.stop > survey_duration, "Censoring event - Survey end", Event_type),  # Censoring event
           t.stop = ifelse(t.stop > survey_duration, survey_duration, t.stop)) %>%  # t.stop of censoring event = survey duration

    # Column information
    select(Site, SurveyId, DateTime_Primary, StartingSurvey, Species, DateTime, t.start, t.stop, event, status, enum) %>%
    rename(primary = StartingSurvey,
           secondary = Species)

  return(data)

}
