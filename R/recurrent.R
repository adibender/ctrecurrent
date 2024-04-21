

#' Convert camera trap data into a recurrent event format
#'
#' Takes a camera trap dataset as input (site ID, timestamp and species name) and return a recurrent event dataset.
#' The function creates a survey after each observation of the primary species for a defined maximum duration.
#' Within each survey, all the observations of the secondary species are converted into recurrent events.
#' The returned dataframe can be directly used for recurrent event analysis.
#'
#'
#' @param df A dataframe containing the site, the timestamp and the species information
#' @param Primary A pool of one or several species names considered to affect the secondary species
#' @param Secondary Name of the species affected by the primary (only one name allowed).
#' @param End_survey Date of study end (e.g. "01-01-2001")
#' @param survey_duration Maximum duration of the survey (in days, e.g. "7")
#'
#'
#' @return A dataframe containing:
#' \describe{
#'   \item{Site}{Camera-trap site ID}
#'   \item{ID}{Survey ID}
#'   \item{Primary}{Primary species starting the survey}
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
#' @importFrom lubridate ymd_hms
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(pammtools)
#' library(ggplot2)
#'
#' ####
#' # Data Formating
#' ####
#'
#' # See data's vignette for details
#' head(Murphy2021_ct_data)
#'
#' # Define the pool of primary species (affecting secondary species, stopping the survey if observed)
#' Primary = c("Fawn", "Deer", "Bear", "Bobcat", "Human", "Motorized")
#'
#' # Define the secondary species (affected bby the primary species, only one)
#' Secondary = c("Coyote")
#'
#' # Define the end of study
#' End_survey = max(Murphy2021_ct_data$DateTime)
#'
#' # Convert into recurrent event
#' recu = recurrent(Murphy2021_ct_data, Primary, Secondary, End_survey, survey_duration = 7)
#'
#' # Select only one primary species for recurrent event analysis
#' recu_deer = recu %>% filter(Primary == "Deer")
#'
#' # PED transformation
#' ped = recu_deer %>%
#'     as_ped(formula = Surv(t.start, t.stop, event)~ matrix,
#'            id = "Id",
#'            transition = "enum",
#'            timescale = "calendar")
#'
#' ####
#' # PAMM
#' ####
#'
#' # Null model
#' m_null = pamm(formula = ped_status ~
#'     s(tend) +
#'     s(Id, bs = "re"),
#'     data = ped,
#'     offset = offset,  engine = "bam", method = "fREML",  discrete = TRUE)
#' summary(m_null)
#'
#' # CPH-like model
#' m_linear = pamm(formula = ped_status ~
#'     matrix +
#'     s(tend) +
#'     s(Id, bs = "re"),
#'     data = ped,
#'     offset = offset,  engine = "bam", method = "fREML",  discrete = TRUE)
#' summary(m_linear)
#'
#' # Time-varying covariate effect
#' m_tv = pamm(formula = ped_status ~
#'     s(tend, bs = "cr") +
#'     s(tend, by = as.ordered(matrix), bs = "cr") +
#'     s(Id, bs = "re"),
#'     data = ped,
#'     offset = offset,  engine = "bam", method = "fREML",  discrete = TRUE)
#' summary(m_tv)
#'
#' #####
#' # Prediction plots
#' #####
#'
#' # Null model
#' ndf_null <- ped %>%
#'    make_newdata(tend = unique(tend)) %>%
#'    add_hazard(m_null, exclude = c("s(id)"))
#' p_null = ggplot(ndf_null, aes(x = tend, y = hazard)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = .3) +
#'   xlab("Time after white-tailed deer (day)") +
#'   ylab(~paste("Coyote event number.Day"^-1,".Camera-Trap"^-1))
#' print(p_null)
#'
#' # Time-varying covariate effect
#' ndf_tv <- ped %>%
#'    make_newdata(tend = unique(tend), matrix = unique(matrix)) %>%
#'    add_hazard(m_tv, exclude = c("s(id)"))#'
#' p_tv = ggplot(ndf_tv, aes(x = tend, y = hazard, colour = matrix)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = .3) +
#'   scale_color_manual(values = c("#999999", "#E69F00"))  +
#'   xlab("Time after white-tailed deer (day)") +
#'   ylab(~paste("Coyote event number.Day"^-1,".Camera-Trap"^-1))
#' print(p_tv)
#'
#' }
#'
#' End_Survey (when camera is removed from study entirely (doesn't work)) -> could be different for each site

recurrent = function(df, Primary, Secondary, End_survey, survey_duration = 10) { # Tertiarry
  data = df %>%
    mutate_if(is.factor, as.character) %>%
    filter(.data[["Species"]] %in% c(Primary, Secondary) & DateTime <= End_survey) %>%
    group_by(Site) %>% # different camera trap locations (multiple surveys per location possible)
    arrange(DateTime, .by_group = TRUE) %>%

    # ID of the primary event (Site + Primary event number) # Survey-ID
    mutate(PredObs = ifelse(Species %in% Primary, 1, 0) %>% cumsum(),# PreditorObs (-> Primary-ID)
           Id = paste0(Site, "-", PredObs)) %>%
    # Remove secondary events before the first primary events
    filter(PredObs != 0) %>%
    group_by(PredObs, .add = T) %>%

    # Calculate t.start and t.stop
    mutate(t.stop = ifelse(is.na(lag(DateTime)), 0, # Primary event: t.stop = 0
                           ifelse(lag(DateTime) == DateTime, difftime(DateTime+1, lag(DateTime), units  = "days"), # Ties forbidden -> add 1 sec
                                  difftime(DateTime, lag(DateTime), units  = "days"))) %>% cumsum(),               # Secondary events: t.stop = cumulative time between previous events
           t.start = lag(t.stop)) %>%

    # Event type (now = species, later will be change by the type of censoring event if needed)
    mutate(Event_type = Species) %>%

    dplyr::select(c(Site, PredObs, Id, DateTime, Species, t.start, t.stop, Event_type)) %>%
    dplyr::ungroup(PredObs) %>%
    arrange(Site, DateTime)


  # Another primary event stops the survey
  ## Secondary events in-between (censoring event = "Following primary")
  data %<>% {bind_rows(.,  filter(., Species %in% Secondary & lead(Species) %in% Primary) %>%
                         mutate(Event_type = "Censoring event - Following Primary"))} %>%
    arrange(Site, DateTime) %>%
    mutate(DateTime = if_else(Event_type == "Censoring event - Following Primary",  lead(DateTime), DateTime),       # t.stop censoring event  = time stamp of following primary event
           t.start = ifelse(Event_type == "Censoring event - Following Primary", lag(t.stop), t.start),              # t.start censoring event = t.stop of last secondary event
           t.stop = ifelse(Event_type == "Censoring event - Following Primary", difftime(DateTime, lag(DateTime), units  = "days") + t.start, t.stop)) %>% # Censoring event t.stop = time difference between last Secondary event and following Primary event

    ## No secondary events in-between (censoring event = "No Secondary")
    mutate(Event_type = ifelse(Species %in% Primary & lead(Species) %in% Primary, "Censoring event - No Secondary", Event_type),
           t.start = ifelse(Event_type == "Censoring event - No Secondary", 0, t.start),
           t.stop = ifelse(Event_type == "Censoring event - No Secondary", difftime(lead(DateTime), DateTime, units  = "days"), t.stop)) %>% # t.start = 0, and t.stop = difftime(primary, following primary)
    # Add event information (1/0)
    mutate(event = ifelse(Event_type %in% Secondary, 1, 0), status = 0)  # Event status = 1 (event) or 0 (censoring)



  # Add primary event information
  ## Primary species name
  pred.obs = data %>%
    group_by(Id) %>%
    slice(1) %>%
    dplyr::select(Id, Species) %>%
    dplyr::rename(Primary = Species)
  data = left_join(data, pred.obs)
  ## Primary species DateTime (required for PlotEvent function in calendar time)
  data %<>% group_by(Id) %>% mutate(DateTime_Primary = DateTime[1])

  # Remove primary events
  #data %<>% subset(Event_type == Secondary | Event_type == "Censoring event - No Secondary" | Event_type == "Censoring event - Following Primary")%>% droplevels()
  data %<>% subset(!(Species %in% Primary))%>% droplevels()

  # Secondary event number
  data %<>% group_by(Id) %>%
    mutate(enum = row_number()) %>%
    ungroup() %>%
    as.data.frame()

  data %<>% dplyr::select(Site, Id, Primary, DateTime_Primary, DateTime, t.start, t.stop, event, enum, status, Event_type)  %>%
    droplevels()

  # Remove event after survey duration limit
  data %<>% filter(t.start < survey_duration) %>%
    mutate(event  = ifelse(t.stop > survey_duration, 0, event),
           Event_type = ifelse(t.stop > survey_duration, "Censoring event - Survey end", Event_type),                 # Censoring event
           t.stop = ifelse(t.stop > survey_duration, survey_duration, t.stop))  # t.stop of censoring event = survey duration

  # Merge with environmental information from initial dataset
  data = left_join(data, df, join_by(Site == Site, DateTime == DateTime),
    relationship = "many-to-many")
  data %<>% select(-Species) %>%
    mutate(Id = as.factor(Id))
  return(data)

}
