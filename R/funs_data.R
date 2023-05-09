# Country-based data (*with* the Q4 loop)
clean_survey_all <- function(path) {
  x <- readRDS(path) %>% 
    mutate(small_org = Q3.4.num <= 50,
           small_org = factor(small_org, levels = c(TRUE, FALSE),
                              labels = c("0–50 employees", "51+ employees"),
                              ordered = TRUE)) 
  
  return(x)
}

# Organization-based data (without the Q4 loop)
clean_survey_orgs <- function(path) {
  x <- readRDS(path) %>% 
    mutate(small_org = Q3.4.num <= 50,
           small_org = factor(small_org, levels = c(TRUE, FALSE),
                              labels = c("0–50 employees", "51+ employees"),
                              ordered = TRUE)) 
  
  return(x)
}

# Country-based data (only the Q4 loop)
clean_survey_countries <- function(path) {
  x <- readRDS(path) %>% 
    mutate(Q4.4_collapsed =
             fct_recode(Q4.4, Registered = "Yes", `Not registered` = "No", 
                        NULL = "Don't know"),
           Q4.4_collapsed = ordered(Q4.4_collapsed)) %>%
    mutate(Q4.11_collapsed = 
             fct_collapse(Q4.11,
                          Negative = c("Extremely negative", "Somewhat negative"),
                          Neither = c("Neither positive nor negative"),
                          Positive = c("Somewhat positive", "Extremely positive"),
                          NULL = c("Don't know", "Prefer not to answer")),
           Q4.11_collapsed = ordered(fct_na_level_to_value(
             Q4.11_collapsed, 
             extra_levels = c("Don't know", "Prefer not to answer")))) %>%
    mutate(Q4.13_collapsed = 
             fct_collapse(Q4.13,
                          "Very familiar" = c("Extremely familiar", "Very familiar"),
                          "Somewhat familiar" = c("Moderately familiar", "Slightly familiar"),
                          "Not familiar at all" = c("Not familiar at all")),
           Q4.13_collapsed = ordered(fct_na_level_to_value(
             Q4.13_collapsed, 
             extra_levels = c("Don't know")))) %>%
    mutate(Q4.14_collapsed = 
             fct_collapse(Q4.14,
                          "At least once a year" = c("Once a month", "Once a year"),
                          "Once every few years" = c("Once every few years"),
                          "Rarely or never" = c("Rarely", "Never"),
                          NULL = c("Don't know")),
           Q4.14_collapsed = ordered(fct_na_level_to_value(
             Q4.14_collapsed, 
             extra_levels = c("Don't know")))) %>%
    mutate(Q4.17_collapsed = 
             fct_collapse(Q4.17,
                          "Not restricted" = c("Not restricted at all"),
                          "Very restricted" = c("Very restricted",
                                                "Extremely restricted"),
                          NULL = c("Don’t know")),
           Q4.17_collapsed = ordered(fct_na_level_to_value(
             Q4.17_collapsed, 
             extra_levels = c("Don't know"))))
  
  return(x)
}

load_world_map <- function(path) {
  suppressPackageStartupMessages(library(sf))
  
  world_map <- read_sf(path) %>%
    filter(ISO_A3 != "ATA")
  
  return(world_map)
}
