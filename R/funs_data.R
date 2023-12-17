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

clean_dcjw <- function(path) {
  # This is copied from my older dissertation R code: 
  # https://github.com/andrewheiss/Dissertation/blob/master/Data/R/clean_merge_data.R#L1044
  # I didn't really update it to more modern tidyverse R (i.e. it still uses 
  # gather() and separate())
  library(readxl)
  library(countrycode)
  
  dcjw <- read_excel(path)[, 1:50] %>%
    select(-c(
      dplyr::contains("source"), dplyr::contains("burden"),
      dplyr::contains("subset"), Coder, Date
    )) %>%
    gather(key, value, -Country) %>%
    separate(key, c("question", "var.name"), 4) %>%
    filter(!is.na(Country)) %>%
    mutate(var.name = ifelse(var.name == "", "value", gsub("_", "", var.name))) %>%
    spread(var.name, value) %>%
    mutate(
      year.actual = ymd(paste0(year, "-01-01"), quiet = TRUE),
      country.name = countrycode(Country, "country.name", "country.name")
    )
  
  return(dcjw)
}

clean_gwf <- function(path) {
  # GWF Autocratic regimes
  # http://sites.psu.edu/dictators/
  # This is included in V-Dem, but only `gwf_regimetype` and `gwf_nonautocracy`
  regime.types.simple <- tribble(
    ~gwf.unified, ~gwf.simple, ~gwf.binary,
    "democracy", "democracy", "democracy",
    "provisional", "other", "",
    "foreign-occupied", "other", "",
    "warlord", "other", "",
    "not-independent", "other", "",
    "warlord/foreign-occupied", "other", "",
    "personal", "autocracy", "autocracy",
    "party-personal", "autocracy", "autocracy",
    "military", "autocracy", "autocracy",
    "military-personal", "autocracy", "autocracy",
    "party", "autocracy", "autocracy",
    "indirect military", "autocracy", "autocracy",
    "party-military", "autocracy", "autocracy",
    "oligarchy", "autocracy", "autocracy",
    "party-military-personal", "autocracy", "autocracy",
    "monarchy", "autocracy", "autocracy"
  )
  
  gwf <- haven::read_stata(path) %>%
    mutate(across(c(cowcode, year), as.numeric)) %>%
    mutate(
      across(c(
        gwf_casename, gwf_country, gwf_regimetype,
        gwf_next, gwf_prior, gwf_nonautocracy
      ), ~ ifelse(.x == "NA", NA, .x))
    ) %>%
    mutate(gwf.unified = ifelse(is.na(gwf_regimetype),
      gwf_nonautocracy, gwf_regimetype
    ))
  
  gwf.simplified <- gwf %>%
    left_join(regime.types.simple, by = "gwf.unified") %>%
    select(cowcode, year, gwf.unified, gwf.simple, gwf.binary) %>%
    mutate(
      gwf.simple = factor(gwf.simple,
        levels = c("autocracy", "democracy", "other"),
        labels = c("Autocracy", "Democracy", "Other")
      ),
      gwf.binary = factor(gwf.binary,
        levels = c("autocracy", "democracy"),
        labels = c("Autocracy", "Democracy")
      )
    ) %>%
    filter(year > 1990)
  
  # GWF data only goes to 2010, but V-Dem goes to 2015. 
  # Extend the gwf.ever.autocracy variable to include 2015 by creating a skeleton
  # panel of all cowcodes and years + 2011:2015, joining gwf.simplified, and then
  # creating gwf.ever.autocracy
  gwf.simplfied.extended <- expand.grid(
    cowcode = unique(gwf.simplified$cowcode),
    year = 1991:2015
  ) %>%
    left_join(gwf.simplified, by = c("cowcode", "year")) %>%
    group_by(cowcode) %>%
    mutate(gwf.ever.autocracy = any(gwf.binary == "Autocracy", na.rm = TRUE)) %>%
    ungroup()
  
  return(gwf.simplfied.extended)
}
