library(targets)
library(tarchetypes)
library(tibble)

# Set the _targets store so that scripts in subdirectories can access targets
# without using withr::with_dir() (see https://github.com/ropensci/targets/discussions/885)
#
# This hardcodes the absolute path in _targets.yaml, so to make this more
# portable, we rewrite it every time this pipeline is run (and we don't track
# _targets.yaml with git)
tar_config_set(
  store = here::here("_targets"),
  script = here::here("_targets.R")
)

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE,
  contrasts = rep("contr.treatment", 2)  # Treat ordered factors as treatments in models
)

set.seed(71215) # From random.org

# Bayesian stuff
suppressPackageStartupMessages(library(brms))
options(
  mc.cores = 4,
  brms.backend = "cmdstanr"
)

BAYES_SEED <- 907680 # From random.org
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000

tar_option_set(
  packages = c("tidyverse"),
  format = "qs"
)

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Load R scripts with functions to use in the pipeline
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

# Actual pipeline ---------------------------------------------------------
list(
  ## Raw data files ----
  tar_target(survey_orgs_file,
    here_rel("data", "raw-data", "survey_orgs_clean.rds"),
    format = "file"
  ),
  tar_target(survey_countries_file,
    here_rel("data", "raw-data", "survey_countries_clean.rds"),
    format = "file"
  ),
  tar_target(survey_all_file,
    here_rel("data", "raw-data", "survey_clean_all.rds"),
    format = "file"
  ),
  tar_target(naturalearth_raw_file,
    here_rel(
      "data", "raw-data", "ne_110m_admin_0_countries",
      "ne_110m_admin_0_countries.shp"
    ),
    format = "file"
  ),

  ## Process and clean data ----
  tar_target(survey_orgs, clean_survey_orgs(survey_orgs_file)),
  tar_target(survey_countries, clean_survey_countries(survey_countries_file)),
  tar_target(survey_all, clean_survey_all(survey_all_file)),
  tar_target(world_map, load_world_map(naturalearth_raw_file)),

  ## Graphics ----
  tar_target(graphic_functions, lst(
    theme_ingo, theme_ingo_map,
    set_annotation_fonts, clrs, build_span
  )),
  tar_target(table_functions, lst(opts_int, opts_theme)),

  ## Analysis ----
  tar_target(summary_activities, make_activities_summary(survey_orgs)),
  tar_target(models_activities, make_activities_models(summary_activities)),
  tar_target(
    models_strategies_size,
    make_strategies_size_models(survey_all)
  ),

  tar_target(df_govt_positivity_regime, make_govt_positivity_regime(survey_countries)),
  tar_target(
    models_govt_positivity_regime,
    make_govt_positivity_regime_models(df_govt_positivity_regime)
  ),

  tar_target(df_govt_positivity_issue, make_govt_positivity_issue(survey_countries)),
  tar_target(
    models_govt_positivity_issue,
    make_govt_positivity_issue_models(df_govt_positivity_issue)
  ),

  tar_target(df_familiarity_regime, make_familiarity_regime(survey_countries)),
  tar_target(
    models_familiarity_regime,
    make_familiarity_regime_models(df_familiarity_regime)
  ),

  tar_target(df_familiarity_issue, make_familiarity_issue(survey_countries)),
  tar_target(
    models_familiarity_issue,
    make_familiarity_issue_models(df_familiarity_issue)
  ),

  tar_target(df_frequency_regime, make_frequency_regime(survey_countries)),
  tar_target(
    models_frequency_regime,
    make_frequency_regime_models(df_frequency_regime)
  ),

  tar_target(df_registration_regime, make_registration_regime(survey_countries)),
  tar_target(
    models_registration_regime,
    make_registration_regime_models(df_registration_regime)
  ),

  tar_target(df_registration_issue, make_registration_issue(survey_countries)),
  tar_target(
    models_registration_issue,
    make_registration_issue_models(df_registration_issue)
  ),

  # Manuscript and analysis notebook ----
  tar_quarto(output_nice, path = "manuscript", quiet = FALSE, profile = "nice"),
  tar_quarto(output_ms, path = "manuscript", quiet = FALSE, profile = "ms"),
  tar_quarto(website, path = ".", quiet = FALSE),
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy, {
    # Force a dependency
    website
    # Run the deploy script
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  })
)
