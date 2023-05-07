library(targets)
library(tarchetypes)
library(tibble)

# Set the _targets store so that scripts in subdirectories can access targets
# without using withr::with_dir() (see https://github.com/ropensci/targets/discussions/885)
#
# This hardcodes the absolute path in _targets.yaml, so to make this more
# portable, we rewrite it every time this pipeline is run (and we don't track
# _targets.yaml with git)
tar_config_set(store = here::here('_targets'),
               script = here::here('_targets.R'))

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

set.seed(71215)  # From random.org

tar_option_set(packages = c("tidyverse"),
               format = "qs")

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}


# Actual pipeline ---------------------------------------------------------
list(
  tar_quarto(output_nice, path = "manuscript", quiet = FALSE, profile = "nice"),
  tar_quarto(output_ms, path = "manuscript", quiet = FALSE, profile = "ms")
)
