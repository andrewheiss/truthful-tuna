suppressPackageStartupMessages(library(tidybayes))

# Functions for extracting draws and calculating pairwise differences ----
extract_posterior_draws <- function(model, prop = FALSE) {
  if (prop) {
    post <- model %>% 
      epred_draws(newdata = model$data) %>% 
      mutate(.epred_count = .epred,
             .epred = .epred_count / total)
  } else {
    var_name <- labels(terms(as.formula(model$formula)))
    
    # Create a tibble with a column that has the value of a variable with
    # `"{blah}" := x`, or walrus operator + glue syntax
    post <- model %>% 
      epred_draws(newdata = tibble("{var_name}" := unique(model$data[[var_name]])))
  }
  
  return(post)
}

extract_diffs <- function(model, prop = FALSE) {
  post <- extract_posterior_draws(model, prop = prop)
  
  var_name <- labels(terms(as.formula(model$formula)))

  post %>% 
    ungroup() %>% 
    compare_levels(.epred, by = var_name,
                   comparison = "pairwise")
}

extract_diffs_summary <- function(diffs) {
  diffs %>% 
    summarize(post_median = median_qi(.epred, .width = 0.95),
              p_greater_0 = sum(.epred > 0) / n()) %>% 
    unnest(post_median) %>% 
    mutate(type = "Differences")
}


# Functions for specific questions ----
make_activities_summary <- function(x) {
  labels_activities <- tribble(
    ~levels, ~labels, 
    "aid", "Providing direct aid and services",
    "education", "Engaging in research and public education",
    "mobilize", "Mobilizing people",
    "advocacy", "Engaging in advocacy",
    "monitor", "Monitoring and assessing the effects of policies"
  )
  
  df_activities <- x %>%
    select(potential.contentiousness, contains("Q3.3"), -contains("TEXT")) %>%
    gather(question, response, -potential.contentiousness) %>%
    mutate(question = str_replace(question, "Q3\\.3_", ""),
           question = factor(question, levels = labels_activities$levels,
                             labels = labels_activities$labels, ordered = TRUE)) %>%
    filter(!(response %in% c("Don't know", "Not applicable"))) %>%
    group_by(question, response, potential.contentiousness) %>%
    summarise(num = n()) %>%
    ungroup() %>%
    mutate(response = factor(response, levels = levels(x$Q3.3_aid), ordered = TRUE))
  
  df_activities_collapsed <- df_activities %>%
    mutate(response = fct_collapse(response,
                                   "Almost always" = c("Always", "Most of the time"),
                                   "Sometimes" = c("About half the time", "Sometimes"),
                                   "Never" = "Never"
    )) %>%
    group_by(question, response, potential.contentiousness) %>%
    summarise(num = sum(num)) %>%
    group_by(potential.contentiousness, question) %>%
    mutate(total = sum(num)) %>%
    ungroup() %>%
    mutate(prop = num / total) %>%
    arrange(response, desc(potential.contentiousness), desc(prop)) %>%
    mutate(question = fct_inorder(question),
           response = fct_drop(response))
  
  return(lst(df_activities, df_activities_collapsed))
}

make_activities_models <- function(x) {
  # To avoid recompiling the model dozens of times, we'll make an intercept-only
  # model first here, then use update() to re-run it with actual data
  initial_model <- brm(
    bf(num | trials(total) ~ 0 + Intercept), 
    data = list(num = 5, total = 5),
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  df_with_models <- x$df_activities_collapsed %>%
    group_by(question, response) %>% 
    nest() %>% 
    mutate(model = map(data, ~{
      update(initial_model, 
             formula = bf(num | trials(total) ~ 0 + potential.contentiousness),
             newdata = .x)
    })) %>% 
    mutate(draws = map(model, extract_posterior_draws, prop = TRUE)) %>% 
    mutate(diffs = map(model, extract_diffs, prop = TRUE)) %>% 
    mutate(diffs_summary = map(diffs, extract_diffs_summary))
  
  return(df_with_models)
}

make_strategies_size_models <- function(survey_all) {
  df_strategies_size <- survey_all %>%
    mutate(num_strategies = Q4.3_value %>% map_int(length)) %>%
    filter(!is.na(Q4.3_value), !is.na(small_org))
  
  model <- brm(
    bf(num_strategies ~ 0 + small_org),
    data = df_strategies_size,
    family = gaussian(),
    prior = c(prior(normal(1, 3), class = b, lb = 0)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model)
  diffs <- extract_diffs(model)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_strategies_size, model, draws, diffs, diffs_summary))
}

make_govt_positivity_regime <- function(x) {
  df_govt_positivity_regime <- x %>%
    select(Q4.11_collapsed, target.regime.type) %>%
    filter(!is.na(Q4.11_collapsed), Q4.11_collapsed != "NULL") %>% 
    mutate(Q4.11_collapsed = fct_drop(Q4.11_collapsed))

  return(df_govt_positivity_regime)
}

make_govt_positivity_regime_models <- function(x) {
  df_govt_positivity_regime_collapsed <- x %>% 
    group_by(Q4.11_collapsed, target.regime.type) %>%
    summarize(num = n()) %>% 
    group_by(Q4.11_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(target.regime.type == "Autocracy")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.11_collapsed),
    data = df_govt_positivity_regime_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_govt_positivity_regime_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_govt_positivity_issue <- function(x) {
  df_govt_positivity_issue <- x %>%
    select(Q4.11_collapsed, potential.contentiousness) %>%
    filter(!is.na(Q4.11_collapsed), Q4.11_collapsed != "NULL") %>% 
    mutate(Q4.11_collapsed = fct_drop(Q4.11_collapsed))
  
  return(df_govt_positivity_issue)
}

make_govt_positivity_issue_models <- function(x) {
  df_govt_positivity_issue_collapsed <- x %>% 
    group_by(Q4.11_collapsed, potential.contentiousness) %>%
    summarize(num = n()) %>% 
    group_by(Q4.11_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(potential.contentiousness == "High contention")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.11_collapsed),
    data = df_govt_positivity_issue_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_govt_positivity_issue_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_familiarity_regime <- function(x) {
  df_familiarity_regime <- x %>%
    select(Q4.13_collapsed, target.regime.type) %>%
    filter(!is.na(Q4.13_collapsed), Q4.13_collapsed != "NULL") %>% 
    mutate(Q4.13_collapsed = fct_drop(Q4.13_collapsed))
  
  return(df_familiarity_regime)
}

make_familiarity_regime_models <- function(x) {
  df_familiarity_regime_collapsed <- x %>% 
    group_by(Q4.13_collapsed, target.regime.type) %>%
    summarize(num = n()) %>% 
    group_by(Q4.13_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(target.regime.type == "Autocracy")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.13_collapsed),
    data = df_familiarity_regime_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_familiarity_regime_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_familiarity_issue <- function(x) {
  df_familiarity_issue <- x %>%
    select(Q4.13_collapsed, potential.contentiousness) %>%
    filter(!is.na(Q4.13_collapsed), Q4.13_collapsed != "NULL") %>% 
    mutate(Q4.13_collapsed = fct_drop(Q4.13_collapsed))
  
  return(df_familiarity_issue)
}

make_familiarity_issue_models <- function(x) {
  df_familiarity_issue_collapsed <- x %>% 
    group_by(Q4.13_collapsed, potential.contentiousness) %>%
    summarize(num = n()) %>% 
    group_by(Q4.13_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(potential.contentiousness == "High contention")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.13_collapsed),
    data = df_familiarity_issue_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_familiarity_issue_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_frequency_regime <- function(x) {
  df_frequency_regime <- x %>%
    select(Q4.14_collapsed, target.regime.type) %>%
    filter(!is.na(Q4.14_collapsed), Q4.14_collapsed != "NULL") %>% 
    mutate(Q4.14_collapsed = fct_drop(Q4.14_collapsed))
  
  return(df_frequency_regime)
}

make_frequency_regime_models <- function(x) {
  df_frequency_regime_collapsed <- x %>% 
    group_by(Q4.14_collapsed, target.regime.type) %>%
    summarize(num = n()) %>% 
    group_by(Q4.14_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(target.regime.type == "Autocracy")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.14_collapsed),
    data = df_frequency_regime_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_frequency_regime_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_registration_regime <- function(x) {
  df_registration_regime <- x %>%
    select(Q4.4_collapsed, target.regime.type) %>%
    filter(!is.na(Q4.4_collapsed), Q4.4_collapsed != "NULL") %>% 
    mutate(Q4.4_collapsed = fct_drop(Q4.4_collapsed))
  
  return(df_registration_regime)
}

make_registration_regime_models <- function(x) {
  df_registration_regime_collapsed <- x %>% 
    group_by(Q4.4_collapsed, target.regime.type) %>%
    summarize(num = n()) %>% 
    group_by(Q4.4_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(target.regime.type == "Autocracy")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.4_collapsed),
    data = df_registration_regime_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_registration_regime_collapsed, 
             model, draws, diffs, diffs_summary))
}

make_registration_issue <- function(x) {
  df_registration_issue <- x %>%
    select(Q4.4_collapsed, potential.contentiousness) %>%
    filter(!is.na(Q4.4_collapsed), Q4.4_collapsed != "NULL") %>% 
    mutate(Q4.4_collapsed = fct_drop(Q4.4_collapsed))
  
  return(df_registration_issue)
}

make_registration_issue_models <- function(x) {
  df_registration_issue_collapsed <- x %>% 
    group_by(Q4.4_collapsed, potential.contentiousness) %>%
    summarize(num = n()) %>% 
    group_by(Q4.4_collapsed) %>%
    mutate(total = sum(num)) %>% 
    filter(potential.contentiousness == "High contention")
  
  model <- brm(
    bf(num | trials(total) ~ 0 + Q4.4_collapsed),
    data = df_registration_issue_collapsed,
    family = stats::binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, lb = 0, ub = 1)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED, refresh = 0
  )
  
  draws <- extract_posterior_draws(model, prop = TRUE)
  diffs <- extract_diffs(model, prop = TRUE)
  diffs_summary <- extract_diffs_summary(diffs)
  
  return(lst(data = df_registration_issue_collapsed, 
             model, draws, diffs, diffs_summary))
}
