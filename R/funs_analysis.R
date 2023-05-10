library(marginaleffects)
suppressPackageStartupMessages(library(tidybayes))

# brms cleans up names for Stan with base::make.names() + removing .s and _s
# https://github.com/paul-buerkner/brms/issues/442#issuecomment-392520245
brms_names <- function(x) {
  gsub("\\.|\\_", "", make.names(x))
}

extract_clean_draws <- function(model) {
  var_name <- labels(terms(as.formula(model$formula)))
  
  group_names <- tibble(nice_name = levels(model$data[[var_name]])) %>% 
    mutate(.variable = paste0("b_", var_name, brms_names(nice_name)))
  
  model %>% 
    gather_draws(`^b_.*`, regex = TRUE) %>% 
    ungroup() %>% 
    left_join(group_names, by = join_by(.variable))
}

extract_diffs <- function(model) {
  var_name <- labels(terms(as.formula(model$formula)))
  var_list <- setNames(list("pairwise"), var_name)
  
  model %>% 
    avg_comparisons(variables = var_list, type = "link") %>% 
    posterior_draws()
}

extract_diffs_summary <- function(diffs) {
  diffs %>% 
    summarize(post_median = median_qi(draw, .width = 0.95),
              p_greater_0 = sum(draw > 0) / n()) %>% 
    unnest(post_median)
}

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
    family = binomial(link = "identity"),
    prior = c(prior(beta(5, 5), class = b, ub = 1, lb = 0)),
    chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED
  )
  
  df_with_models <- x$df_activities_collapsed %>%
    select(-total, -prop) %>%
    group_by(question, potential.contentiousness) %>%
    mutate(total = sum(num)) %>%
    group_by(question, response) %>% 
    nest() %>% 
    mutate(model = map(data, ~{
      update(initial_model, 
             formula = bf(num | trials(total) ~ 0 + potential.contentiousness),
             newdata = .x)
    })) %>% 
    mutate(draws = map(model, extract_clean_draws)) %>% 
    mutate(diffs = map(model, extract_diffs)) %>% 
    mutate(diffs_summary = map(diffs, extract_diffs_summary))
  
  return(df_with_models)
}
