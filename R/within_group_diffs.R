
within_group_diffs_by_visit <- function(
  data,
  outcome,
  control_variables = c("age","gender")
) {

  if(all(is.na(data[[outcome]]))) return(NULL)

  control <- glue_collapse(control_variables, sep = " + ")

  mcall <- glue(
    "{outcome} ~ {outcome}_at_bl + {control} + pt_group * visit + (1|patient_id)"
  )

  model <- lmer(
    formula = as.formula(mcall),
    data = data
  )

  vis_comparison_estimates <- emmeans(model, ~ visit | pt_group)

  contrast(
    vis_comparison_estimates,
    method = 'revpairwise',
    adjust = 'none'
  ) %>%
    confint() %>%
    as_tibble() %>%
    separate(
      col = contrast,
      into = c('vis_after', 'vis_before'), sep = " - "
    ) %>%
    filter(vis_before == "Baseline", vis_after == 'Week 12') %>%
    mutate(
      tbl_value = glue(
        "{adapt_round(estimate)} ({adapt_round(lower.CL)}, {adapt_round(upper.CL)})"
      )
    ) %>%
    select(vis_before, vis_after, pt_group, tbl_value) %>%
    spread(pt_group, tbl_value)

}

###################################################################################################


within_group_diffs_by_visit_screening <- function(
  data,
  outcome,
  control_variables = c("age","gender")
) {

  if(all(is.na(data[[outcome]]))) return(NULL)

  control <- glue_collapse(control_variables, sep = " + ")

  mcall <- glue(
    "{outcome} ~ {outcome}_at_bl + {control} + pt_group * visit + (1|patient_id)"
  )

  model <- lmer(
    formula = as.formula(mcall),
    data = data
  )

  vis_comparison_estimates <- emmeans(model, ~ visit | pt_group)


  contrast(
    vis_comparison_estimates,
    method = 'revpairwise',
    adjust = 'none'
  ) %>%
    confint() %>%
    as_tibble() %>%
    separate(
      col = contrast,
      into = c('vis_after', 'vis_before'), sep = " - "
    ) %>%
    filter(vis_before == "Screening", vis_after == 'Week 12') %>%
    mutate(
      tbl_value = glue(
        "{adapt_round(estimate)} ({adapt_round(lower.CL)}, {adapt_round(upper.CL)})"
      )
    ) %>%
    select(vis_before, vis_after, pt_group, tbl_value) %>%
    spread(pt_group, tbl_value)

}


###################################################################################################



