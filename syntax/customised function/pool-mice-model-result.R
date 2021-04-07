get_pooled_estiamte <- function(.mice_object){
  .mice_object %>% 
    pool() %>%
    summary(exponentiate = T, conf.int = T) %>% 
    as_tibble() %>% 
    select(term, estimate, p.value) %>% 
    # mutate_at(vars(estimate:p.value), ~ round(., 2)) %>% 
    mutate_at(vars(p.value), ~ round(., 3)) %>% 
    mutate(sig_star = case_when(p.value < 0.01 ~ "***",
                                between(p.value, 0.01, 0.05) ~ "**",
                                between(p.value, 0.05, 0.10) ~ "*",
                                TRUE ~ ""))
}
