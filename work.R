nodes <- g1 %>% activate(nodes) %>% as_tibble()

dyads <- g1 %>% activate(edges) %>% 
         as_tibble() %>% 
         filter(!is.na(edge_latent_community)) %>%
         select(from, to, sgn, edge_latent_community)

dyads <- dyads %>% mutate(A = )

write.csv(as_tibble(the_model$connectParam), "Model parameters.csv")
