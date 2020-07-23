# this script contains functions for PCA

# variance explained
var_table <- function() {
  if (n_comp == 1) {
    a <- data.frame(component = 1:length(fit$communality),
                    eigen = fit$values,
                    prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    cum_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0))) %>% 
      round(2)
  } else {
    a <- data.frame(component = 1:length(fit$communality),
                    eigen = fit$values,
                    prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    cum_var = c(fit$Vaccounted[3,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0))) %>% 
      round(2)
  
  }
  fit$Vaccounted[2,c(1:n_comp)]
  a[a == 0] <- ""
  
  a %>% 
    kable(booktabs = T, caption = "Variance accounted for by components") %>%
    kable_styling(font_size = 6, latex_options = "HOLD_position")
}


# pattern matrix
pattern_matrix <- function() {
  
  load <- data.frame(var = rownames(fit$loadings),
                     PC1 = round(fit$loadings[1:length(fit$communality)], 2),
                     PC2 = round(fit$loadings[(1+length(fit$communality)):(length(fit$communality)*2)], 2),
                     PC3 = round(fit$loadings[(1+length(fit$communality)*2):(length(fit$communality)*3)], 2),
                     PC4 = round(fit$loadings[(1+length(fit$communality)*3):(length(fit$communality)*4)], 2),
                     PC5 = round(fit$loadings[(1+length(fit$communality)*4):(length(fit$communality)*5)], 2),
                     PC6 = round(fit$loadings[(1+length(fit$communality)*5):(length(fit$communality)*6)], 2),
                     PC7 = round(fit$loadings[(1+length(fit$communality)*6):(length(fit$communality)*7)], 2),
                     h2 = round(fit$communality, 2)) %>% 
    mutate(PC1 = ifelse(PC1 < .3 & PC1 > -.3, "", PC1),
           PC2 = ifelse(PC2 < .3 & PC2 > -.3, "", PC2),
           PC3 = ifelse(PC3 < .3 & PC3 > -.3, "", PC3),
           PC4 = ifelse(PC4 < .3 & PC4 > -.3, "", PC4),
           PC5 = ifelse(PC5 < .3 & PC5 > -.3, "", PC5),
           PC6 = ifelse(PC6 < .3 & PC6 > -.3, "", PC6),
           PC7 = ifelse(PC7 < .3 & PC7 > -.3, "", PC7)) %>% 
    arrange(desc(PC1), desc(PC2), desc(PC3), desc(PC4), desc(PC5), desc(PC6), desc(PC7)) %>% 
    select(var, PC1:paste0("PC", n_comp), h2)
  
  load %>% 
    kable(booktabs = T, caption = "Pattern Matrix") %>%
    kable_styling(font_size = 6, latex_options = "HOLD_position")
}


# correlations between components
pca_cor <- function() {
round(fit$r.scores,2) %>% 
  kable(booktabs = T, caption = "Correlations between components") %>%
  kable_styling(font_size = 6, latex_options = "HOLD_position")
}