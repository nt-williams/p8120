
library(tidyverse)
library(gmodels)
library(descr)

peptic <- tibble(
  spicy_food = c("yes", "yes", "no", "no"), 
  pud = c("yes", "no", "yes", "no"), 
  counts = c(16, 39, 10, 35)
)

x <- crosstab(peptic$spicy_food, peptic$pud, weight = peptic$counts, 
         expected = TRUE, chisq = TRUE, dnn = c("hi", "hello"))


glimpse(x)

x$tab %>% 
  oddsratio(method = "wald")



