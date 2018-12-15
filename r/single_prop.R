
library(tidyverse)
library(gmodels)
library(DescTools)

fractures <- tibble(
  x = 2, 
  y = 98
)

prop <- prop.test(fractures$x, 100, conf.level = .99, correct = FALSE)

broom::glance(prop)

prop.test(14, 100, p = 0.08, correct = FALSE)

binom.test(2, 100, p = 0.08, conf.level = .95)

BinomCI(14, 100, method = "wald")
