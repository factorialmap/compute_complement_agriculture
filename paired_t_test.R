
# packages ----------------------------------------------------------------
library(tidyverse)
library(infer)
library(rstatix)
library(ggpubr)

# data --------------------------------------------------------------------
data_sbeet_yield <- 
  read.csv("http://rstats4ag.org/data/sugarbeet.csv") %>% 
  janitor::clean_names() %>% 
  mutate(yield = round(yield*2.24,1),
         type = as.factor(type))

# approach 1 --------------------------------------------------------------
data_sbeet_yield %>% 
  t_test(yield~type, paired = TRUE)


# approach 2 --------------------------------------------------------------
data_sbeet_yield %>% 
  pairwise_t_test(formula = yield~type,
    p.adjust.method = "holm",
    paired = TRUE)


# approach 3 --------------------------------------------------------------

data_paired_test <- 
  tribble(~before, ~after,
          200.1,392.9,
          190.9,393.2,
          192.7,345.1,
          213,393,
          241.4,434,
          196.9,427.9,
          172.2,422,
          185.5,383.9,
          205.2,392.3,
          193.7,352.2)

data_paired_test %>% 
  ggpaired(cond1 = "before",
           cond2 = "after",
           fill = "condition",
           palette = "uchicago")


