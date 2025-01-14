#load packages
library(tidyverse)
library(infer)
library(ggpubr)

# data --------------------------------------------------------------------
data_sbeet_yield <- 
  read.csv("http://rstats4ag.org/data/sugarbeet.csv") %>% 
  janitor::clean_names() %>% 
  mutate(yield = round(yield*2.24,1),
         type = as.factor(type))

# explore -----------------------------------------------------------------
data_sbeet_yield %>% 
  gghistogram(x = "yield",
              add = "mean",
              rug = "TRUE",
              color = "type",
              fill = "type")

data_sbeet_yield %>% 
  ggboxplot(x = "type",
            y = "yield",
            fill = "type",
            add = "jitter")+
  stat_compare_means()

# approach 1 --------------------------------------------------------------

#calculate the difference between the groups
sbeet_observed_stat <- 
  data_sbeet_yield %>% 
  specify(yield ~ type) %>% 
  calculate(stat = "diff in means", 
            order = c("R", "C")) #mean of R - mean of C

#generate the null distribution with randomization
null_dist_two_sample <- 
  data_sbeet_yield %>% 
  specify(yield~type) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("R","C"))

#plot the results
null_dist_two_sample %>% 
  visualize() +
  shade_p_value(sbeet_observed_stat,
                direction = "two-sided")

#calculate p-value
null_dist_two_sample %>% 
  get_p_value(obs_stat = sbeet_observed_stat,
              direction = "two-sided")

