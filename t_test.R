
# data        --------------------------------------------------------------------
data_yield <- 
  tibble(
    yield = c(2280,2690,2080,2820,1340,2080,2480,2420,2150,1880),
    mu = rep(1810,10))

# mu t-test   ---------------------------------------------------------------
#load packages
library(tidyverse)
library(infer)


# approach 1  --------------------------------------------------------------

data_yield %>%  t_test(response = yield, mu = 1810)


# approach 2  --------------------------------------------------------------

#observed stat calculations
observed_stat <- 
  data_yield %>% 
  specify(response = yield) %>% 
  calculate(stat = "mean")

observed_stat

#generates a null distribution
null_dist_one_sample <- 
  data_yield %>% 
  specify(response = yield) %>% 
  hypothesize(null = "point", mu = 1810) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

null_dist_one_sample

#plot null distribution and shade_p_value
null_dist_one_sample %>% 
  visualize()+
  shade_p_value(observed_stat, direction = "two-sided")

#calculate p-value
p_value_one_sample <- 
  null_dist_one_sample %>% 
  get_p_value(obs_stat = observed_stat,
              direction = "two-sided")

# approch 3   ---------------------------------------------------------------

observed_stat_3 <-
  data_yield %>% 
  specify(response = yield) %>% 
  hypothesize(null = "point", mu = 1810) %>% 
  calculate(stat = "t") %>% 
  pull() 

#p_value
pt(unname(observed_stat_3), df = nrow(data_yield)-1, lower.tail = FALSE)*2
