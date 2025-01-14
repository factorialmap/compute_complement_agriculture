#Is our yield different from the population yield?
data_yield %>% 
  t_test(response = yield,
         mu = 1810)

#Is the mpg different between groups v and s?
mtcars %>% 
  select(vs, mpg) %>% 
  mutate(vs = as.factor(vs)) %>% 
  group_by(vs) %>% 
  ungroup() %>% 
  summarize(t_test = t.test(mpg~vs)$p.val)

#how extract more info from this calculations?
mtcars %>% 
  summarize(ttest_mdl = list(t.test(mpg~vs))) %>% 
  mutate(res_ttest = map(ttest_mdl, broom::tidy)) %>% 
  unnest(res_ttest)


#how to plot this results
mtcars %>% 
  mutate(vs = as.factor(vs)) %>% 
  ggplot(aes(x = vs, y= mpg, fill = vs))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.3)+
  theme(legend.position = "none")

