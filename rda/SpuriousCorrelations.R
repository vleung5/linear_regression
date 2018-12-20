
#Spurious Correlations

#Let's perform a monte carlo simulation to show how data dredging (cherry picking) can result in finding high
#correlations among variables that are theoretically uncorrelated.

#This simulated one million groups, each with 25 observations.
#For each group, we generate 25 observations which are stored in the second and third column.

N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

#Then we compute the correlation between x and y for each group, and look for the maximum.
res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(X,Y)) %>%
  arrange(desc(r))

res

#we can just plot the data from this group, it shows a convincing plot that x and y are correlated.
sim_data %>% filter(group == res$group[which.max(res$r)]) %>% 
  ggplot(aes(X,Y)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#Remember that the correlations number is a random variable.
#Here is the distirubiton we just generated with our Monte Carlo simulation
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")


#If we performed regression on this group and interpreted the p-value, we would incorrectly claim
#this was a statistically significant relation.
sim_data %>%
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~X, data = .)))
#Look how small the p-value is.



