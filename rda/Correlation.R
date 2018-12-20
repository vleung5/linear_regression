#install.packages("HistData")
library(HistData)
data("GaltonFamilies")

#head(GaltonFamilies,6)

#Create a data set with heights of fathers and the first sons
galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

#Is there a trend the taller the father, the taller the son?
galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

#Let's look at the correlation between father and sons' height
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% summarize(cor(father,son))

#The correlation is 0.5

#Let's assume that the 179 pris of fathers and sons is our entire population.
#A less fortunate geneticist can only afford to take a random sample of 25 pairs.
#Let's look at the sample correlation for this random sample where R is the random variable:
set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(cor(father, son))
R

#Let's run a monte-carlo simulation to see the distribution of this random variable.
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, 25, replace = TRUE) %>%
    summarize(r=cor(father,son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

#We see that the expected value is the population correlation, the mean of these Rs is 0.5,
#and that is has a relatively high standard error relative to its size, SD 0.147
mean(R)
sd(R)


#Let's use conditional average to predict the son's height based on the father's height
#The avg height is 70.5 in, but what if we are told the father is 72 in?
#We will stratify the father's side with conditional avg, by looking for fathers who are about 72 in.
#The problem is we don't have many fathers that are exactly 72 in, so let's round the heights to nearest inch.
conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% .$avg

conditional_avg

#This is 0.54 standard deviations larger than the average son, a smaller
#number than the 1.14 standard deviations taller that the father was above the average father.

galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

#The centers of these groups are increasing with height and the means of each group appear to follow
#a linear relationship.

d <- galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son))

p <-ggplot(d, aes(father, son_conditional_avg)) +
  geom_point() 

# Calculate slope and intercept of line of best fit
coef(lm(son_conditional_avg ~ father, data = d))

p + geom_abline(intercept = 37.25, slope = 0.477)

#The slope of the line is about 0.5, which happens to be the correlation between father 
#and son heights

#Let's plot the standardized heights against each other, son vs father, with a line
#that has a slope equal to the correlation
r <- galton_heights %>% summarize(r = cor(father, son)) %>% .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

#Let's look at the original data, father son data, and add the regression line
#and compute the intercept and the slope using the formulas we just derived
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

#If we plot the data in standard units then the regression line has intercept 0
#and slope rho
galton_heights %>%
  ggplot(aes(scale(father),scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

#If we think the height data is well approximated by the bivariate normal distribution,
#then we should see the normal approximation hold for each group.
#We stratify the son height by the standardized father heights and see that the
#assumption appears to hold.
galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)


#For regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

#To predict the father's height based on the son's
m <- r*s_x/s_y
b <- mu_x - m*mu_y


#Least Squares Estimates (LES) - the values that minimize the distance of the fitted 
#model to the data.
#Residual Sum of Squares (RSS)
#Let's write the function that computes the RSS for any pair of values, beta0 and beta1
#for our heights data.
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  return(sum(resid^2))
  }

#So for any pair of values, we get an RSS.  So this is a three-dimensional plot with
#beta1 and beta2 as x and y and the RSS as a z.  To find the minimum, you would have to 
#look at this three-dimensional plot.  Here, we're just going to make a two-dimensional
#version by keeping beta0 fixed at 25.


beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
  geom_line(aes(beta1, rss), col=2)


#This lm function gives us the least squares estimates, which we can see in the output of r.
fit <- lm(son ~ father, data = galton_heights)
fit


#Now run a Monte Carlo simulation in which we assume that the son and father height data
#that we have defines an entire population.  We will take a random sample of size 50
#and compute the regression slope coefficient for each one.
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    	lm(son ~ father, data = .) %>% .$coef
  })
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

#We can see the variability of the estimates by plotting their distribution.
#Here you can see the histogram of the estimated beta 0's and the estimated beta 1's.
#install.packages("gridExtra")
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol=2)

#Here are the estimated standard errors for one of our simulated data sets.
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary

#You coul dsee that the standard errors estimates reported by the summary functions
#are closed, so the standard errors that we obtain from our Monte Carlo simulation.
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#This plots confidence intervals around the predicted y hat (father's height).
#The line is the prediction and the band around them are the confidence intervals.
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")


#The predict function takes an lm object as input and returns these predictions.
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()

#If requested the standard errors and other information from which we can construct
#confidence intervals can be obtained from the predict function.
fit <- galton_heights %>% lm(son ~father, data = .)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
