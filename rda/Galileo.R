library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)

falling_object <- rfalling_object()

#measurement error models.

#Imagine Galileo trying to describe the velocity of a falling object.  An assistant climbs the Tower of Pisa and 
#drops the ball.  While several other assistants record the position at different times.  The assistant
#hands the data to Galileo and this is what he sees.
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters")+
  xlab("Time in seconds")

#Now, to start making preditions about other falling objects, Galieo needs actual numbers, rather than the 
#unknown parameters.  We will use LSE.  The lm() function will find the betas that minimize the residual sum of
#squares.  We use this code to obtain our estimated parameters.
fit <- falling_object %>%
  mutate(y = observed_distance,time_sq = time^2) %>%
  lm(y~time+time_sq, data=.)
tidy(fit)

#To check if the estimated parabola fits the data, the augment() function lets us do this easily.
augment(fit) %>%
  ggplot() +
  geom_point(aes(time,y)) +
  geom_line(aes(time, .fitted))

#Dropping the ball, that means the starting velocity is 0 because we start just by dropping it from the 
#Tower of Pisa, which has a height of about 56.67 meters. These known quantities are consistent with the 
#parameters that we estimated, which we can see using the tidy function.
tidy(fit, conf.int = TRUE)


