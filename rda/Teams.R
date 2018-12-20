library(Lahman)
library(ggplot2)
library(dslabs)
ds_theme_set()
?Teams

#We see that the correlation between bases on balls and homerun is higher than other two
#It turns out that pitchers, afraid of homeruns, will sometimes avoid throwing strikes to homerun hitters.
#As a result, homerun hitters tend to have more bases on balls.
#Thus, a team with many homeruns will also have more bases on balls than average, and as a result,
#it may appear that bases on balls cause runs.
#But it is actually the homeruns that caused the runs.
#Bases on balls are CONFOUNDED with HR
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB,HR), cor(Singles, HR), cor(BB, Singles))


#To try to determine if bases on balls is still useful for creating runs, we will keep home runs
#fixed at a certain value and then examine the relationship between runs and bases on balls.
#Just as we stratified fathers by rounding to the cloest inch, we can stratify home runs per game
#to the closest 10th. We filtered our strata with few points.
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G,1),
           BB_per_game = BB/G,
           R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

#Make a scatter plot for each strata.
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

#The regression slope for predicting runs with bases on balls when we ignore home runs 
#was 0.735. We can see what the slopes are by using this code. We stratify by home run and then 
#compute the slope using the formula that we used previously.
dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))


#Let's check if, after stratifying by base on balls, we still see a home run effect or
#if it goes down.
#Now we swap home runs for bases on balls
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G,1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

#Make a scatter plot for each strata.
dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

#This shows the slope was not changed much from the original slope estimate of 1.84.
dat %>%
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))



install.packages("broom")
library(broom)
library(tidyverse)

#This shows that the confidence intervals overlap, which provides a nice visual confirmation
#that our assumption that the slopes do not change with home run strata, is relatively safe.
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
         filter(term == "BB") %>%
         select(HR, estimate, conf.low, conf.high) %>%
         ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
         geom_errorbar() + 
         geom_point()

#In trying to answer how well bases on balls predict runs, data exploration let us to 
#this model.  Here the data is approximately normal and conditional distributions
#were also normal.  Thus, we're justified to pose a linear model.
#Here, we need to let lm know we have two predictive variables. So we use the plus symbol 
#as follows.  Here's a code that fits that multiple regression model.

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

#Let's build a model that predicts runs based on all outcomes.
#Let's assume that these 5 variables are jointly normal.
#This means that if we pick any one of them and hold the other for fixed, the relationship
#with the outcome, runs per game, is linear.
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
           singles = (H-X2B-X3B-HR)/G,
           doubles = X2B/G,
           triples = X3B/G,
           HR = HR/G,
           R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)


#We can use the tidy funtion to see the coefficients, the standard errors and CI.
coefs <- tidy(fit, conf.int = TRUE)
coefs

#Here we predict the number of runs for each team in 2002.  Our model does a good job
#since points from the observed verses predicted plot fall close to the identity line.
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
           singles = (H-X2B-X3B-HR)/G,
           doubles = X2B/G,
           triples = X3B/G,
           HR = HR/G,
           R = R/G) %>%
  mutate(R_hat = predict(fit, newdata= .))%>%
  ggplot(aes(R_hat, R)) + 
  geom_point() +
  geom_text(aes(label=teamID), nudge_x = .05) +
  geom_smooth(method="lm", se=FALSE, color="black")


#We want to predict how one player does, so we imagine a team made up of players just like
#player A. 
#Previously we have derived metrics from teams based on team-level summary statistics.
#Some players might get better opportunties than others (per=plate-appearance rate).
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
       group_by(teamID) %>%
       summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
       .$pa_per_game %>%
       mean

#we will compute the per-plate-appearance rates for players available in 2002.
#To avoid small sample artifaces, we're going to filter players with few plate interference.
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/G,
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata=.))

#The distribution shows that there's y variablility across players.
players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, color = "black")

#We are pretending to be the Oakland A's in 2002 with only a $40 million budget.
#We need to know the players' salaries since we have a limited budget.
#We also need to know the player's position.
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

#filtering Outfielder and pitcher as they don't bat in the league.
players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>%
  top_n(1,G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

#if we make a plot, we cna see that players with high metrics have high salary
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#Rookies with good stats ahve not yet been able to negotiate a salary and are not
#going to be available in 2002.  Here's a plot with players that debuted before 1997.
players %>% filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#Let's take a look and examine the data for battering averages to see if 
#sophomore slump exists

library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#Let's create a table with only Rockie of the Year Award winners.
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS !="P")


#Now we'll keep only the rookie and sofphmore seasons and remove players that did not play
#a sophmore seasons.
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#Finally, we will use the spread function to have one column for the rookie and another
#column for the soophmore years' batting averages
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

#Now we can see the top performers in their first year.
ROY

#The sophmore slump appears to be real.  Let's look at all players at 2013 and 2014 seasons.
#And we're going to look at players that batted at least 130 times.
#This is the minimum needed to win the Rookie of the Year.
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS !="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(-playerID)

#let's look at the top performers of 2013 and then at their performance in 2014
two_years

#The batter averages go down for the top performers but these are not rookies.
#let's look at the worse performers of 2013
arrange(two_years, `2013`)

#Their batting averages go up in their second season in 2014.  The sophmore slump
#is not real.  This can be explained that the correlation for performance in two
#separate years is high but not perfect.

#Here is the data for 2013 performance and 2014 performance
two_years %>% ggplot(aes(`2013`, `2014`))+
  geom_point()

#The graph shows it is correlated but not perfectly corrected
#The coorelation is 0.46.
summarize(two_years, cor(`2013`,`2014`))