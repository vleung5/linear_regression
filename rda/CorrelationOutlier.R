
#Correlation is Not Causation: Outliers

#Suppose we take measurements from two independent outcomes, x and y, and we standardize the meansurements.
#However, imagine we made a mistake and forgot to standardize entry 23.  We can simulate such data using
#this code:
set.seed(1)

x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

#The data looks like this
tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

#Not surprisingly, the correlation is very high
cor(x,y)

#The one loutlier is making the correlation to be as high as 0.99.  If we remove this outlier, the
#correlation is greatly reduced to almost 0, which is wha tit should be
cor(x[-23],y[-23])


#We can use the Spearman correlation that is robust to outliers. If we compute the correlation of the ranks, 
#we get something much closer to 0.
cor(rank(x), rank(y))

#We can also use the correlation function using the method argument to tell which correlation to compute.
cor(x, y, method = "spearman")