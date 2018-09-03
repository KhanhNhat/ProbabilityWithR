library(tidyverse)
library(dslabs)

#Define an empirical Cumulative Distribution Function (eCDF)
data(heights)
maleHeights = heights %>% filter(sex == 'Male') %>% '$'(height)

#In this case eCDF is just a proportion of the above list which its value is less than a specific one.
maleHeightCDF = function(h) mean(maleHeights <= h)


#For example, probability of picking a male that his height is less than 65 inches is:
maleHeightCDF(65)

#Let take a look how male heighs are distributed:
qplot(x = maleHeights, col = I('white'), xlab = 'Male Heigh Distribution')

#And how is eCDF look like:
maleHeightProb = sapply(seq(floor(min(maleHeights)), ceiling(max(maleHeights)), by = 1), maleHeightCDF)

qplot(x = seq(floor(min(maleHeights)), ceiling(max(maleHeights)), by = 1), y = maleHeightProb, col = I('tan1'),
      xlab = 'Male Height', ylab = 'Cumulative Probability')

ggplot(heights, aes(x = height)) + 
  geom_histogram(aes(y = ..density..), col = 'white', binwidth = 2) + 
  geom_density(col = 'blue') +
  facet_grid(rows = vars(sex))

#Simulation a Random Walk
#You stay at the midle of the way
#4 metres on your left hand side is Western Restaurant
#5 metres on your right hand side is Asian Restaurant
#You do not know to choose and roll a dice
#If a dice come up with 1, 2, 3, 4 you will go to the left with 1 metre
#If a dice come up with 5, 6 you will go to the right with 1 metre
#You stop rolling dice if you reach one of these restaurant
#Calculate a probability to go to the Western Restaurant

#Create a function that simulate a random walk
#If we go left we will go to a negative side with 1 unit
#If we go right we will go to a positive side
#Function return -4 means we will go to the  Western Restaurant
#And 5 for Asian one.
randomWalk = function(){
  target = 0
  while (!target %in% c(-4, 5)){
    target = target + sample(c(-1, 1), size = 1, prob = c(2/3, 1/3))
  }
  return(target)
}

#We do it 10000 times
randomwlkSim = replicate(10000, randomWalk())

#Let see a result
prop.table(table(randomwlkSim))

#So that, probability to go to Western Restaurant is 0.9708

#Now, compare with mathematic formulation
#This is a case of Random Walk with absorbing barriers
#Formulation is: p(a) = (s^a - 1)/(s^b - 1)
#In our case, a = 5, b = 9, s = (1/3)/(2/3) = 1/2
(0.5^5 - 1)/(0.5^9 - 1)

#It is 0.9706458, very closed with our simulation result.