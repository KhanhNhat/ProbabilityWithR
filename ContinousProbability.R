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
