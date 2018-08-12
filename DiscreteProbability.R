library(tidyverse)


#Discrete Probability with R
#Monte Carlo simulation with simple example: pick a red bead in a bag of red and blue beads
#Set num of experiments is N = 10000
N = 10000

#Create a bag of red and blue beads
beads = rep(c('red', 'blue'), times = c(2, 3))

#Pick 1 bead from this bag in N times without replacement
pickingBead = replicate(N, sample(beads, 1))

#Let see how many red and blue is picked
beadTable = table(pickingBead)

#And approximately probability
prop.table(beadTable)
