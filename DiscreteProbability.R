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

#Create a desk of card
suits = c('Diamonds', 'Clubs', 'Hearts', 'Spades')
numbers = c('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')
deckCardsDF = expand.grid(number = numbers, suit = suits)
deckCards = str_c(deckCardsDF$number, deckCardsDF$suit, sep = ' ')

#Now calculate a probability of picking an Ace from this deck
#Get each card from the deck, check if it is in a group of Aces
#The result is a vector of TRUE and FALSE value
#Mean of this vector = num of TRUE/num of TRUE and FALSE
#Therefore, it is also probability of picking an Ace
mean(deckCards %in% c("A Diamonds", "A Hearts", "A Clubs", "A Spades"))




