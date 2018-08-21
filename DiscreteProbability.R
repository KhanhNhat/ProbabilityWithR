library(tidyverse)
library(gtools)


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

#Now, use permutation() and combination() functions from gtool package to simulation probability
#Calculate a probability of get a '10' or face card after picked an Ace
#We do not use mathematic formulation but we use R to simulate
#In this case, order is matter, so that we use permutation() to create all of possible of getting 2 cards
handOf2Cards = permutations(52, 2, deckCards)

#Result is a matrix of all possible of getting 2 cards from the deck
#Let take a look of the first of six
head(handOf2Cards)

#Get all possible of first cards
firstCards = handOf2Cards[, 1]

#And second cards
secondCards = handOf2Cards[, 2]

#Let A is an event of getting an Ace of the first card
#Probability of A is 1/13 = 0.0769
p_A = mean(firstCards %in% c("A Diamonds", "A Hearts", "A Clubs", "A Spades"))

#Let B is an event of getting a '10' or face cards of the second card
favorableCardsOfB_DF = expand.grid(number = c('10', 'J', 'Q', 'K'), suit = suits)
favorableCardsOfB = str_c(favorableCardsOfB_DF$number, favorableCardsOfB_DF$suit, sep = ' ')

#We need to calculate this probability of getting a '10' or face cards after picking an Ace
#It means we need to find P(B|A)
#And we have P(B|A) = P(B and A)/P(A)
#We already have P(A), we need P(B and A)
#Event of B and A happends when we have Ace in first card, '10' or face cards in second card at the same time
#It is calculated by:
p_BandA = mean((firstCards %in% c("A Diamonds", "A Hearts", "A Clubs", "A Spades")) & #first is an Ace
               (secondCards %in% favorableCardsOfB))                                  # and second is 10 or face cards   

#Therefore, P(B|A) is:
p_BgivenA = p_BandA/p_A

#Let check with mathematic
#After picking one card, we have 51 cards left
#We have 16 cards of '10' and face cards
#So, the probability of picking one of them at the second cards is: 16/51
#Use identical() to check p_BgivenA is exactly 16/51
identical(p_BgivenA, 16/51)

#What happend if order is not matter?
#Calculate a probability of getting natural 21 in Black Jack?
#In Black Jack, we get natural 21 when we have an Ace and one of 10 or face cards
#Which card comes first is not matter
#In this case, we use combination to create all of collection of 2 cards from the deck
blackJack = combinations(52, 2, deckCards)
head(blackJack)

#Now, we need to count how many case of natural 21 appear
#Create a dataframe
blackJackDF = data.frame(blackJack)
blackJackDF$X1 = as.character(blackJackDF$X1)
blackJackDF$X2 = as.character(blackJackDF$X2)

#Find how many of natural 21 appeards in this dataframe:
numOfNatural21 = blackJackDF %>% unite('Collection', c('X1', 'X2')) %>%
                                 filter(str_detect(Collection, '[1JQK]') &       #Have '10' or face cards
                                        str_count(Collection, 'A') == 1) %>%     #Must have only one Ace
                                 nrow()
#Therefore, probability is
numOfNatural21/nrow(blackJackDF)

#Compare with mathematic
#We have 4 Ace and 16 of '10' or face cards, so that we have 16 * 4 case of natural 21
#Then, probability is:
16 * 4 / choose(52, 2)

#Another simple way:
#We take care of 2 cases:
# - Ace in first card
# - Ace in second card
#Then we add them together by logic OR
mean((blackJack[, 1] %in% c("A Diamonds", "A Hearts", "A Clubs", "A Spades") & 
      blackJack[, 2] %in% favorableCardsOfB) |
     (blackJack[, 1] %in% favorableCardsOfB &
      blackJack[, 2] %in% c("A Diamonds", "A Hearts", "A Clubs", "A Spades")))


#Run Monte Carlo simulation for birthday problem
#Calculate probability of having at least 1 duplicated birthday in a group of n people
#We use duplicated() to check if an element of a vector is duplicated or not
#Result of this function is a logical vector which has same length with input vector
#It is FALSE if an element is distinct
#It is TRUE at the second, third, ... position of duplicated value
#For example:
#duplicated(c(1, 2, 3, 1, 1, 4, 2, 5))
#[1] FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE
#And any() function that return TRUE if at least one TRUE in input vector

#Let n = 30
n = 30
birthdaySimulation = replicate(N, any(duplicated(sample(1:365, n, replace = TRUE))))

#Therefore, approximate probability is:
mean(birthdaySimulation)

#Now compare with mathematical way
#With n people, we have 365^n possible birthday
#We calculate a chance of no duplicated birthday is:
#First one has:   365
#Second one has:  364
#Third one has:   363
#.................
#Upto nth person: (365 - n + 1)
#Probability is: prod(365 - n + 1:365)/(365^n)
#Complement of above probability is what we need
1 - prod((365-n+1):365)/(365^n)

#Now let simulate a medical case:
#There is a medical test machine with accuracy is 99%.
#It means that if patient is positive, it result test is positive in 99% of all case.
#There is only 1% of a whole country population is possitive with this disease.
#If a person has a positive result, what is a probability that he is really sick?

#Create a model for a medical test machine:
medicalTest = function(patient){
  if (patient == 'positive'){
    return(sample(c('positive', 'negative'), size = 1, prob = c(0.99, 0.01)))
    } else if (patient == 'negative'){
    return(sample(c('negative', 'positive'), size = 1, prob = c(0.99, 0.01)))
    } else
      return('NA')
}

#Now we apply Rule of Large Number to get a aproximately probability
#We do this for 100 times
probs = replicate(100, {
                        patientPop = sample(c('positive', 'negative'), size = 10000, replace = TRUE, prob = c(0.01, 0.99))
                        medicalResult = sapply(patientPop, medicalTest)
                        sum(medicalResult == 'positive' & patientPop == 'positive')/sum(medicalResult == 'positive')
                        })

#With Central Limit Theorem, we will have:
#True value of probability is close to this
meanProb = mean(probs)

#Standard deviation is equal this sd/sqrt(N)
sdProb = sd(probs)/10

#True probability is meanProb +/- 2*sdProb with confidence level is 95%
#From 0.4938 - 0.5089
