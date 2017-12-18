library(devtools)
library(statsr)
library(dplyr)
library(ggplot2)
library(shiny)
library(purrr)

## Posterior Probabilities
#Question 1: Based on the preceding result, what is the probability that Machine 1 is "Bad" given you won playing on Machine 1?
#Answer: .4

#Question 2: Based on the preceding result, what is the probability that Machine 2 is "Good" given you won playing on Machine 1?
#Answer: .4

#Question 3: Under the Bayesian paradigm, which of the following correctly matches the probabilities with their names?
#Answer:  Answer is choice 4

## Bayesian Updating
#Question 5: Using the bandit_posterior function calculate the posterior probabilities of Machine 1 and 2 being "good" after playing Machine 1 
#twice and winning both times and then playing Machine 2 three times and winning twice and then losing.
#Answer: 
bandit_posterior(data = data.frame(machine=c(1L,1L, 2L, 2L, 2L), outcome=c("W","W", "W", "W", "L")))
#.574 for leveler 1 and .428 for lever 2


#Question 6: What would the posterior probabilities be if we had instead played Machine 2 first, playing three times, winning twice and losing 
#once and then playing Machine 1 twice and winning both times?
#Answer: 
bandit_posterior(data = data.frame(machine=c(2L, 2L, 2L, 1L, 1L), outcome=c("W","W", "L", "W", "W")))
#.574 for leveler 1 and .428 for lever 2

#Back to the Bandits
#####Last section of R Markdown
data = data.frame(machine = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                              1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                              2L, 2L, 2L, 2L), 
                  outcome = c("W", "W", "L", "W", "L", "L", "W", "L", "L", "L", "W", "W", "W", "L", "W", "L", "W", "W", "L",
                              "L", "L", "W", "L", "W", "W", "L", "L", "L", "L", "W", "W", "L", "L", "W", "W", "L", "L", "L",
                              "L", "L", "L", "L", "L", "L", "L", "W", "W", "W", "L", "L"))
plot_bandit_posterior(data)


##Question: Why do the posterior probabilities for Machine 1 and Machine 2 mirror each other?
#1) P(M1 | data)P(M1 | data)  and P(M2 | data)P(M2 | data) are complementary
#2) Machine 1 and Machine 2 being "good" are mutually exclusive events
#3) All of the above

#Answer: All of the above