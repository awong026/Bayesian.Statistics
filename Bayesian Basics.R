
library(LearnBayes)
library(lattice)
#2.2: Let p denote the probability that it lands heads. To estimate this probaility, we will use a histogram to model our prior beliefs about p.
#Divide the interval [0,1] into ten subintervals [0,.1],..[.9,1], and specify probabilities that p is in each interval. Next spin the penny 20 times
# and count the number of success(heads) and failures(tails). Simulate from the posterior dist. by (1) computing the posterior density of p on a grid 
#of values on (0,1) and (2) taking a simulated sample with replacement from grid. (The functions histprior and sample are helpful in this computation)
#How have the interval probablities changed on the basis of your data?

#Histogram prior setup
midp=seq(0.05,0.95, by=0.1) #Histogram prior is represented by the midpoints in the intervals.
prior=c(0,0,1,2,7,7,2,1,0,0) #Prior weights
prior = prior/sum(prior) #prior probabilites
curve(histprior(x,midp, prior), from = 0, to=1, ylab = "Prior Density") #Graph of prior

#Figure out posterior density by multipling the histogram prior by the likelihood function. 
#likelihood function for a binomial density is given by beta(s+1, f+1).
s=15 #Choose the s and f. 
f=5
curve(histprior(x,midp,prior) * dbeta(x,s+1, f+1), from = 0, to = 1, ylab = "Posterior Densisty")

#To get the simulated sample from the posterior density. 
#Construct an equally spaced grid of values of the proportion p and compute the product of the prior and likelihood.
#Then convert products on the grid to probabilities. 
p=seq(0,1,length=500)
prior=histprior(p,midp,prior)
like=dbeta(p,s+1,f+1)
post=like*prior
post=post/sum(post)
Simulated=sample(p,replace=TRUE, prob=post)
hist(Simulated, xlab='p')#The interval probablities have gotten higher in the higher intervals. Not as symmetric as the prior histogram.
#2.3: A study reported on the long-term effects of exposure to low levels of lead in childhood. Researchers analyzed children's shed
# primary teeth forlead content. Of the children whose teeth had a lead content of more than 22.22 parts per million (ppm),  22 eventually graduated from high school
# and 7 did not. . Suppose your prior density for p, the proportion of all such children who will graduate from high school, is beta(1, 1),
# and so your posterior density is beta(23,8)

#a) Use the function qbeta to find a 90% interval estimate for p
#b) Use the function pbeta to find the probability that p exceeds .6.
#c) Use the function rbeta to take a simulated sample of size 1000 from the posterior distribution of p
#d) Suppose you find ten more children who have a lead content of more than 22.22 ppm. Find the predictive probability that nine or ten of
#them will graduate from high school. (Use your simulated sample from part (c) and the rbinom function to take a simulated sample from the predictive distribution.)

# Lead Content -- 22 graduated, 7 did not
 #Prior = Beta(1, 1)
 #so
 a = 1
 b =1
 s = 22
 f = 7
 #Posterior = Beta(22+1,7+1) = Beta(23,8)
 
#Show graph of prior, likelihood and posterior (For practice)
curve(dbeta(x,a+s,b+f), from = 0, to = 1, xlab = "p", ylab = "Density", lty = 1, lwd =4) #Posterior 
curve(dbeta(x, s+1, f+1), add = T, lty = 2, lwd =4) #likelihood
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4) #prior
legend(.7,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))

#a) Use the function qbeta to find a 90% interval estimate for p
xx <- qbeta(c(0.05, 0.95), 23, 8)
xx #.6060526 to .8598149


#b) Use the function pbeta to find the probability that p exceeds .6.
yy <- 1.0 - pbeta(0.6,23,8) #Used cdf to get this answer. 
yy #.9564759

#c) Use the function rbeta to take a simulated sample of size 1000 from the posterior distribution of p.
#code is rbeta(1000, a +s, b +f)
sampbeta <- rbeta(1000,a+s,b+f)
hist(sampbeta,col="blue",xlab="",ylab="",main="") #Use histogram to display findings
# Main title
mtext("Posterior Distribution for Graduation
      Probability",side=3,line=1.00,cex=1.2,font=2)
# x-axis title
mtext("Graduation Probability",side=1,line=2.75,font=2,cex=1.2)
# y-axis title
mtext("Frequency",side=2,line=2.75,font=2,cex=1.2)



##d) Suppose you find ten more children who have a lead content of more than 22.22 ppm. Find the predictive probability that nine or ten of
#them will graduate from high school. (Use your simulated sample from part (c) and the rbinom function to take a simulated sample from the predictive distribution.)
# Prediction 10 more children with high lead content
ynew <- rbinom(1000,10,sampbeta)
freq <- table(ynew)
freq
ys <- as.integer(names(freq))
ys
predprob <- freq/sum(freq)
predprob
plot(ys,predprob,type="h",xlab="",ylab="",main="")
# Main title
mtext("Predictive Distribution",side=3,line=1.00,cex=1.2,font=2)
# x-axis title
mtext("Predictive Probability",side=1,line=2.75,font=2,cex=1.2)
# y-axis title
mtext("YNEW",side=2,line=2.75,font=2,cex=1.2) 

#I got: .194 +.084 for prob of 9 0r 10
.194+.084 #.278. Might be different next time since sampling and I didn't set seed


#2.5 Suppose you are interested in estimating the average total snowfall per year ?? (in inches) for a large city on the East Coast of the United States.
#Assume individual yearly snow totals y1, ..., yn are collected from a population that is assumed to be normally distributed with mean ?? and known standard deviation ?? = 10 inches

#a) Before collecting data, suppose you believe that the mean snowfall ?? can be the values 20, 30, 40, 50, 60, and 70 inches with the following probabilities:
##Table:
#?? 20 30 40 50 60 70
#g(??) .1 .15 .25 .25 .15 .1
#Place the values of ?? in the vector mu and the associated prior probabilities in the vector prior

mu <- c(20,30,40,50,60,70)
mu
prior <- c(.1,.15,.25,.25,.15,.1)
prior

#b) Suppose you observe the yearly snowfall totals 38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, and 6.4 inches. Enter these data into a vector y and compute the sample mean ybar
y <- c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4)
ybar <- mean(y)
ybar #45.2833

#c) In this problem, the likelihood function is given by: (In pdf)
#where ybar is the sample mean. Compute the likelihood on the list of values in mu and place the likelihood values in the vector like.

sigma=10
n = length(y)
like=exp(-n*(mu-ybar)^2/(2*sigma^2))
like

#d) One can compute the posterior probabilities for ?? using the formula
#post=prior*like/sum(prior*like)
#Compute the posterior probabilities of ?? for this example
post=prior*like/sum(prior*like)
post

#e) Using the function discint, find an 80% probability interval for ??
print(cbind(mu,post))
print(discint(cbind(mu,post),0.8))
#Note: asnwer is 40 50 with p = 0.9999959. Any smaller interval yields p<0.8

#2.6: Suppose you own a trucking company with a large fleet of trucks. Breakdowns occur randomly in time and the number of breakdowns during an
# interval of t days is assumed to be Poisson distributed with mean t??. The parameter ?? is the daily breakdown rate. The possible values for ?? are
#.5, 1, 1.5, 2, 2.5, and 3 with respective probabilities .1, .2, .3, .2, .15, and .05. If one observes y breakdowns, then the posterior probability of ?? is proportional to (in pdf)
#where g is the prior probability.

#a) If 12 trucks break down in a six-day period, find the posterior probabilities for the different rate values.
#b) Find the probability that there are no breakdowns during the next week. Hint: If the rate is ??, the conditional probability of no breakdowns
#during a seven-day period is given by exp{???7??}. One can compute this predictive probability by multiplying a list of conditional
#probabilities by the posterior probabilities of ?? and finding the sum of the products.

#a) 
lam = c(.5, 1, 1.5, 2, 2.5,3)
prior = c(.1, .2, .3, .2, .15, .05)
y = 12
t = 6
mean = (t*lam)
like <- exp(-mean)*(mean^y)
post <- prior*like/sum(prior*like)
sum(post)
post



#b)Prob of no breakdowns in during next week
Nobreaks <- exp(-7*lam)
Nobreaks_prob <- Nobreaks * post
Nobreaks_prob
sum(Nobreaks_prob) #4.640932 e^-05










