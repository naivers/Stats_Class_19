####HOMEWORK 2

##Question 1


#This is to create the dataset for Question 1
P <- rpois(15, 15)
P

#1a.
#simply using ppois function to generate a cumulative Poisson distribution with Lambda set at PREDICTED mean of 15 calls.
#ask for probability of hearing more than 8 calls in a given day
ppois(8, lambda = 15, lower=FALSE)
#returns 0.9625535
#therefore 96.25% probability of hearing more than 8 calls

#1b.
#dpois function to calculate poisson distribution probabilities
#ask for probability of hearing 0 calls 
dpois(0, lambda=15)
#returns: 3.059023e-07
# 0.0000003059% probability of hearing 0 calls in a day

#1c.
#same function and rationale as 1b
#ask for probability of hearing 3 calls
dpois(3, lambda=15)
#returns 0.0001720701
#0.017% probability of hearing 3 calls in a day

#1d
x <- 0:30  #No idea what the length of the vector should be. PLotted 0:100 initially, saw the mass function tapered around 30 calls, so I reset the vector for 30 call max.
l <- 15    #lambda is the mean?
probset <- dpois(x, lambda = l)
barplot(probset, names.arg = x, space = 0, xlab = "x", ylab = "Pr(X = x)", main = paste0("Probability Mass Function\nlambda = ", l))

#1.e. Simulate 104 results from this distribution 
#(i.e., 2 years of Saturday monitoring sessions).

rand_obs <- (rpois(104, lambda = 15))
#1f plot the results of 1e

hist((rpois(104, lambda = 15)), xlim = c(0, 30))



##Question 2

library(curl)
f <- f <- curl("https://raw.githubusercontent.com/difiore/ADA-2019/master/zombies.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

#2.a. Calculate the population mean and standard deviation for each quantitative random variable 
#(height, weight, age, number of zombies killed, and years of education).

mean_height <- mean(d$height)
mean_height

mean_weight <- mean(d$weight)
mean_weight

mean_age <- mean(d$age)
mean_age

mean_killed <- mean(d$zombies_killed)
mean_killed

mean_education <- mean(d$years_of_education)
mean_education

sd_height <- (sum((d$height-mean_height))
sd_height

sd_height <- sqrt(sum(((d$height - mean_height)^2)/1000))
sd_height

sd_weight <- sqrt(sum(((d$weight - mean_weight)^2)/1000))
sd_weight

sd_age <- sqrt(sum(((d$age - mean_age)^2)/1000))
sd_age

sd_killed <- sqrt(sum(((d$zombies_killed - mean_killed)^2)/1000))
sd_killed

sd_education <- sqrt(sum(((d$years_of_education - mean_education)^2)/1000))
sd_education


#########2.b. Use {ggplot} and make boxplots of each of these variable by gender.


males <- subset(d, gender=="Male")
females <- subset(d, gender=="Female")

par(mfrow = c(1, 2))
boxplot(males$weight, ylab = "Male Weight")
boxplot(females$weight, ylab = "Female Weight")

par(mfrow = c(1, 2))
boxplot(males$age, ylab = "Male Age")
boxplot(females$age, ylab = "Female Age")

par(mfrow = c(1, 2))
boxplot(males$zombies_killed, ylab = "Male Kills")
boxplot(females$zombies_killed, ylab = "Female Kills")

par(mfrow = c(1, 2))
boxplot(males$years_of_education, ylab = "Male Education")
boxplot(females$years_of_education, ylab = "Female Education")



#2.c. Use {ggplot} and make scatterplots of height and weight in relation to age. 
#Do these variables seem to be related? In what way?

par(mfrow = c(1, 2))
plot(x = d$height, y = d$age)
plot(x = d$weight, y = d$age)
#There appears to be a positive linear trend to age and height, but the scatter plot does not reveal any obvious trends in age/weight comparisons.

#2.d. Using histograms and Q-Q plots, 
#check whether the quantitative variables seem to be drawn from a normal distribution. 
#Which seem to be and which do not?

qqnorm(d$height, main = "QQ-plot Height")
qqnorm(d$weight, main = "QQ-plot Weight")
qqnorm(d$age, main = "QQ-plot Age")
qqnorm(d$zombies_killed, main = "QQ-plot Zombies Killed")
qqnorm(d$years_of_education, main = "QQ-plot Education")
##QQ plots indicate Height, Weight, Age are normally distributed
## Years of Eductation and Zombies Killed do not appear to be continuously distributed, so QQ plots are not ideal test.

hist(d$height, labels = TRUE)
hist(d$weight, labels = TRUE)
hist(d$age, labels = TRUE)
hist(d$zombies_killed, labels = TRUE)
hist(d$years_of_education, labels = TRUE)

#histograms indicate Height, Weight and Age are normally distributed, but number of zombies killed and education are not normally distributed.

#2e
thirty_d <- d[sample(nrow(d), 30), ]

mean30_height <- mean(thirty_d$height)
mean30_height
mean30_weight <- mean(thirty_d$weight)
mean30_age <- mean(thirty_d$age)
mean30_killed <- mean(thirty_d$zombies_killed)
mean30_education <- mean(thirty_d$years_of_education)

sd30_height <- sd(thirty_d$height)
sd30_height
#4.32197
sd30_weight <- sd(thirty_d$weight)
sd30_weight
#18.700
sd30_age <- sd(thirty_d$age)
sd30_age
#2.7236
sd30_killed <- sd(thirty_d$zombies_killed)
sd30_killed
#1.299
sd30_education <- sd(thirty_d$years_of_education)
sd30_education
#1.544

##disregard: these are standard error
library(sciplot)
se_30_height <- sciplot::se(thirty_d$height)
se_30_height
#0.789081
se_30_weight <- sciplot::se(thirty_d$weight)
se_30_weight
#3.414
se_30_age <- sciplot::se(thirty_d$age)
se_30_age
#0.4972
se_30_killed <- sciplot::se(thirty_d$zombies_killed)
se_30_killed
#0.237
se_30_education <- sciplot::se(thirty_d$years_of_education)
se_30_education
#0.2820



#2f

sim99 <- NULL  # sets up a dummy variable to hold our 10000 simulations
for (i in 1:99) {
  sim99[i] <- sample(nrow(d), 30, )
}
# need to merge 99 + 1 samples

sim100 <- merge(sim99,thirty_d, by="id")

##
k <- 99  # number of samples
n <- 30  # size of each sample
s99_height <- NULL  # dummy variable to hold each sample
for (i in 1:k) {
  s99_height[[i]] <- mean(d[sample(d$height, 30), ])
}
head(s99_height)
#s99 is actually a list of 99 data frames

sim100 <- merge(sim99,thirty_d, by="id")
#warning must specify a valid column

