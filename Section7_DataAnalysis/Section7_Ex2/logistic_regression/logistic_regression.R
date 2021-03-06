#################################################
#
# Section 7, Exercise 2: Logistic Regression
# 
# Author: Antoine Beauchamp
# Edited: March 5th, 2017
# Created: March 5th, 2017
#
################################################


## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.


# set the working directory
setwd("/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/Section7_DataAnalysis/Section7_Ex2/logistic_regression/")
rm(list=ls())

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
labs

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
str(NH11$hypev)
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

#1. 

#Remove unclear answers from everwrk data
str(NH11$everwrk)
levels(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, levels=c("1 Yes","2 No"))
levels(NH11$everwrk)

#Check age data. Seems fine
str(NH11$age_p)

#Check categories for marital status.
str(NH11$r_maritl)
factvec <- levels(NH11$r_maritl)
factvec
#We can remove Under 14 and unknown marital status
factvec <- factvec[-10]
factvec <- factvec[-1]
factvec <- factvec[-3]
factvec
#This should be fine. Let's impose these categories. 
NH11$r_maritl <- factor(NH11$r_maritl, levels=factvec)
str(NH11$r_maritl)
levels(NH11$r_maritl)

#Generate our logistic model to predict everwrk based on age and marital status
workmod <- glm(everwrk ~ age_p + r_maritl, data=NH11, family = "binomial")

#2. 

#Now that we have our logistic model, let's predict the prob. of working for each marital status. 
#Narrow down the data to the specific marital status. For the age value, we will use the mean age for 
#that category. 

predData <- numeric(length(factvec))
for(i in 1:length(factvec)){
  dat <- with(NH11,expand.grid(r_maritl = factvec[i], age_p = mean(subset(NH11$age_p, NH11$r_maritl == factvec[i]), na.rm=TRUE)))
  predData[i] <- predict(workmod,type="response",newdata=dat)
}

cbind(factvec,predData)

#These probabilities seem very low. It seems to say that if you are divorced, your probability of ever
# having worked is 6%. 

#Let's try to get a feel for this. 
#Isolate all data related to marital status of divorced. 
divorced <- subset(NH11$everwrk, NH11$r_maritl == "5 Divorced")
length(divorced)

#Remove NAs
divorced <- divorced[!is.na(divorced)]
length(divorced)

#Our total number of outcomes is 1907

length(divorced[divorced == "1 Yes"]) #1806
length(divorced[divorced == "2 No"]) #101

#So from this naive estimate the number of percentage of those who are divorced and have worked is
1806/1907
#And those who haven't
101/1907 #5%

#Perhaps the logistic model is considering not having worked as the success? 
#Consider the values in 
cbind(factvec,predData)

#Let's try this again with a different class. 
married <- subset(NH11$everwrk, NH11$r_maritl==factvec[1])
length(married)

married <- married[!is.na(married)]
length(married) #5458

length(married[married=="1 Yes"])/length(married) # 89%
length(married[married=="2 No"])/length(married) # 11%

cbind(factvec,predData)[1,]


#Yeah I think that's what's happened. The logistic regression is treat "2 No" as a success.

#The one other different value I could check is "Never married"
nevermarried <- subset(NH11$everwrk, NH11$r_maritl==factvec[6])
length(nevermarried)

nevermarried <- nevermarried[!is.na(nevermarried)]
length(nevermarried)

length(nevermarried[nevermarried=="1 Yes"])/length(nevermarried)
length(nevermarried[nevermarried=="2 No"])/length(nevermarried)

#We find a probability of No to be around 25%, compared to 24% based on our model
cbind(factvec,predData)[6,]



