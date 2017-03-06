#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
setwd("/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/Section7_DataAnalysis/Section7_Ex1/linear_regression/")


##   You might also start by listing the files in your working directory

#What is the working dir? 
getwd() 
#What files are in the subdirectory dataSets?
list.files("dataSets") 

## Load the "states" data
## ────────────────────────

#Read the states.rds data
states.data <- readRDS("dataSets/states.rds") 

#Let's take a look at the data first. 
head(states.data)

#The data is organized according to states, so this tells me that
# this contains a variety of information about different states, such as
# population, area, density, energy, etc. 
# We can learn more about what these variables are by getting their attributes. 
attributes(states.data)
#There are a number of attributes. The ones we want are "names" and "var.labels"
# "names" is the variable names while "var.labels" is a brief description
attributes(states.data)$var.labels

#Get the variable names and their descriptions
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)
states.info

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

#For our linear regression, we will be examining the SAT scores for the different states.
#The mean composite SAT score is found in the variable csat
# We will begin by examining the relationship between expenditure and csat scores
# for different states

# summary of expense and csat columns, all rows
# csat is the mean composite SAT score
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)

# Is there a correlation between expense and csat
cor(sts.ex.sat)
#Seem to be a slight negative correlation

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

#There seems to be a mild negative trend, as indicated by the correlation. Let's fit this. 

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model. We are predicting csat based on expenditure. 
sat.mod <- lm(csat ~ expense, data=states.data) 
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

#We can see that expense is a significant predictor of csat, but it is negative. 
# This means that less expense predicts higher csat. Also model isn't great based on R square value.

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

# percent is the % of high school graduates taking the SAT within that state
summary(lm(csat ~ expense + percent, data = states.data))

#This is a better model. 

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

#class() gives the class of the object
class(sat.mod)
#names() gives the names of different variables in the model object
names(sat.mod)
#Methods shows available methods to use on the given class of object. 
# This can give us more information about what we can do with our model. 
methods(class = class(sat.mod))[1:9]

##   • Use function methods() to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

#We want to see if the state's house and senate predict the csat scores. 
states.info
#House and senate are some measure of the house and senate in that state. 

# fit another model, adding house and senate as predictors
#This is our new model that takes politics into account
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
#This is our old model
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
#Extract coefficients
coef(summary(sat.voting.mod))

#This is not a good model. 

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

str(states.data)
states.info

#We want to examine how the energy consumed per capita, stored in "energy" is
# related to the percentages of people living in metropolitan areas, stored in "metro". 

plot(energy ~ metro, data=states.data)

#Looks like we have a few outliers. Data doesn't have much of a linear trend to it, even without outliers

EnergyModel <- lm(energy~metro,data=na.omit(states.data))
summary(EnergyModel)

# metro appears to be a slightly significant predictor of energy. 
# However this model is not very good since our R-squared value is very low. 

plot(EnergyModel)

#Let's add more variable to our model to see what might influence the energy use
states.info
#Let's add a bunch of variables and see what comes up. 
EnergyModel2 <- lm(energy~metro+pop+density+area+waste+miles+green, data=na.omit(states.data))
summary(EnergyModel2)

#Only "green" appears to be significant. 
#Let's start removing variables with the largest p-values

EnergyModel3 <- lm(energy~metro+density+area+waste+miles+green, data=na.omit(states.data))
summary(EnergyModel3)

EnergyModel4 <- lm(energy~metro+area+waste+miles+green, data=na.omit(states.data))
summary(EnergyModel4)

EnergyModel5 <- lm(energy~metro+area+miles+green, data=na.omit(states.data))
summary(EnergyModel5)

EnergyModel6 <- lm(energy~area+miles+green, data=na.omit(states.data))
summary(EnergyModel6)

EnergyModel7 <- lm(energy~area+green, data=na.omit(states.data))
summary(EnergyModel7)

#It seems only the green variable is significant. The area variable is slightly important but not really. 
# R-squared value is around 0.6, which is okay. 


## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
summary(sat.region)
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.


#The regression we created was done by modelling energy via green and area. 
states.info
plot(energy~green,data=states.data)
summary(lm(energy~metro,data=states.data))

#Let's add some variables to green to see what happens
summary(lm(energy~green*metro,states.data))
#This is interesting, the interaction between greenhouse gas emissions and metro area population 
# seems to be a strong predictor of energy consumption
states.info
#Let's try other things
summary(lm(energy~green*density,states.data))
summary(lm(energy~green*pop,states.data))

#It seems the stronger models are the ones that consider greenhouse gasses and populations

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


states.data$region <- factor(states.data$region)

summary(lm(energy~region,data=na.omit(states.data)))

plot(energy~region,states.data)

summary(lm(energy ~ C(region, base=2),
                data=states.data))

#I don't really understand how to interpret these coefficients. This is a poor exercise.
