############  HEADER   ############
#
# Data Wrangling: Exercise 1
# Springboard Foundations of Data Science
# Author: Antoine Beauchamp
# Edited: January 13th, 2017
# Created: January 10th, 2017
#
###################################

#Clear working environment
rm(list=ls())

#Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)

#Set correct working directory
path_to_wd <- file.path("~","Documents","Work","DataScience","Springboard","FoundationsofDataScience","Section3_DataWrangling","DataWrangling_Exercise1")
setwd(path_to_wd)
rm(path_to_wd)


############# Section 0: Import Data ########################


#Read in dataset, convert to tibble format
colnames_vec=c("company","product_code_number","address","city","country","name")
refine_data <- tbl_df(read_csv("refine_original.csv", col_names=c("company","product_code_number","address","city","country","name")))
#Drop unnecessary name row
refine_data <- refine_data[-1,]

#Some making info, glancing at the data
class(refine_data)
glimpse(refine_data)
head(refine_data)
View(refine_data) #Opens up spreadsheet like table
refine_data #Can just print the data with our tbl_df, instead of using head(). 



########### Section 1: Clean Up Brand Names #####################


#Correct the data for each of the companies. (Is there a better way of doing this? I can't imagine that this would be helpful for a large dataset. )
d1 <- refine_data %>% mutate(company= gsub("^([Pp][Hh]|[Ff]).+[Pp][Ss]$", "Phillips", refine_data$company)) 
d2 <- refine_data %>% mutate(company=gsub("^[Aa][Kk].*[Zz]([Oo]|0)$", "Akzo", d1$company))
d3 <- refine_data %>% mutate(company=gsub("^[Vv].+[Hh].+en$", "Van Houten", d2$company))
refine_data1 <- refine_data %>% mutate(company=gsub("^[Uu].*er", "Unilever", d3$company))

#Check to be sure
refine_data1$company
  

########### Section 2: Separate Product and Code Number #####################

# Separate the product and code numbers in the product_code_number column
refine_data2 <- refine_data1 %>% separate(product_code_number,into=c("product_code","product_number"),sep="-")

View(refine_data2)

########### Section 3: Add Product Category #####################

# Define a function that takes a vec of product numbers and returns a vector of product categories
prodcategory_func <- function(invec){
  newvec = rep(0, length(invec))
  for (i in 1:length(invec)){
    if (invec[i]=='p'){
      newvec[i]='Smartphone'
    } else if (invec[i]=='v') {
      newvec[i]='TV'
    } else if (invec[i]=="x") {
      newvec[i]="Laptop"
    } else if (invec[i]=="q") {
      newvec[i]="Tablet"
    }
  }
    return(newvec)
}

#Use mutate() with the newly defined function to generate the product_category variable.
refine_data3 <- refine_data2 %>% mutate(product_category=prodcategory_func(refine_data2$product_code))

#Check to be sure
refine_data3



########### Section 4: Add Full Address #####################
     
#Add the full address by uniting the variables address, city, and country
refine_data4 <- refine_data3 %>% unite(full_address, address, city, country, sep=",")
                                        
refine_data4


########### Section 5: Create dummy variables for company and product #####################

#Create a dummy variable for companies by defining an anonymous function as appropriate. 
refine_data5 <- refine_data4 %>% 
  mutate(company_phillips=sapply(refine_data4$company, function (x) {if (x=="Phillips") {1} else {0}}))  %>% 
  mutate(company_akzo=sapply(refine_data4$company, function (x) {if (x=="Akzo") {1} else {0}})) %>% 
  mutate(company_van_houten=sapply(refine_data4$company, function (x) {if (x=="Van Houten") {1} else {0}})) %>% 
  mutate(company_unilever=sapply(refine_data4$company, function (x) {if (x=="Unilever") {1} else {0}}))

#Create a dummy variable for products by defining an anonymous function as appropriate. 
refine_clean <- refine_data5 %>% 
  mutate(product_smartphone=sapply(refine_data4$product_category, function (x) {if (x=="Smartphone") {1} else {0}})) %>%
  mutate(product_tv=sapply(refine_data4$product_category, function (x) {if (x=="TV") {1} else {0}})) %>%  
  mutate(product_laptop=sapply(refine_data4$product_category, function (x) {if (x=="Laptop") {1} else {0}})) %>%
  mutate(product_tablet=sapply(refine_data4$product_category, function (x) {if (x=="Tablet") {1} else {0}}))
  
  View(refine_data6)
  
  
  
  
  ########### Section 6: Write to File #####################
  
  #Write the clean data to a .csv file
  write_csv(refine_clean, "refine_clean.csv")
  
  

