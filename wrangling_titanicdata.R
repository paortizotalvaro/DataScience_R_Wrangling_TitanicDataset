################################################################
# This script contains a function that replaces missing values in
# the titanic dataset

# # To work properly this function require:
#   titanic_original.csv
### Arguments:
#   titanic_originaldf : data frame with purchases information
#                          (pclass, survived, name, sex, age, sibsp, parch, ticket, fare, cabin, embarked, boat, body, home.dest)
#
### Return: 
#   titanic_cleandf : titanic_originadf with replaced NAs
#
#
# Author: Paula Andrea Ortiz Otalvaro
# Created:  14-08-2019
# Last modified:   14-08-2019
#
################################################################

# --------------------------------------------------------------
#                        LOAD PACKAGES
# --------------------------------------------------------------
library(dplyr)
library(tidyr)

# --------------------------------------------------------------
#                    DEFINE GLOBAL VARIABLES
# --------------------------------------------------------------



# --------------------------------------------------------------
#                        FUNCTIONS
# --------------------------------------------------------------

# ********* Function to get csv file from excel file *************
xls_to_csv <- function(xlsfile, csvfile){
  
  exceldata <- xlsx::read.xlsx(xlsfile, sheetIndex = 1)
  data.table::fwrite(exceldata, file = csvfile)
  
}



# *********************** Wrangling function ***********************

clean_titanicdf <- function(titanic_originaldf){
    
    # ********** Set all blank and empty observations to NA *******************
    titanic_originaldf[titanic_originaldf==""] <- NA
    titanic_originaldf[titanic_originaldf==" "] <- NA
    
    # ********** Clean and modify observations *******************
    titanic_cleandf <- titanic_originaldf %>% 
                       # ********** 1. Replaced missing values with "S" in 'embarked' *******************
                       mutate(embarked =  ifelse(is.na(embarked), "S",embarked)) %>% 
      
                       # ********** 2. Replace missing ages with mean value in 'age' *****************  
                       mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age) ) %>% 
                       
                       # NOTE
                       # Mode could be another way of populating the missing values in the age column.
                       # Mode gives the most frequent age which would also be a good guess for the age of passengers.
    
                       # ********** 3. Replace empty lifeboat slots with 'None' in 'boat **********
                       mutate(boat = ifelse(is.na(boat), 'None', boat)) %>% 
    
                       # ********** 4. New column: has_cabin_number **********
                       mutate(has_cabin_number = ifelse(is.na(cabin), 1, 0))
    
    # *********************** Write cleaned dataframe to file ***********************
    data.table::fwrite(titanic_cleandf, file = "titanic_clean.csv" )
    
    
    return(titanic_cleandf)
}


# --------------------------------------------------------------
#             CALL FUNCTION ON TOY DATA SET
# --------------------------------------------------------------

# *********************** Get csv file (uncomment when needed) ***********************
#xls_to_csv(xlsfile = "titanic3.xls", csvfile = "titanic_original.csv")

# *********************** Load data ***********************
titanic <- read.csv("titanic_original.csv", sep=",", stringsAsFactors = FALSE)

# *********************** Wrangle data ***********************
titanic_cleaned <- clean_titanicdf(titanic_originaldf = titanic)






