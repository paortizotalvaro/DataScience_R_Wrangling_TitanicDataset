################################################################
# This script contains 3 function that make 3 different plots out
# of the titanic data set
# # To work properly these functions require:
#   titanic_clean.csv
#   ggplot2 to be installed

### Arguments:
#   titanic_originaldf : data frame with purchases information
#                          (pclass, survived, name, sex, age, sibsp, parch, ticket, fare, cabin, embarked, boat, body, home.dest)

### Return: 
#   Nothing. The plots are saved as pdf files.

### Usage:
#   To run all script do > 

# Author: Paula Andrea Ortiz Otalvaro
# Created:  30-08-2019
# Last modified:   30-08-2019
#
################################################################

# --------------------------------------------------------------
#                        LOAD PACKAGES
# --------------------------------------------------------------
library(ggplot2)

# --------------------------------------------------------------
#                        FUNCTIONS
# --------------------------------------------------------------

# ********* Function to get csv file from excel file *************
xls_to_csv <- function(xlsfile, csvfile){
  
  exceldata <- xlsx::read.xlsx(xlsfile, sheetIndex = 1)
  data.table::fwrite(exceldata, file = csvfile)
  
}


# ****** function: plot distribution of one of the features *******

plot_distribution_titanic <- function(titanic_originaldf, feature_x, feature_fill){
    
    # Print the plot to a pdf file
    pdf( paste0(feature_x,"_distribution_withfill_",feature_fill,".pdf")  )
  
    # make plot  
    plot_featuredistribution <- ggplot(titanic, aes_string(x = feature_x , fill = feature_fill)) +
                                geom_bar(position = "dodge")
    
    print(plot_featuredistribution)
    dev.off()
}


# ****** function: plot distribution of one of the features for both survival cases *******

plot_distribandsurvivalgrid_titanic <- function(titanic_originaldf, feature_x, feature_fill){
  
  # Print the plot to a pdf file
  pdf( paste0(feature_x,"_distribution_withfill_",feature_fill,"_withsurvivalgrid.pdf")  )
  
  # make plot  
  plot_featuredist_wgrid <- ggplot(titanic, aes_string(x = feature_x, fill = feature_fill)) +
                            geom_bar(position = "dodge") +
                            facet_grid(.~survived)
  
  print(plot_featuredist_wgrid)
  dev.off()
}

#plot_xvsydist_withgrid
# ****** function: plot distribution of one of the features against a second one and for both survival cases *******

plot_distributionxvsy_survivalgrid_titanic <- function(titanic_originaldf, feature_x, feature_y, feature_fill){
  
  # Define an object for position jitterdodge, to use below 
  posn.jd <- position_jitterdodge(0.5, 0, 0.6)
  
  # Print the plot to a pdf file
  pdf( paste0(feature_x,"_vs_","_withfill_",feature_fill,"_withsurvivalgrid.pdf")  )
  
  # make plot  
  plot_xvsydist_withgrid <- ggplot(titanic, aes_string(x = feature_x, y=feature_y, color = feature_fill)) +
                            geom_point(position = posn.jd, size = 3, alpha = 0.5) +
                            facet_grid(.~survived) 
  
  print(plot_xvsydist_withgrid)
  dev.off()
}

# --------------------------------------------------------------
#             CALL FUNCTION 
# --------------------------------------------------------------

# *********************** Get csv file (uncomment when needed) ***********************
#xls_to_csv(xlsfile = "titanic3.xls", csvfile = "titanic_original.csv")

# *********************** Load data ***********************
titanic <- read.csv("titanic_clean.csv", sep=",", stringsAsFactors = FALSE)

# *********************** Plot ***********************
# 1. Plot the distribution of sexes within the classes of the ship, dodged bar plot
plot_distribution_titanic(titanic_originaldf = titanic, feature_x = "pclass", feature_fill = "sex")

# 2. Plot the distribution of sexes within the classes of the ship, dodged bar plot with grid comparing survival 1 and 0.
plot_distribandsurvivalgrid_titanic(titanic_originaldf = titanic, feature_x = "pclass", feature_fill = "sex")

# 2. Plot the distribution of sexes within the classes of the ship, dodged bar plot with grid comparing survival 1 and 0.
plot_distributionxvsy_survivalgrid_titanic(titanic_originaldf = titanic, feature_x = "pclass", feature_y = "age", feature_fill = "sex")








