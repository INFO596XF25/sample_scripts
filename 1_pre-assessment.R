# ==============================================================================
# Pre-assessment script
# ==============================================================================

## 1. Basic R

#What will this code output? Write the result:

species <- c("oak", "pine", "maple", "birch")
length(species)
species[2:3]

## 2. Data structures

#Write a single line of code to create a data frame with two columns:

#- species: oak, pine, maple
#- abundance: 45, 23, 67

## 3. Working with data

#What function or package would you use to:

#a) Read a CSV file with biodiversity data: \_\_
#b) Create a plot of species abundance: \_\_
#c) Filter rows where abundance > 50: \__________\_

## 4. Functions

#Complete this function to calculate the mean abundance of species:

calculate_mean_abundance <- function(abundance_vector) {

}

## 5. Error Recognition

#Describe any errors in this code. Feel free to suggest changes if you're able to fix it:

library(ggplot2)
data <- read.csv(biodiversity_data.csv)
mean_value = mean(data$abundance, na.rm = TRUE)
print("The mean abundance is: " + mean_value)