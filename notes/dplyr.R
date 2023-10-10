library(MASS)
data(Cars93)

#install.packages('dplyr')
library(dplyr)

# Base-R vs. dplyr
# select Horsepower from Cars93
#Cars93$Horsepower

Cars93 %>% select(Type,Horsepower)

# keyboard shortcut for 'pipe' is cntrl + shift + m, (MacOS: cmd + shift + m)


# filter Type==Small
#Cars93[Cars93$Type=="Small",]

Cars93 %>% filter(Type=="Small")

# combine dplyr functions:
# return Type and Horsepower, only for Small cars
Cars93 %>% 
    filter(Type=="Small") %>% 
    select(Type,Horsepower)

# this order is not recommended!:
Cars93 %>% 
    select(Type,Horsepower) %>% 
    filter(Type=="Small")


##########
# return cars that are Small or Midsize (all columns) using dplyr
Cars93 %>% 
   filter(Type=="Small" | Type == "Midsize")

# isin, '%in%' operator
Cars93 %>% 
  filter(Type %in% c("Small","Midsize"))

#######
# msleep data from tidyverse
#install.packages('tidyverse')
library(tidyverse) # load in tidyverse which contains the msleep data
data("msleep") # load the msleep data

#dplyr
msleep %>%
  filter(vore=="herbi" & awake>=12) %>%
  select(vore,awake)

# base R (combining functions in base R is not recommended)
msleep[msleep$vore=="herbi",]$name

# dplyr, using %in%
msleep %>% 
  filter(vore %in% c('herb','carni') & sleep_total >12)

