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


##############
# mutate (creating new columns)
data(Cars93)
Cars93_output <- Cars93 %>% mutate(HPperLiter = Horsepower / EngineSize) %>% 
  dplyr::select(Manufacturer,Model,Horsepower,EngineSize,HPperLiter,MPG.city)

# arrange (sorts the df)
Cars93_output <- Cars93 %>% mutate(HPperLiter = Horsepower / EngineSize) %>% 
  dplyr::select(Manufacturer,Model,Horsepower,EngineSize,HPperLiter,MPG.city) %>% 
  arrange(desc(Horsepower))

# combining dplyr with base-R
# what if you wanted to round the numbers in HPperLiter col?
# %>% round() does not work.. solution?
# could use the round() and mutate() and overwrite an existing col 
Cars93_output %>% mutate(HPperLiter = round(HPperLiter,2))

# what if you wanted to grab the mean of a col in dplyr?
Cars93_output %>% dplyr::select(Horsepower) %>% mean() # fix this sytnax error

# what if you wanted to generate a new categorical variable from a numerical one
# create a new variable named "HighPerformance" that equals one if HPperLiter >60,
# 0 otherwise
Cars93_output <- Cars93_output %>% mutate(HighPerformance = (HPperLiter>60)*1)
Cars93_output <- Cars93_output %>% mutate(HighPerformance = ifelse(HPperLiter>60,1,0))

# more than 2 categories?
# >70 give 2; between 50-70 give 1; below 50 give 0
Cars93_output <- Cars93_output %>% mutate(HighPerformance = ifelse(HPperLiter>70,2,ifelse(HPperLiter>50,1,0)))

######
# group_by and summarize
# average EngineSize in the entire data
mean(Cars93$EngineSize)
# how about average EngineSize by Type?
Cars93 %>% group_by(Type) %>%
  summarize(Avg_EngineSize = mean(EngineSize)) %>% 
  arrange(desc(Avg_EngineSize))


#####
# Tips with dplyr
# Tip 1 combining dplyr with base R
Cars93 %>%
  dplyr::select(Type,Horsepower) %>% 
  pull(Horsepower) %>% 
  mean()

Cars93 %>% 
  dplyr::select(Type,Horsepower) %>% 
  pull(Horsepower) %>% 
  hist()

# Tip 2 combining dplyr with ggplot
Cars93 %>% 
  filter(Price<40) %>% 
  ggplot(aes(x=Horsepower)) +
  geom_histogram()

Cars93 %>% 
  filter(Price<40) %>% 
  ggplot(aes(x=Horsepower,y=Price)) +
  geom_point()


#########
# get min, max, and average price; average engine size, average HP, median MPG
# highway, # of cars with 2 airbags by # of cylinders
Cars93 %>% 
  group_by(Cylinders) %>%
  summarize(min_price = min(Price), 
            max_price = max(Price), avg_price = mean(Price), 
            avg_size=mean(EngineSize),avg_power=mean(Horsepower),
            med_mpg = median(MPG.highway),
            num_airbags=sum(AirBags == "Driver & Passenger"),
            count = n())


#######
# msleep data from tidyverse
#install.packages('tidyverse')
library(tidyverse) # load in tidyverse which contains the msleep data
data("msleep") # load the msleep data

# in-class application:
# 1) return average awake time by "vore" sorted by average awake time in
# descending order
msleep %>% 
  group_by(vore) %>%
  summarize(avg_awake = mean(awake)) %>% 
  arrange(desc(avg_awake))

# 2) create a new col brain_per that shows the percentage of body weight that 
#brain takes (use the ratio brainwt/bodywt)
# note that the following does not alter the original df:
msleep_new <- msleep %>% 
  mutate(brain_per=brainwt/bodywt)

# 2') What is the average brain_per? (Hint: use na.rm=T) What does the distribution 
# of it look like? (combine dplyr with ggplot2 for this question)
msleep %>% 
  mutate(brain_per=brainwt/bodywt) %>%
  pull(brain_per) %>% 
  mean(na.rm=T)

msleep_new %>% 
  dplyr::select(brain_per) %>% 
  ggplot(aes(brain_per)) +
  geom_histogram()

# 3) Return the top 5 animals with the highest brain_per
msleep_new %>%
  dplyr::select(name,brain_per) %>% 
  arrange(desc(brain_per)) %>%
  head(n=5)


# Use ggplot2 for the following:
# 4) Make a scatterplot showing brain_per against awake
msleep %>% 
  mutate(brain_per=brainwt/bodywt) %>%
  ggplot(aes(x=brain_per, y=awake)) +
  geom_point()

# 4') Make a scatterplot showing brain_per against awake with different colors
# by "vore"
msleep %>% 
  mutate(brain_per=brainwt/bodywt) %>%
  ggplot(aes(x=brain_per, y=awake,col=vore)) +
  geom_point(size=2)






