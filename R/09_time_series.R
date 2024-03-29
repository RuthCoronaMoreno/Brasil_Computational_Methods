# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to perform Time Series
# For more explanations follow:
# https://scientific-computing.netlify.app/09_population_models/09_time_series.html
# --------------------------------------------------#


library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)


covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")
head(covid)

# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date) # require(lubridate)
summary(covid$date)

# Checking the class
class(covid$date)

# Now we can make numeric operations
range(covid$date)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

#Oops. We have negative cases and will substitute the negative values per zero.
covid$new_confirmed[covid$new_confirmed < 0] <- 0

#Rolling mean (Useful to show the tendency)
covid$roll_mean <-zoo::rollmean(covid$new_confirmed,14, fill=NA)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks="4 months", date_labels = "%Y-%m")+
  labs(x = "Date", y = "New cases")+
  geom_line(aes(x=date, y=roll_mean), color='red', size=1.2)



