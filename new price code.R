# clean the environment ####
rm(list= ls())

# load packages ####

require(tidyverse)
require(lubridate)
require(broom)
require(tseries)
require(forecast)
require(urca)
require(zoo)

# setup and load data ####

setwd("C:/Users/moctar.aboubacar/Desktop/price analysis 2.0")

dat <- read_csv("combined markets.csv")

glimpse(dat)


# data cleaning and prep ####

# convert prices to double and Markets, Location, Commodities, Location code and Commodities Code to factor

factorcols <- names(dat[,1:5])
dat[,factorcols] <- map(dat[factorcols], as.factor)

dblcols <- names(dat[,6:length(dat)])
dat[,dblcols] <- map(dat[,dblcols], as.double)


# gather data into tidy 'long' format

dat.long <- gather(dat, key = date, value = price, 6:ncol(dat))

# convert data to correct type (date, factor for month and year)

dat.long <- dat.long %>% 
  mutate(date = mdy(date),
         month = month(date),
         year = year(date))

dat.long$month <- factor(dat.long$month,
                         levels = c(1:12))
dat.long$year <- factor(dat.long$year)

glimpse(dat.long)

# create unique ID for each of the 96 combinations of market/commodity
# create a counter within each of the 96 combinations, numbering the successive 

dat.2 <- dat.long%>%
  mutate(ID = group_indices_(dat.long, .dots = c('Location.code', 'Commodities.code')))%>% #create an 'ID' for each of the 96 combinations of commodities and locations
  group_by(Location.code, Commodities.code)%>%
  mutate(counter = row_number())%>%
  ungroup()

# check stationarity-unit root test ####

# create different data frames for each of the 96
# convert to time series, gather them in a list and apply the test to all areas
# draw conclusions as necessary and prep for actual ALPS model
dat.simp <- dat.2[,7]

list.1 <- split(dat.simp, dat.2$ID)

list.1 <- map(list.1, function(x) {
  ts(x)})

list.1 <- map(list.1, function(x){
  na.remove(x)
})

obj <- map(list.1, function(x){ # test for stationarity 
  adf.test(x)
})

#  extract the p-values into a vector
vec <- vector("numeric", length(obj))

for (i in seq_along(obj)) {
  
  vec[i] <- obj[[i]]$p.value

}

hist(vec)
length(which(vec < 0.05))
length(which(vec < 0.1))
plot(vec)
abline(h = 0.1, col = "red")
abline(h = 0.05, col = "blue")

# in between 30% and 40% are stationary, depending on p-value cutoff.

# Regression ####

# using a for loop to iterate over market/commodity combinations

df_list <- list()

for (i in 1:length(unique(dat.2$ID))){
  df <- dat.2%>%
    filter(ID == i)%>% # filter by commodity/location
    mutate(counter = row_number()) # create a time counter
  
  df <- df[!is.na(df$price),] # delete rows w missing price data...
  model <- lm(price ~ counter + month, data = df)
  
  fitted <- fitted(model)
  residu <- resid(model)
  
  df <- cbind(df, fitted)
  df <- cbind(df, residu)
  
  df_list[[i]] <- df
}

dat.4 <- do.call(rbind, df_list)

# calculate the ALPS indicator (stress, alert, crisis)

dat.4 <- dat.4%>%
  group_by(ID)%>%
  mutate(sd_all = sd(residu))%>%
  ungroup()%>%
  mutate(indicator = residu/sd_all,
         indi_stress = ifelse(indicator >=0.25 & indicator < 1, indicator, ""),
         indi_alert = ifelse(indicator >= 1 & indicator < 2, indicator, ""),
         indi_crisis = ifelse(indicator >= 2, indicator, ""))

dat.4 <- dat.4%>%
  mutate(dm_stress = ifelse(indicator >=0.25 & indicator < 1, 1, 0),
         dm_alert = ifelse(indicator >= 1 & indicator < 2, 1, 0),
         dm_crisis = ifelse(indicator >= 2, 1, 0)) 
  # mutate(index = case_when(dm_stress == 1 ~ "1",
  #                          dm_alert == 1, ~ "2",
  #                          dm_crisis == 1, ~ "3"))

glimpse(dat.4)


# convert character to double

char_to_dbl <- names(dat.4[16:18])
dat.4[,char_to_dbl] <- map(dat.4[char_to_dbl], as.double)


# save file to .csv
write.csv(dat.4, "C:/Users/moctar.aboubacar/Desktop/price analysis 2.0/prices-out.csv")

# try the whole thing with prices starting in 2015, see if there are more stationary series...
 
# example fitted values
dat.4 %>%
  filter(ID == 11) %>%
  ggplot(aes(x = date, y = fitted))+
  geom_line()

# (alternative way) nesting to run multiple models
# 
# dat.3 <- dat.long %>%
#   group_by(Location, Commodities) %>% 
#   nest()
# 
# model <- function(df){
#   
#   lm(price ~ date + month, data = df)
# 
# }
# 
# dat.3 <- dat.3 %>% 
#   mutate(model = map(dat.3$data, model)) %>% 
#   unnest()


# Predict values for each point, then link them up in a graph.
# figure out how to deal with missing values


