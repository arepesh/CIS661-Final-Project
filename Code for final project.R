#Reading in dataset
knitr::opts_chunk$set(echo = TRUE)
library(gt)
library(tidyverse)
library(ggplot2)
library(readr)
birth <- read_csv("NationalAndStatePregnancy_PublicUse.csv")

#Select and filtering
birth2 <- birth %>%
  select("state", "year", "abortionrate2024" : "abortionrate40plus") %>%
  filter(year >= 2000, state != "US" & state != "DC") 

pivot_birth <- birth2 %>%
  pivot_longer(!state:year, names_to = "groups", values_to = "rates")

state2 <- c("ME", "MA", "RI", "CT", "NH", "VT", "NY", "PA", "NJ", "DE", "MD", "WV", "VA", "KY", "TN", "NC", "SC", "GA", "AL", "MS", "AR", "LA", "FL", "OH", "IN", "MI", "IL", "MO", "WI", "MN", "IA", "KS", "NE", "SD", "ND", "TX", "OK", "NM", "AZ", "CO", "WY", "MT", "ID", "WA", "OR", "UT", "NV", "CA", "AK", "HI")
region <- c("Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", 
            "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", "Southeast", 
            "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest", "Midwest",
            "Southwest", "Southwest", "Southwest", "Southwest",
            "West", "West", "West", "West", "West", "West", "West", "West", "West", "West", "West")
stateregion <- data.frame(state2, region)
stateregion

is.element(pivot_birth$state, stateregion$state2) %>%
  all()

pivot_birth_region <- merge(pivot_birth, stateregion, by.x = c("state"), by.y = c("state2"), all.x = FALSE)


#Southwest abortion rates
Southwest <- function(x){
  filter(pivot_birth_region, year==x, region == "Southwest") %>%
    group_by(groups, year) %>%
    summarise(Rate_Avg = mean(rates))}
Southwest(2000)

#West Abortion rates
West <- function(x){
  filter(pivot_birth_region, year==x, region == "West") %>%
  group_by(groups, year) %>%
    summarise(Rate_Avg = mean(rates))}
West(2000)

#Midwest abortion rates
Midwest <- function(x){
  filter(pivot_birth_region, year==x, region == "Midwest") %>%
  group_by(groups, year) %>%
    summarise(Rate_Avg = mean(rates))}
Midwest(2000)

#Northeast abortion rates

Northeast <- function(x){
  filter(pivot_birth_region, year==x, region == "Northeast") %>%
    group_by(groups, year) %>%
    summarise(Rate_Avg = mean(rates))}
Northeast(2000)

#Southeast Abortion rates

Southeast <- function(x){
  filter(pivot_birth_region, year==x, region == "Southeast") %>%
    group_by(groups, year) %>%
    summarise(Rate_Avg = mean(rates))}
Southeast(2000)
          
#Line graph for Southwest
Southwest1 <- pivot_birth_region %>%
  filter(region == "Southwest") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates)) %>%
  ggplot(mapping=aes(x=year, y=Rate_Avg, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Southwest region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 1")

Southwest1
          
#line graph for west
West1 <- pivot_birth_region %>%
  filter(region == "West") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates)) %>%
  ggplot(mapping=aes(x=year, y=Rate_Avg, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the West region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 2")

West1
          
#Line graph for Midwest
Midwest1 <- pivot_birth_region %>%
  filter(region == "Midwest") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates)) %>%
  ggplot(mapping=aes(x=year, y=Rate_Avg, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Midwest region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 3")

Midwest1
          
#Line graph for Northeast
Northeast1 <- pivot_birth_region %>%
  filter(region == "Northeast") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates)) %>%
  ggplot(mapping=aes(x=year, y=Rate_Avg, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Northeast region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 4")

Northeast1
          
#Line graph for Southeast
Southeast1 <- pivot_birth_region %>%
  filter(region == "Southeast") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates)) %>%
  ggplot(mapping=aes(x=year, y=Rate_Avg, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Southeast region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 5")

Southeast1
          
#Simple Southwest
simplesouthw <- pivot_birth_region %>%
  filter(region == "Southwest") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates))

simplesouthw

#SLR for Simple Southwest
ggplot(data=simplesouthw, aes(x=year, y=Rate_Avg)) +
  geom_point() +
  geom_smooth(method = lm, se= FALSE) +
  facet_wrap(vars(groups))+
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Southwest region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 6")
          
#Model for Simple Southwest
model <- lm(Rate_Avg ~ year + groups, data=simplesouthw)
model %>%
  broom::tidy()
          
#Simple Midwest
simplemid <- pivot_birth_region %>%
  filter(region == "Midwest") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates))
          
#SLR for simple Midwest
ggplot(data=simplemid, mapping=aes(x=year, y=Rate_Avg)) +
  geom_point() +
  geom_smooth(method = lm, se=F) +
  facet_wrap(vars(groups)) +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Midwest region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 7")
          
#Model for simple Midwest
model <- lm(Rate_Avg ~ year + groups, data=simplemid)
summary(model)
model %>%
  broom::tidy()
          
#Simple West
simplewest <- pivot_birth_region %>%
  filter(region == "West") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates))
          
#SLR for simple West
ggplot(data=simplewest, mapping=aes(x=year, y=Rate_Avg)) +
            geom_point() +
            geom_smooth(method = lm, se=F) +
            facet_wrap(vars(groups)) +
            labs(x = "Year",
                 y= "Average abortion rates",
                 title = "Average abortion rates per year in the West region from 2000-2017",
                 caption = "*Rates are per 1,000 women",
                 tag = "Fig. 8")
          
#Model for simple West
model <- lm(Rate_Avg ~ year + groups, data=simplewest)
summary(model)
model %>%
  broom::tidy()
          
#Simple Northeast
simplenorth <- pivot_birth_region %>%
  filter(region == "Northeast") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates))
          
#SLR for simple Northeast
ggplot(data=simplenorth, mapping=aes(x=year, y=Rate_Avg)) +
  geom_point() +
  geom_smooth(method = lm, se=F) +
  facet_wrap(vars(groups)) +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Northeast region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 9")
          
#Model for simple Northeast
model <- lm(Rate_Avg ~ year + groups, data=simplenorth)
summary(model)
model %>%
  broom::tidy()
          
#Simple Southeast
simplesouthe <- pivot_birth_region %>%
  filter(region == "Southeast") %>%
  group_by(groups, year) %>%
  summarise(Rate_Avg = mean(rates))

simplesouthe
          
#SLR for simple Southeast
ggplot(data=simplesouthe, mapping=aes(x=year, y=Rate_Avg)) +
  geom_point() +
  geom_smooth(method = lm, se=F) +
  facet_wrap(vars(groups)) +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in the Southeast region from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 10")

#Model for simple Southeast
model <- lm(Rate_Avg ~ year + groups, data=simplesouthe)
summary(model)
model %>%
  broom::tidy()
          
#Average abortion rates for Michigan
MI <- pivot_birth %>%
  filter(state=="MI") %>%
  group_by(groups, year) %>%
  ggplot(mapping=aes(x=year, y=rates, color=groups)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y= "Average abortion rates",
       title = "Average abortion rates per year in Michigan from 2000-2017",
       caption = "*Rates are per 1,000 women",
       tag = "Fig. 11")

MI

#adding variables to the for the sas code
abortion2014 <- birth2 %>%
  filter(year == 2014)

abortion2014

state <- c("WA", "OR", "CA", "NV", "MT", "WV", "CO", "NM", "MN", "DE", "IA", "IL", "WY", "MD", "NJ", "NY", "CT", "MA", "VT", "NH", "ME", "AK", "HI", "RI", "PA", "MI", "KY", "TN", "NC", "SC", "GA","ID", "UT", "AZ", "ND", "SD", "NE", "KS", "OK", "TX", "WI", "MO", "AR", "LA", "MS", "AL", "FL", "IN", "OH", "VA")
rank <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
staterank <- data.frame(state, rank)

staterank

is.element(abortion2014$state, staterank$state) %>%
  all()

Abortionrateswithrank <- merge(abortion2014, staterank, by = 'state', all.x = FALSE)

is.element(Abortionrateswithrank$state, stateregion$state2) %>%
  all()

Abortionrateswithrankandregion <- merge(Abortionrateswithrank, stateregion, by.x = c("state"), by.y = c("state2"), all.x = FALSE)

#getting average abortion rate and rank
AveragesperRegion <- Abortionrateswithrankandregion %>%
  group_by(region) %>%
  summarise(Avg_Rate2024 = mean(abortionrate2024), 
            Avg_Rate2529 = mean(abortionrate2529), 
            Avg_Rate3034 = mean(abortionrate3034),
            Avg_Rate3539 = mean(abortionrate3539),
            Avg_Rate40plus = mean(abortionrate40plus),
            Avg_Rank = mean(rank),
            Avg_Rank_Round = round(Avg_Rank))

AveragesperRegion

write.csv(Abortionrateswithrank, "Abortion_rates.csv")
write.csv(Abortionrateswithrankandregion, "Abortion_rates_Regions.csv")