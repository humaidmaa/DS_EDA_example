# Diamond Analysis
# Mansour Alhumaid
# 05.10.2020
# A small case study for EDA and stats

# load packages
library(tidyverse)
library(rio)

jems2 <- read_csv("data/diamonds.csv")
jems2

summary(jems2)
str(jems2)
glimpse(jems2)
dim(jems2)
names(jems2)
rownames(jems2)
nrow(jems2)

jems2 %>% 
  head(5)

jems2 %>% 
  tail(5)

#basic filtering:
#are there any diamonds withVVS2 (clarity) & good (cut)
jems %>% 
  filter(clarity == "VVS2" & cut == "Good")


#How many diamonds with a clarity of category “IF” are present in the data-set?
jems2 %>% 
  filter(clarity == "IF") -> clarity_IF
clarity_IF

#What fraction of the total do they represent?
nrow(clarity)/nrow(jems2)

#What proportion of the whole is made up of each category of clarity?
jems %>% 
  group_by(jems$clarity) %>% 
  mean()

#What is the cheapest diamond price? 

min(jems2$price)

# What is the range of diamond prices?
range(jems2$price)

#What is the average diamond price in each category of cut and color?
jems2 %>% 
  group_by(cut, color) %>% 
  summarise(mean(price))

#A scatter plot of the price of a diamond as described by the carat

ggplot(jems2, aes(x= carat, y=price)) + 
  geom_point()

#applying log10 transformation to both the price and carat. 

price_log10 <- log10(jems$price)
carat_log10 <- log10(jems$carat)

jems$price_log10 <- price_log10
jems$carat_log10 <- carat_log10
jems


#A scatter plot of the price of a diamond as described by the carat with log 10 applied
ggplot(jems2, aes(x= log10_carat, y=log10_price)) + 
  geom_point()

#to recreate a model that describes the relationship shown in the plot?

jems2_lm <- lm(log10_price  ~  log10_carat, data= jems2)
jems2_lm

#geom_smooth analysis

ggplot(jems2, aes(x= log10_carat, y=log10_price)) + 
  geom_point() +
  geom_smooth(method = "lm")

#another plot
ggplot(jems, aes(carat_log10, price_log10)) +
  geom_point() +
  geom_smooth(method = 'lm' ,
              se = FALSE ,
              colour = "red")

