#loading library
library(tidyverse)
#read.csv(chickwts)
#chickwts <- read_csv ("chickwts.csv")
glimpse(chickwts)
summary(chickwts)
plot(chickwts)

chickwts %>% 
  group_by(feed) %>%
  summarise(n = n(),
            average = mean(weight),
            SD = sd(weight))


#box plot of feed vs weight
ggplot(chickwts, aes( x = feed, y =  weight )) + 
  geom_boxplot()

#box plot of feed vs weight
ggplot(chickwts,aes(x=feed, y=weight)) + 
  geom_jitter() + 
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult=1),
               color="red")


# Inferential Statistics
chickwts_lm <- lm(weight ~ feed , data =  chickwts)
chickwts_lm
anova(chickwts_lm)


# Rate of change
chickwts_lm <- chickwts_lm %>% 
  chickwts_lm <- (diff [weight ~ feed] - value[data =  chickwts]) %>% 
  arrange(-diff)


chickwts.av <- aov(weight ~ feed, data = chickwts)


#measure and compare feed supplements on the growth rate of chickens

require(stats); require(graphics)
boxplot(weight ~ feed, data = chickwts, col = "lightgray",
        varwidth = TRUE, notch = TRUE, main = "chickwts",
        ylab = "Weight at six weeks (gm)")
anova(lm(weight ~ feed, data = chickwts))


