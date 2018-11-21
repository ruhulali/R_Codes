names(deliveries)
dim(deliveries) #nrow & ncol
table(is.na(deliveries)) #is.na
colSums(is.na(deliveries)) #column with NA count
describe(deliveries) #library(psych)
class(deliveries) #variable class
table(deliveries$batting_team) #frequency distribution of a categorical variable
levels(deliveries$batting_team) #values in a variable
filter(deliveries, batsman_runs>4) #filter

sixes <- NULL
source("https://sebastiansauer.github.io/Rcode/find_funs.R")
find_funs("grid.arrange")

install.packages("Rcpp", dependencies = T)

library(plyr)
library(psych)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)  # for percentage scales

data(tips, package = "reshape2")
View(tips)
describe(tips)

##Way 1

tips %>% 
  count(day) %>% 
  mutate(perc = n / nrow(tips)) -> tips2
View(tips2)

ggplot(tips2, aes(x = reorder(day,-perc), y = perc)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 2)), size = 3, hjust = 0.5, vjust = 2)


##Way 2

ggplot(tips, aes(x = day)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))

##Way 3

myplot <- ggplot(tips, aes(day)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

myplot

##Way 4

myplot <- ggplot(tips, aes(day, group = sex)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~sex)

myplot

##Way 5

ggplot(tips, aes(x= day,  group=sex)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day") + facet_grid(~sex) +
  scale_y_continuous(labels = scales::percent)


################# charts #################
data("tips", package = "reshape2")
View(mtcars)

#1
library(ggplot2)
library(scales)
library(gridExtra)
install.packages("gridExtra", dependencies = T)

ggplot(mtcars, aes(x = as.factor(gear))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) + scale_y_continuous(labels = percent) +
  labs(title = "Manual vs. Automatic Frequency", y = "Percent", x = "Automatic Transmission")

#2
p1 <- ggplot(mtcars, aes(reorder(row.names(mtcars), mpg), mpg)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = mpg), nudge_y = 2)

p2 <- ggplot(mtcars, aes(reorder(row.names(mtcars), mpg), mpg)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = mpg), nudge_y = -2, color = "white")

grid.arrange(p1, p2, ncol = 2)


#3
deliveries <- read.csv("D:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
matches <- read.csv("D:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/matches.csv")

matches_won <-as.data.frame(table(matches$winner))
colnames(matches_won)[2] <-"Won"
matches_played <-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2] <-"Played"
ggplot(left_join(matches_played,matches_won),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1))+
  geom_bar(stat = "identity") + geom_text(aes(label = round(Won*100/Played)), size = 3.5, hjust = 0.5, vjust = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Team")+
  ylab("Win Percentage") +  guides(fill=FALSE) + coord_cartesian(ylim = c(0, 100))


#4
library(tidyverse)
library(forcats)

theTable <- data.frame(
  Name = c('James', 'Frank', 'Jean', 'Steve', 'John', 'Tim'),
  Position = c('Zoalkeeper', 'Zoalkeeper', 'Defense',
               'Defense', 'Defense', 'Striker'))

theTable %>%
  count(Position) %>%
  mutate(Position = fct_reorder(Position, n, .desc = TRUE)) %>%
  ggplot(aes(x = Position, y = n)) + geom_bar(stat = 'identity')+
  geom_text(aes(label = n), size = 3.5, hjust = 0.5, vjust = 2)+
  xlab("Count") + ylab("Position") +  guides(fill=TRUE)

## or ##

dplyr::count(theTable, Position) %>%
  arrange(-n) %>%
  mutate(Position = factor(Position, Position)) %>%
  ggplot(aes(x=Position, y=n)) + geom_bar(stat="identity")+
  geom_text(aes(label = n), size = 3.5, hjust = 0.5, vjust = 2)+
  xlab("Count") + ylab("Position") +  guides(fill=TRUE)

####################################