

rm(list = ls())
install.packages("tidyverse", dependencies = T)

path = "E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl"
setwd(path)

library(ggplot2)
library(readr)
library(dplyr)
library(psych)
library(Hmisc)
library(gridExtra)
library(data.table)
library(scales)  # for percentage scales

## ipl function takes the name of the team as an input and returns a data frame which displays the
## different type of extra runs conceded by the team. It also displays a pie chart representing the
## obtained data frame. If the name of the ipl team is entered incorrectly it shows an error displaying
## all the thirteen teams.

ipl <- function(name){
  data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
  teamname <- levels(data[, 3])
  teamnamematrix <- data.frame(teamname)
  colnames(teamnamematrix) <- "Team Name"
  teamid <- subset(data, batting_team == name)
  teamname_flag <- FALSE
  for(i in 1:length(teamname)){9
    if(name == teamname[i]){
      teamname_flag <- TRUE
    }
  }
  if(!teamname_flag){
    print(teamnamematrix)
    stop("Please Enter Correct Team Name from the following\n")
  }
  wideruns <-     sum(teamid$wide_runs)
  byeruns <-      sum(teamid$bye_runs)
  legbyeruns <-   sum(teamid$legbye_runs)
  noballruns <-   sum(teamid$noball_runs)
  penaltyruns <-  sum(teamid$penalty_runs)
  batsmanruns <-  sum(teamid$batsman_runs)
  extraruns <-    sum(teamid$extra_runs)
  givenruns <- rbind.data.frame(wideruns, byeruns, legbyeruns,
                                noballruns, penaltyruns)
  rownames(givenruns) <-  c("Wide Runs", "Bye Runs", "LegBye Runs",
                            "Noball Runs", "Penalty Runs")
  colnames(givenruns) <- "Runs"
  v <- paste("Runs conceded by", as.character(name))
  print(v)
  print(givenruns)
  TeamName <- name
  DifferentRuns <- c("Wide Runs", "Bye Runs", "LegBye Runs",
                     "Noball Runs", "Penalty Runs")
  
  ggplot(givenruns, aes(x = TeamName ,y = Runs, fill = DifferentRuns)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    scale_fill_manual(values=c("goldenrod1","grey27","pink4","chocolate3",
                               "lightblue3","royalblue4")) + 
    ggtitle(v)
  
}

ipl("Chennai Super Kings")
ipl("Gujarat Lions")


## The following R script displays the bar plot of all the thirteen teams with there total runs scored 
## by the following team.

data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
CSK <- subset(data, batting_team == "Chennai Super Kings" )
DC <- subset(data, batting_team == "Deccan Chargers")
DD <- subset(data, batting_team == "Delhi Daredevils")
GL <- subset(data, batting_team == "Gujarat Lions")
KXIP <- subset(data, batting_team == "Kings XI Punjab")
KTK <- subset(data, batting_team == "Kochi Tuskers Kerala")
KKR <- subset(data, batting_team == "Kolkata Knight Riders")
MI <- subset(data, batting_team == "Mumbai Indians")
PW <- subset(data, batting_team == "Pune Warriors")
RR <- subset(data, batting_team == "Rajasthan Royals")
RPS <- subset(data, batting_team == "Rising Pune Supergiants")
RCB <- subset(data, batting_team == "Royal Challengers Bangalore")
SR <- subset(data, batting_team == "Sunrisers Hyderabad")
Total_runs_csk <- sum(CSK$total_runs)
Total_runs_dc <- sum(DC$total_runs)
Total_runs_dd <- sum(DD$total_runs)
Total_runs_gl <- sum(GL$total_runs)
Total_runs_kxip <- sum(KXIP$total_runs)
Total_runs_ktk <- sum(KTK$total_runs)
Total_runs_kkr <- sum(KKR$total_runs)
Total_runs_mi <- sum(MI$total_runs)
Total_runs_pw <- sum(PW$total_runs)
Total_runs_rr <- sum(RR$total_runs)
Total_runs_rps <- sum(RPS$total_runs)
Total_runs_rcb <- sum(RCB$total_runs)
Total_runs_sr <- sum(SR$total_runs)
Totalruns <- rbind.data.frame(Total_runs_csk, Total_runs_dc, Total_runs_dd,
                              Total_runs_gl, Total_runs_kxip, Total_runs_ktk,
                              Total_runs_kkr, Total_runs_mi, Total_runs_pw,
                              Total_runs_rr, Total_runs_rps, Total_runs_rcb,
                              Total_runs_sr)
colnames(Totalruns) <- "Total Runs"
rownames(Totalruns) <- c("CSK", "Dc", "DD", 
                         "GL", "KXIP", "KTK",
                         "KKR", "MI", "PW",
                         "RR", "RPS", "RCB",
                         "SR")
print(Totalruns)
Teams <- c("CSK", "DC", "DD", "GL", "KXIP", "KTK",
           "KKR", "MI", "PW","RR", "RPS", "RCB","SR")
Runs <- Totalruns$`Total Runs`

p <- ggplot(Totalruns, aes(x = reorder(Teams,-Runs), y = Runs, fill = Teams)) +
     geom_bar(width = 1, stat = "identity")+ geom_text(aes(label=Runs),size = 3.5, hjust = 0.5, vjust = 1.5)
plot(p)

#
p <- ggplot(Totalruns, aes(x = Teams, y = Runs), fill = Teams)+
  geom_bar(stat = "identity") + geom_text(aes(label = Runs), size = 3.5, hjust = 0.5, vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1))+
  xlab("Team")+ ylab("Runs") +  guides(fill=TRUE)
#

## The following R script displays the Top order batsman. it also shows the runs on a bar plot for the
## first ten batsaman with highest runs scored.

data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
batsmandata <- data.frame(data[, 7], data[, 16])
colnames(batsmandata) <- c("PlayerName", "RunsScored")
batsmanname <- levels(batsmandata[, 1])
individualdata <- split(batsmandata,  batsmandata[, 1])
eachplayer <- matrix(nrow = 436 , ncol = 1)
colnames(eachplayer) <- "RunsScored"
for(i in 1:436){
  eachdata <- as.data.frame(individualdata[i])
  colnames(eachdata) <- c("PlayerName", "RunsScored")
  eachplayer[i, ] <- sum(eachdata$RunsScored)
}
eachplayer_new <- data.frame(batsmanname, eachplayer[, 1])
colnames(eachplayer_new) <- c("Batsman Name", "Runs Scored")
order <- eachplayer_new[order(eachplayer_new$`Runs Scored`, decreasing = TRUE), ]
h <- order[1:10, ]
rownames(h) <- 1:10
rownames(order) <- 1:436
print(h)
pp <- ggplot(h, aes(x = `Batsman Name`, y = `Runs Scored`,
                    fill = `Batsman Name`)) + 
  geom_bar(width = 1, stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_text(aes(label = `Runs Scored`, y = `Runs Scored`),
            size = 3,  position = position_dodge(0.9), vjust = 0)
plot(pp)


## The following R script displays the top 10 bowlers. It also gives a bar plot of all these 10 bowlers.

data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
bowler <- data.frame(data[, 9], data[, 19])
colnames(bowler) <- c("Bowler Name", "PlayerDismissed")
bowlernames <- levels(bowler[, 1])
individualdata <- split(bowler, bowler[, 1])
df <- matrix(individualdata)
bw <- matrix(nrow = 334, ncol = 1)

for(i in 1:334){
  eachdata <- data.frame(individualdata[i])
  colnames(eachdata) <- c("Bowler", "Wickets Taken")
  r <- eachdata[, 2]
  r[r == ""] <- NA
  c <- complete.cases(r)
  s <- sum(c)
  bw[i, ] <- s
}
df <- data.frame(bowlernames, bw)
colnames(df) <- c("Bowler", "Wickets Taken")
o <- df[order(df$`Wickets Taken`, decreasing = TRUE), ]
h <- o[1:10, ]
rownames(h) <- 1:10
colnames(h) <- c("Bowler Name","Wickets Taken")
print(h)

pp <- ggplot(h, aes(x = reorder(`Bowler Name`,-`Wickets Taken`), y = `Wickets Taken`,fill = `Bowler Name`)) + 
      geom_bar(width = 1, stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
      geom_text(aes(label = `Wickets Taken`, y = `Wickets Taken`),
      size = 3,  position = position_dodge(0.1), vjust = 2)
plot(pp)


## The following R script helps us in determining the different ypes of dismissal type for a batsman.
## If the input name to the function is not correct it gives an error

dismissaltype <- function(name){
  data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
  names <- levels(data[, 7])
  namesmatrix <- data.frame(names)
  colnames(namesmatrix) <- "Batsman"
  batsman_flag <- FALSE
  for(i in 1:length(names)){
    if(name == names[i]){
      batsman_flag <- TRUE
    }
  }
  if(!batsman_flag){
    print(tbl_df(namesmatrix))
    stop("Please Enter Correct Batsman Name from the following")
  }
 
  subset <- select(data, batsman, bowler, player_dismissed, dismissal_kind)
  batsman <- filter(subset, batsman == name)
  dismissaltype1 <- as.data.frame(table(batsman$dismissal_kind, exclude = ""))
  dismissaltype_df <- data.frame(name, dismissaltype1)
  colnames(dismissaltype_df) <- c("Player","Dismissal Type", "Times")
  arrange(dismissaltype_df,desc(Times))
}
a <- dismissaltype("MS Dhoni")
b <- dismissaltype("V Kohli")
c <- dismissaltype("SR Tendulkar")
d <- dismissaltype("SK Raina")
m1 <- merge(a,b, all = TRUE)
m2 <- merge(c,d, all = TRUE)
mf <- merge(m1, m2, all = TRUE)
g <- ggplot(mf, aes(Player, Times, fill = `Dismissal Type`)) + 
     geom_bar(stat = "identity", position = position_stack(), color = "black")
     scale_fill_brewer(palette="Paired")+theme_minimal()
print(g)


## The following r script helps us predict the total number of matches played at different venues and
## total matches played in a season.

#install.packages("gridExtra", dependencies = T)

data <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/matches.csv")
stadium_table <- table(data$venue)
stadium_table_df <- data.frame(stadium_table)
colnames(stadium_table_df) <- c("Venue", "Matches Played")
p <- ggplot(stadium_table_df, aes(x = Venue, y = `Matches Played`, fill = Venue)) + 
     geom_bar(stat = "identity", position = position_dodge(), color = "white") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
     geom_text(aes(label = `Matches Played`, y = `Matches Played`),
     size = 2.0,  position = position_dodge(0.9), vjust = 0)
print(p)

############################################################################

matches_year <- select(data, season, venue)
year_subset <- split(matches_year, matches_year$season)
matches_year_matrix <- matrix(nrow = 9, ncol = 1)
rownames(matches_year_matrix) <- seq.int(from = 2008, to = 2016)
colnames(matches_year_matrix) <- c("Matches Played")
matches_year_df <- as.data.frame(matches_year_matrix)
for(i in 1:9){
  matches_year_df[i, ] <- length(year_subset[[i]][[1]])
}
Year <- rownames(matches_year_df)
k <- ggplot(matches_year_df, aes(x = reorder(Year,-`Matches Played`), y = `Matches Played`, fill = Year)) + 
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = `Matches Played`, y = `Matches Played`),
  size = 3, position = position_dodge(0), vjust = 2)
print(k)

#################################################################

deliveries <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
matches <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/matches.csv")

############# data check #############

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
############# data check #############

runs <- select(deliveries, batting_team, batsman_runs)
describe(runs$batsman_runs)
six <- (table(runs))
View(six)
table(runs)

x <- filter(deliveries, batting_team %in% 
              c("Kolkata Knight Riders") & batsman_runs>5)
View(x)

innings <- select(deliveries, c(inning, batting_team, sum(total_runs)))
inning_tabel <- table(innings)
table(innings)
describe(inning_tabel)

KKR_team <- subset(deliveries, batting_team == "Kolkata Knight Riders")
kkr_total_run <- sum(KKR_team$total_runs)
print(kkr_total_run)

mynewdata %>% 
  select(cyl, wt, gear) %>%
  filter(wt > 2)

mykkr <- subset(deliveries, batting_team == "Kolkata Knight Riders")
View(mykkr)


kkr_team <- filter(deliveries, batting_team == "Kolkata Knight Riders")
kkr_runs_total <- sum(kkr_team$total_runs)
print(kkr_runs_total)

View(kkr_team)

kkr_inning_runs <- filter(kkr_team, inning == "2")
kkr_total_runs <- sum(kkr_inning_runs$total_runs)
print(kkr_total_runs)

summarise_if(deliveries, is.numeric, funs(n(), mean, median))

#####################################################################################

install.packages("radarchart", dependencies = T)

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(dplyr)
library(gridExtra)
library(treemap)
library(RColorBrewer)
library(tidyr)
library(radarchart)

deliveries <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/deliveries.csv")
matches <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL/ipl/matches.csv")

matches<-matches[matches$result=="normal",]

matches[which(as.character(matches$team2)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team2)==as.character(matches$winner)),"team1"]
matches[which(as.character(matches$team1)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team1)==as.character(matches$winner)),"team2"]


matches1<-matches[matches$win_by_runs!=0,]
closeness<-function(x,y = "gold" ){
  data1<-matches1[matches1$winner==x|matches1$loser==x,]
  data1[data1$loser==x,"win_by_runs"]<- -data1[data1$loser==x,"win_by_runs"]
  ggplot(data1,aes(1:nrow(data1),win_by_runs))+ geom_area(fill=y)+ggtitle(x)+
  ylab("Runs")+ xlab("Matches")+ geom_ribbon(aes(ymin=-5, ymax=5),fill="red",alpha=0.4)+
  geom_ribbon(aes(ymin=-15, ymax=15),fill="red",alpha=0.1) +
  guides(fill=FALSE)+scale_alpha(guide = 'none')+coord_cartesian(ylim = c(-100, 100)) 
}
a<-closeness("Chennai Super Kings")
b<-closeness("Kolkata Knight Riders","purple")
c<-closeness("Sunrisers Hyderabad","orange")
d<-closeness("Mumbai Indians","blue2")
e<-closeness("Royal Challengers Bangalore","red3")
f<-closeness("Delhi Daredevils","firebrick3")
g<-closeness("Rajasthan Royals","blueviolet")
h<-closeness("Kings XI Punjab","salmon")
grid.arrange(a,b,c,d,e,f,g,h,ncol=2)

ggplot(matches[which(!is.na(matches$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
ylab("Number of Matches Played") +
guides(fill=FALSE)

ggplot(matches,aes(venue, rm.na=T)) +geom_bar(fill="#0072B2") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
ylab("Number of Matches Played")

matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")
ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ 
geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")

Data<-matches[matches$season!="2009",]
Data$date<- as.Date(Data$date)
Data1<-Data[Data$date < as.Date("2014-04-16") | Data$date > as.Date("2014-04-30"),]
Data1$home_team[Data1$city=="Bangalore"]<- "Royal Challengers Bangalore"
Data1$home_team[Data1$city=="Chennai"]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Delhi"]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Chandigarh"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Jaipur"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Mumbai"]<- "Mumbai Indians"
Data1$home_team[Data1$city=="Kolkata"]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Kochi"]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season <=2012]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season >2012]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ahmedabad"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Dharamsala"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Visakhapatnam" & Data1$season== 2015]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ranchi" & Data1$season== 2013]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Ranchi" & Data1$season > 2013]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Rajkot" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Kanpur" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Raipur" ]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Nagpur" ]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Indore" ]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Pune" & Data1$season!= 2016]<- "Pune Warriors"
Data1$home_team[Data1$city=="Pune" & Data1$season== 2016]<- "Rising Pune Supergiants"
Data1<-Data1[ which(!is.na(Data1$home_team)),]
Data1$win_host <- ifelse(as.character(Data1$winner)==as.character(Data1$home_team),"Home","Away")

ggplot(Data1[which(!is.na(Data1$win_host)),],aes(win_host,fill= win_host))+geom_bar()+
  ggtitle("Is home advantage a real thing in IPL?")+
  xlab("Team")+
  ylab("Number of Matches won")+labs(aesthetic="Winner")

ggplot(as.data.frame(table(matches$team2) + table(matches$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+
  ylab("Number of Matches") +guides(fill=FALSE)

ggplot(matches,aes(winner))+geom_bar(fill="#0072B2") + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Team") + ylab("Matches won")


matches_won<-as.data.frame(table(matches$winner))
colnames(matches_won)[2]<-"Won"
matches_played<-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2]<-"Played"
ggplot(left_join(matches_played,matches_won ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1))+
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))

ggplot(matches[matches$win_by_runs!=0,],aes(id,win_by_runs,col= winner )) + geom_point() +
  ylab("Runs won by ") + xlab("Matches won by team batting first")+
  ggtitle("Margin of Victories(Won by team batting first)")+ 
  scale_y_continuous(breaks=c(0,25,50,75,100))+
  geom_hline(yintercept = mean(matches[matches$win_by_runs!=0,]$win_by_runs),col="blue")
ggplot(matches[matches$win_by_wickets!=0,],aes(id,win_by_wickets,col= winner )) + geom_point() +
  ylab("Wickets won by ") + xlab("Matches won by team bowling first")+
  ggtitle("Margin of Victories(Won by team bowling first)")+
  scale_y_continuous(breaks=c(2,4,6,8,10))+
  geom_hline(yintercept = mean(matches[matches$win_by_wickets!=0,]$win_by_wickets),col="blue")



