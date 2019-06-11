rm(list = ls())

#### Required packages ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(treemap)
library(RColorBrewer)

#### Importing deliveries ####
deliveries <- read.csv(file.choose())
matches <- read.csv(file.choose())

#### Subset Data
matches <- m %>% filter(season >= 2016 & season <= 2019)
deliveries <- d %>% filter(match_id >= 577 & match_id <= 636 | match_id >= 1 & match_id <= 59 | match_id >= 7894 & match_id <= 7953 | match_id >= 11137 & match_id <= 11415)

table(matches$season)
table(deliveries$match_id)

#### Change Data type
deliveries$match_id <- as.factor(d$match_id)
matches$season <- as.factor(m$season)

#### Deliveries Check ####
dim(deliveries)
names(matches)
str(deliveries)
psych::describe(matches)
mlr::summarizeColumns(matches)

#### Finding missing value ####
colSums(is.na(matches))
table(is.na(matches))

#get percentage of missing value of the attributes - Approach 2 (Function)
sapply(matches, function(df)
{
  round(sum(is.na(df)==T)/length(df)*100,2)
})

#Approach - Amelia Package
# install.packages("Amelia")
library("Amelia")
missmap(deliveries, main = "Missing Map")

#### Deliveries Wrangling ####
matches <- matches[,-18]
deliveries$wickets <- as.numeric(ifelse(deliveries$player_dismissed =="" ,"",1))
teams <- deliveries %>% select(batting_team)%>% distinct()
teams <- rename(teams, team = batting_team)  
s_team <- c("SRH","RCB","MI","RPS","GL","KKR","KXIP","DD","RPS","CSK","RR","DCs")
teams <- cbind(teams, s_team)
player_of_match <- matches%>% select(id,player_of_match,season) %>% distinct()
player_of_match <- rename(player_of_match, player=player_of_match)
Season <- data.frame(season=c(2016,2017,2018,2019),
                     T_winner=c("Sunrisers Hyderabad","Mumbai Indians","Chennai Super Kings","Mumbai Indians"))
matches$city <- as.character(matches$city)
matches$city[matches$city==""] <- "Dubai"
venue_city <- matches %>% select(city) %>% distinct()

-----------------------------------------------------------------------------------------

#### Runs scored by tournament winners in each season ####
batting_TW <- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  semi_join(Season, by=c("season"="season","batting_team"="T_winner"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(season,batting_team,s_team)%>%
  summarize(runs =sum(total_runs))
batting_TW

ggplot(batting_TW,aes(x=season,y=runs,colour=batting_team,fill=batting_team))+
  geom_bar(position = "stack",  show.legend = TRUE, width = .6,stat="identity")+
  geom_text(aes(label=s_team,hjust=-.45))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Total Runs by Tournament winners by season")


#### Runs scored by each team in 1s, 2s, 3s, 4s,and 6s ####
runs_cat <- deliveries %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(s_team,batsman_runs)%>%
  summarize(no=n(),runs=sum(total_runs))
runs_cat$batsman_runs <- as.factor(runs_cat$batsman_runs)
runs_cat

ggplot(runs_cat,aes(x=s_team,y=runs,colour=batsman_runs,fill=batsman_runs))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="Runs")+
  scale_x_discrete(name="Teams")+
  ggtitle("Total runs scored in 1s to 6s")


#### Highest run scorers in each season ####
batsmen <- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  group_by(season,batsman)%>%
  summarize(runs =max(sum(batsman_runs,na.rm=TRUE)))%>%
  arrange(season,desc(runs))%>%
  filter(runs==max(runs))
batsmen

ggplot(batsmen,aes(x=season,y=runs,colour=batsman,fill=batsman))+
  geom_bar(position = "stack",  show.legend = TRUE, width = .7,stat="identity")+
  geom_text(aes(label=batsman,hjust=-.25))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Highest run scorers by season")

# or

match_full <- deliveries%>%
  left_join(matches, by = c("match_id" = "id"))

match_full%>%
  group_by(season,batsman)%>%
  summarize(total_runs = sum(batsman_runs))%>%
  filter(total_runs == max(total_runs))%>%
  ggplot(aes(x = season,y = total_runs,fill = batsman))+
  geom_bar(stat ="identity")+
  ggtitle("Highest run scorer each season")+
  theme(axis.text.x = element_text(angle = 0))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  geom_text(aes(label= total_runs, vjust= 1.50))+
  scale_y_continuous(name="Total Runs", breaks = c(0,250,500,750,1000,1250))

#### Strike rate of all batsman ####
Bat_sr<- deliveries %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(batsman)%>%
  summarize(balls=n(),runs=sum(batsman_runs))%>%
  mutate(sr=runs*100/balls)%>%
  arrange(desc(sr))%>%
  mutate(sr_grp=ifelse(sr<100,"100",ifelse(sr<150,"100-150","150+")))%>%
  mutate(player_lab=ifelse(batsman=="AD Russell","AD Russell",ifelse(batsman=="V Sehwag","V Sehwag",ifelse(batsman=="V Kohli","V Kohli",ifelse(batsman=="CH Gayle","CH Gayle","")))))
Bat_sr

ggplot(Bat_sr,aes(x=sr,y=runs,colour=sr_grp,fill=sr_grp,size=runs))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_lab,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Runs")+
  scale_x_continuous(name="strike rate")+
  ggtitle("strike rate for batsman   ")


#### Number of Toss and Match wins by each team ####
toss <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())
toss
toss$type <- "toss"

wins <-matches%>%
  left_join(teams,by=c("winner"="team") )%>%
  select(s_team,winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())
wins$type <- "wins"
wins

toss_w <- rbind(toss,wins)

toss_w <- toss_w %>%
  group_by(s_team, type)%>%
  summarize(wins=sum(wins))
toss_w

ggplot(toss_w,aes(x=s_team,y=wins,colour=type,fill=type))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss and Match Wins")+
  scale_x_discrete(name="Toss and Match winner")+
  ggtitle("Toss and Match wins by each Team")


#### Toss decisions of toss winners ####
wins_1 <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner,toss_decision)%>%
  group_by(s_team,toss_decision)%>%
  summarize(wins=n())
wins_1

ggplot(wins_1,aes(x=s_team,y=wins,colour=toss_decision,fill=toss_decision))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss decision")+
  scale_x_discrete(name="Toss winners and toss decisions")+
  ggtitle("Toss decisions by each Team")


#### All Cities with Average runs, Average wickets per match and matches played ####
venue_c <- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  select(match_id,city,total_runs,wickets)%>%
  group_by(city)%>%
  summarize(runs=sum(total_runs),wickets=sum(wickets,na.rm=TRUE))
venue_c

city_mat <- matches %>%
  group_by(city)%>%
  summarize(matches=n())
city_mat

venue_c <- venue_c %>%
  left_join(city_mat, by=c("city"="city"))%>%
  mutate(Avg_runs=runs/matches)%>%
  mutate(Avg_wkt =wickets/matches)%>%
  arrange(city)
venue_c

venue_all <- venue_c%>%
  left_join(venue_city, by=c("city"="city"))%>%
  arrange(Avg_runs)
venue_all

venue_all$city <- factor(venue_all$city, levels = venue_all$city[order(venue_all$Avg_runs)])

ggplot(venue_all,aes(x=city,y=Avg_runs,colour=city,fill=city))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  scale_y_continuous(name="Average runs per match")+
  scale_x_discrete(name="Cities ")+
  ggtitle("Average score per match  at each city")

venue_all$city <- factor(venue_all$city, levels = venue_all$city[order(venue_all$Avg_wkt)])

ggplot(venue_all,aes(x=city,y=Avg_wkt,colour=city,fill=city))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  scale_y_continuous(name="Average wickets per match")+
  scale_x_discrete(name="Cities ")+
  ggtitle("Average wickets per match at each city")

venue_all$city <- factor(venue_all$city, levels = venue_all$city[order(venue_all$matches)])

ggplot(venue_all,aes(x=city,y=matches,colour=city,fill=city))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  scale_y_continuous(name="Total no of Matches in each city")+
  scale_x_discrete(name="Cities ")+
  ggtitle("Cities with most no of matches")


#### Matches with highest winning margins by runs and wickets ####
win_r <- matches%>%
  left_join(teams,by=c("winner"="team"))%>%
  select(s_team,id,winner,win_by_runs,win_by_wickets)%>%
  arrange(desc(win_by_runs),desc(win_by_wickets))%>%
  filter(!is.na(s_team))
win_r

win_w <-win_r[,-4]
win_r <- win_r[,-5]

win_w <- win_w %>% arrange(desc(win_by_wickets)) %>% filter(win_by_wickets>8)
win_r <- win_r %>% arrange(desc(win_by_runs)) %>% filter(win_by_runs>60)

win_w$win_by_wickets <-as.numeric(win_w$win_by_wickets)
win_r$win_by_runs <-as.numeric(win_r$win_by_runs)

win_r$id <- factor(win_r$id, levels = win_r$id[order(win_r$win_by_runs)])

ggplot(win_r,aes(x=id,y=win_by_runs,colour=s_team,fill=s_team))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
  scale_y_continuous(name="winning margins by runs")+
  scale_x_discrete(name="Match ID's ")+
  ggtitle("Highest winning margins by runs")

win_w$id <- factor(win_w$id, levels = win_w$id[order(win_w$win_by_wickets)])

ggplot(win_w,aes(x=id,y=win_by_wickets,colour=s_team,fill=s_team))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
  scale_y_continuous(name="winning margins by wickets")+
  scale_x_discrete(name="Match ID's")+
  ggtitle("Highest winning margins by wickets")


#### Highest runs scorers across all seasons ####
runs_h <- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  select(batsman,season,s_team,batsman_runs)%>%
  group_by(batsman,season,s_team)%>%
  summarize(runs=sum(batsman_runs))%>%
  arrange(desc(batsman))
runs_h

run_gh<-runs_h%>%
  group_by(batsman)%>%
  summarize(runs_t=sum(runs))
run_gh

runs_ht <- merge(runs_h,run_gh)
runs_ht <- runs_ht%>%
  arrange(desc(runs_t))%>%
  filter(runs_t>2500)

runs_ht$season<-as.character(runs_ht$season)
runs_ht$batsman <- factor(runs_ht$batsman, levels = run_gh$batsman[order(run_gh$runs_t)])

ggplot(runs_ht,aes(x=batsman,y=runs,colour=season,fill=season))+
  geom_bar(position = "stack",stat = "identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="Total runs scored across all seasons")+
  scale_x_discrete(name="Players")+
  ggtitle("Highest run scorers across all seasons")

### or ### 

df<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>% filter(runs > 3000) 
df %>% ggplot(aes(reorder(batsman,runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+ xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F) + coord_flip() +
  geom_text(aes(label = runs, runs = runs + 0.05), position = position_dodge(0.9), vjust = 0.5)

### or ###

deliveries %>% 
  group_by(batsman) %>% 
  summarise(total_runs = sum(batsman_runs)) %>% 
  arrange(desc(total_runs)) %>% 
  top_n(n = 10, wt = total_runs) %>% 
  ggplot(aes(x = reorder(batsman, -total_runs), y = total_runs))+
  geom_bar(aes(fill = batsman),stat = "identity")+
  labs(list(title = "Top 10 Batsman", x = "Batsman", y = "Total Runs"))+
  geom_text(aes(label = total_runs, total_runs = total_runs + 0.05), position = position_dodge(0.9), vjust = 1.5)+
  theme(axis.text.x=element_text(angle=0, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))


#### Top 10 batsman who has most number of sixes
deliveries%>%
  group_by(batsman)%>%
  filter(batsman_runs == 6)%>%
  summarize(sixes = n())%>%
  top_n(10)%>%
  ggplot(aes(x = reorder(batsman, sixes),y = sixes))+
  geom_bar(stat = "identity",fill = "orange")+
  coord_flip()+
  xlab("Players")+
  ggtitle("Top ten players with most sixes in IPL career")+
  geom_text(aes(label = sixes),hjust =1.25)


##### Top 5 batsman who has most no. of fours
deliveries %>%
  group_by(batsman)%>%
  filter(batsman_runs == 4)%>%
  summarize(fours = n())%>%
  top_n(10)%>%
  ggplot(aes(x = reorder(batsman,fours),y = fours))+
  geom_bar(stat = "identity",fill = "slategray4", width =  0.75)+
  coord_flip()+
  xlab("Players")+
  ggtitle("Top 5 batsmans with most fours in IPL career")+
  geom_text(aes(label = fours), hjust =1.25)

-----------------------------------------------------------------------------------------

#### Most batsmen are dismissed being caught ####
dismissal <- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  filter(dismissal_kind!="")%>%
  group_by(season,dismissal_kind,s_team)%>%
  summarize(wickets =n())
dismissal

ggplot(dismissal,aes(x=dismissal_kind,y=wickets,colour=as.factor(season),fill=as.factor(season)))+
  geom_bar(position = "stack", show.legend = TRUE, width =.9,stat="identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="wickets")+
  scale_x_discrete(name="dismissal kind")+
  ggtitle("Breakdown of dismissal type")

# or

deliveries%>%
  filter(dismissal_kind == "bowled")%>%
  count(bowler)%>%
  arrange(desc(n))%>%
  top_n(5)%>%
  ggplot(aes(reorder(x = bowler,-n),y =n, fill = bowler))+
  geom_bar(stat="identity", show.legend = FALSE)+
  labs(x = "Bowlers",y = "Wickets", title = "Top 5 bowled wickets")+
  geom_text(aes(label = n), vjust = 1.5)


#### Wickets taken by tournament winners in each season ####
bowling_TW <- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  semi_join(Season, by=c("season"="season","bowling_team"="T_winner"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(season,bowling_team,s_team)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))
bowling_TW

ggplot(bowling_TW,aes(x=season,y=wicket,colour=bowling_team,fill=bowling_team))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity")+
  geom_text(aes(label=s_team,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Total wickets by Tournament winners by season")


#### Highest Wicket takers in each season ####
bowler<- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(season,bowler)%>%
  summarize(wicket =max(sum(wickets,na.rm=TRUE)))%>%
  arrange(season,desc(wicket))%>%
  filter(wicket==max(wicket))
bowler

ggplot(bowler,aes(x=season,y=wicket,colour=bowler,fill=bowler))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .7,stat="identity")+
  geom_text(aes(label=bowler,hjust=-.25))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Highest wickter takers by season")

# or

bowler_wicket <- c("caught","caught and bowled", "bowled","hit wicket", "lbw","stumped","obstructing the field	")
match_full%>%
  group_by(season, bowler)%>%
  summarise(wickets =  sum(dismissal_kind %in% bowler_wicket))%>%
  filter(wickets == max(wickets))%>%
  ggplot(aes(x= season, y = wickets, fill = bowler))+
  geom_bar(stat = "identity")+
  labs(y = "Wickets" ,title = "Leading wicket takers in a season")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  geom_text(aes(label = wickets, vjust =1.50))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))


#### Players with most player of Match awards in each season ####
MOM_ply <-player_of_match %>%
  group_by(season,player)%>%
  summarize(awards =n())%>%
  filter(awards==max(awards))
MOM_ply

MOM_bat<- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  group_by(season,batsman)%>%
  summarize(runs =sum(batsman_runs,na.rm=TRUE))
MOM_bat <- rename(MOM_bat,player=batsman)
MOM_bat

MOM_ball<- deliveries%>%
  left_join(matches,by=c("match_id"="id" ))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(season,bowler)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))
MOM_ball <- rename(MOM_ball,player=bowler)
MOM_ball

MOM_field <-deliveries%>%
  left_join(matches,by=c("match_id"="id" ))%>%
  group_by(season,fielder)%>%
  summarize(catches =sum(wickets,na.rm=TRUE))
MOM_field <- rename(MOM_field,player=fielder)
MOM_field

MOM <- MOM_ply%>%
  left_join(MOM_bat,by=c("player"="player","season"="season"))%>%
  left_join(MOM_ball,by=c("player"="player","season"="season"))%>%
  left_join(MOM_field,by=c("player"="player","season"="season"))%>%
  group_by(season,player)%>%
  summarize(awards=max(awards),runs=sum(runs,na.rm=TRUE),wickets=sum(wicket,na.rm=TRUE),catches=sum(catches,na.rm=TRUE))
MOM

MOM <- gather(MOM, type, scores,3:6)
type <- MOM$type=="runs"
MOM_r <-MOM[type,]
type_n <- MOM$type !="runs"
MOM_n <- MOM[type_n,]
MOM_r

ggplot(MOM_r,aes(x=season,y=scores,colour=player,fill=player))+
  geom_bar(position = "stack", show.legend = FALSE, width = .6,stat="identity")+
  theme(legend.position="none")+
  geom_text(aes(label=player,hjust=-.25, colour="green"))+
  coord_flip()+
  scale_y_continuous(name="Total Runs")+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Total Runs for Most no of Player of Match awards in a season")

ggplot(MOM_n,aes(x=season,y=scores,colour=type,fill=type))+
  geom_bar(position = "dodge", show.legend = TRUE, width = .6,stat="identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="Wickets,catches and NO of awards")+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Most no of Player of Match awards in a season")

# or

matches%>%
  group_by(player_of_match)%>%
  summarize(most_award = n())%>%
  top_n(10)%>%
  ggplot(aes(x =  reorder(player_of_match, most_award), y = most_award))+
  geom_bar(stat = "identity", fill = "steel blue")+
  geom_text(aes(label= most_award, hjust= 1.25, vjust= -0.00))+
  coord_flip()+
  xlab("Players")




#### Total wickets taken by each team across all seasons ####
team_wickets <- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(season,bowling_team,s_team)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))
team_wickets

ggplot(team_wickets,aes(x=season,y=wicket,colour=s_team,fill=s_team))+
  geom_line(show.legend = TRUE, size =1.25,linetype=1)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(limits = c(60,125))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Total wickets by teams by season")






#### Strike rate and Economy rate for all bowlers ####
ball_sr<- deliveries %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(bowler)%>%
  summarize(balls=n(),runs=sum(total_runs,na.rm=TRUE))
ball_sr

ball_wk <-deliveries %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(bowler)%>%
  summarize(wickets=sum(wickets,na.rm=TRUE))
ball_wk

ball_sr <-ball_sr%>%
  left_join(ball_wk,by=c("bowler"="bowler"))%>%
  mutate(sr=runs/wickets)%>%
  mutate(er=runs/(balls/6))%>%
  arrange(desc(sr))%>%
  mutate(sr_grp=ifelse(sr<10,"10",ifelse(sr<40,"11-40","41+")))%>%
  mutate(er_grp=ifelse(er<6,"6",ifelse(er<10,"6-10","11+")))%>%
  mutate(player_l=ifelse(bowler=="SL Malinga","SL Malinga",ifelse(bowler=="DJ Bravo","DJ Bravo",ifelse(bowler=="R Ashwin","R Ashwin",ifelse(bowler=="DW Steyn","DW Steyn","")))))
ball_sr

ggplot(ball_sr,aes(x=sr,y=wickets,colour=sr_grp,fill=sr_grp,size=wickets))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_l,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Wickets")+
  scale_x_continuous(name="strike rate ")+
  ggtitle("strike rate for bowlers  ")

ggplot(ball_sr,aes(x=er,y=runs,colour=er_grp,fill=er_grp,size=runs))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_l,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Runs")+
  scale_x_continuous(name="Economy rate ")+
  ggtitle("Economy rate for bowlers  ")


#### Best Death bowlers
deliveries%>%
  filter(over >= 15)%>%
  group_by(bowler)%>%
  summarise(total_wickets = sum(dismissal_kind %in% bowler_wicket))%>%
  arrange(desc(total_wickets))%>%
  top_n(10)%>%
  ggplot(aes(reorder(x =bowler,total_wickets),y= total_wickets,fill = bowler))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  labs(x = "Bowlers", y = "Wickets",title = "Top Death bowlers by total wickets")+
  geom_text(aes(label = total_wickets),hjust =1.5)+
  coord_flip()









#### Against which bowlers have the top run-getters performed? ####
Kohli<- deliveries%>% group_by(batsman,bowler)%>% filter(batsman=="V Kohli")%>% summarise(runs=sum(batsman_runs))%>% top_n(n=50,wt=runs)

treemap(Kohli, #Your deliveries frame object
        index=c("batsman","bowler"),  #A list of your categorical variables
        vSize = "runs",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = brewer.pal(7,"Reds"),  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 12,
        fontfamily.title = "serif",
        fontfamily.labels = "symbol",
        title = "Runs by Virat Kohli against different bowlers",
        fontface.labels = "bold",
        border.col="#FFFFFF",
        fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
        aspRatio= 1.1)


#### Top run getters performance against different teams ####
df<-deliveries %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="RV Uthappa")  %>% 
  group_by(batsman,bowling_team) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs >100)

df %>% group_by(batsman,bowling_team) %>% summarise(runs=sum(runs)) %>% arrange(desc(runs))

treemap(df, #Your deliveries frame object
        index=c("batsman","bowling_team"),  #A list of your categorical variables
        vSize = "runs", 
        vColor = "bowling_team",
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = brewer.pal(12,"Set3"),  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 15,
        fontfamily.title = "serif",
        fontfamily.labels = "symbol",
        title = "Runs against diff teams",
        aspRatio = 1,
        border.col="#FFFFFF",bg.labels = "#FFFFFF" ,fontcolor.labels= "black",fontsize.legend = 0)


#### Batting with whom have our top run getters been successful? ####
df<-deliveries %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="RV Uthappa") %>% 
  group_by(batsman,non_striker) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs >100)

treemap(df, #Your deliveries frame object
        index=c("batsman","non_striker"),  #A list of your categorical variables
        vSize = "runs", 
        vColor = "batsman",
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = brewer.pal(6,"Set1"),  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 15,
        fontfamily.title = "serif",
        fontfamily.labels = "italic",
        title = "Runs with different players at the other end ",
        aspRatio = 1,
        border.col="#FFFFFF",bg.labels = "black" ,fontcolor.labels= "#FFFFFF",fontsize.legend = 0)


#### Type of Dismissals ####
df<-deliveries %>% filter(player_dismissed=="V Kohli"| player_dismissed=="SK Raina" |player_dismissed=="RG Sharma"|
                            player_dismissed=="RV Uthappa") %>% group_by(player_dismissed,dismissal_kind) %>% 
  summarise(type= length(dismissal_kind))

treemap(df, #Your deliveries frame object
        index=c("player_dismissed","dismissal_kind"),  #A list of your categorical variables
        vSize = "type", 
        vColor = "dismissal_kind",
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = brewer.pal(6,"Set2"),  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 15,
        fontfamily.title = "serif",
        fontfamily.labels = "italic",
        title = "Type of Dismissals ",
        aspRatio = 1,
        border.col="#FFFFFF",bg.labels = "black" ,fontcolor.labels= "#FFFFFF",fontsize.legend = 0)

# Or

dismissaltype <- function(name){
  data <- read.csv("D:/KNOWLEDGE KORNER/ANALYTICS/MISC/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL_2019/ipl_08-19/deliveries.csv")
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
  library(dplyr)
  library(data.table)
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
  geom_bar(stat = "identity", position = position_stack(), color = "black")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
print(g)


#### Strike rate in different stages of the game ####
deliveries %>% 
  filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="RV Uthappa") %>% 
  group_by(batsman,over) %>% summarise(strike= mean(batsman_runs)*100) %>%  
  ggplot(aes(over,strike, col=batsman)) + geom_line(size=2) + ylab("Strike Rate") + 
  ggtitle("Strike rate in different stages of the game ") + scale_x_continuous(breaks = 1:20)

#### Best powerplay bowlers
deliveries%>%
  filter(over < 7)%>%
  group_by(bowler)%>%
  summarise(total_wickets = sum(dismissal_kind %in% bowler_wicket))%>%
  arrange(desc(total_wickets))%>%
  top_n(10)%>%
  ggplot(aes(reorder(x =bowler,total_wickets),y= total_wickets,fill = bowler))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  labs(x = "Bowlers", y = "Wickets",title = "Top powerplay bowlers by total wicket")+
  geom_text(aes(label = total_wickets),hjust =1.5)+
  coord_flip()


#### Different type of extra runs conceded by the team ####
ipl <- function(name){
  data <- read.csv("D:/KNOWLEDGE KORNER/ANALYTICS/MISC/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/IPL_2019/ipl_08-19/deliveries.csv")
  teamname <- levels(data[, 3])
  teamnamematrix <- data.frame(teamname)
  colnames(teamnamematrix) <- "Team Name"
  teamid <- subset(data, batting_team == name)
  teamname_flag <- FALSE
  for(i in 1:length(teamname)){
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
  library(ggplot2)
  ggplot(givenruns, aes(x = TeamName ,y = Runs, fill = DifferentRuns)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    scale_fill_manual(values=c("goldenrod1","grey27","pink4","chocolate3",
                               "lightblue3","royalblue4")) + 
    ggtitle(v)
}
ipl("Kolkata Knight Riders")
ipl("Rajasthan Royals")
ipl("Deccan Chargers")
ipl("Chennai Super Kings")
ipl("Mumbai Indians")
ipl("Sunrisers Hyderabad")
ipl("Mumbai Indians")
ipl("Kings XI Punjab")
ipl("Royal Challengers Bangalore")
ipl("Delhi Daredevils")

# "Rajasthan Royals","Deccan Chargers","Chennai Super Kings","Kolkata Knight Riders",
# "Mumbai Indians","Sunrisers Hyderabad","Mumbai Indians","Kings XI Punjab",
# "Royal Challengers Bangalore","Delhi Daredevils",


#### Highest wicket takers across all seasons ####
wickets_h <- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  filter(dismissal_kind!="run out")%>%
  select(bowler,season,s_team,wickets)%>%
  group_by(bowler,season,s_team)%>%
  summarize(wickets=sum(wickets,na.rm=TRUE))%>%
  arrange(desc(bowler))
wickets_h

wickets_gh<-wickets_h%>%
  group_by(bowler)%>%
  summarize(wickets_t=sum(wickets))
wickets_gh

wickets_ht <- merge(wickets_h,wickets_gh)
wickets_ht <- wickets_ht%>%
  arrange(desc(wickets_t))%>%
  filter(wickets_t>75)
wickets_ht

wickets_ht$season<-as.character(wickets_ht$season)
wickets_ht$bowler <- factor(wickets_ht$bowler, levels = wickets_gh$bowler[order(wickets_gh$wickets_t)])

ggplot(wickets_ht,aes(x=bowler,y=wickets,colour=season,fill=season))+
  geom_bar(position = "stack",stat = "identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="Total wickets across all seasons")+
  scale_x_discrete(name="Bowlers")+
  ggtitle("Highest wicket takers across all seasons")

### or

df<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
df %>% ggplot(aes(reorder(bowler,wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F) + coord_flip() +
  geom_text(aes(label = wickets, wickets = wickets + 0.05), position = position_dodge(0.9), vjust = 0.5)

### or

deliveries %>% 
  group_by(bowler) %>% 
  summarise(total_wickets = length(dismissal_kind[dismissal_kind%in% c("caught","bowled","lbw","stumped","caught and bowled","hit wicket")])) %>% 
  arrange(desc(total_wickets)) %>% 
  top_n(n= 10, wt = total_wickets) %>% 
  ggplot(aes(x = reorder(bowler, -total_wickets), y= total_wickets))+
  geom_bar(aes(fill= bowler), stat = "identity")+
  labs(list(title = "Top 10 Bowlers", x = "Bowler", y = "Total Wickets"))+
  geom_text(aes(label = total_wickets, total_wickets = total_wickets + 0.05), position = position_dodge(0.9), vjust = 1.5)+
  theme(axis.text.x=element_text(angle=0, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))


#### Players with most number of catches and run outs ####
catches_h <- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  filter(fielder!="")%>%
  select(fielder,season,s_team,wickets)%>%
  group_by(fielder,season,s_team)%>%
  summarize(catches=sum(wickets,na.rm=TRUE))%>%
  arrange(desc(fielder))
catches_h

catches_gh <-catches_h%>%
  group_by(fielder)%>%
  summarize(catches_t=sum(catches))
catches_gh

catches_ht <- merge(catches_h,catches_gh)
catches_ht <- catches_ht%>%
  arrange(desc(catches_t))%>%
  filter(catches_t>45)
catches_ht

catches_ht$season<-as.character(catches_ht$season)
catches_ht$fielder <- factor(catches_ht$fielder, levels = catches_gh$fielder[order(catches_gh$catches_t)])

ggplot(catches_ht,aes(x=fielder,y=catches,colour=season,fill=season))+
  geom_bar(position = "stack",stat = "identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="Total catches & run outs across all seasons")+
  scale_x_discrete(name="fielders")+
  ggtitle("Highest catches and run outs across all seasons")


#### Players scoring most number of runs between 50 and 99 in a inning ####
fifties_h <- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  select(match_id,batsman,season,s_team,batsman_runs)%>%
  group_by(match_id,batsman,season,s_team)%>%
  summarize(runs=sum(batsman_runs,na.rm=TRUE))%>%
  mutate(fifties=ifelse(runs<50,"49",ifelse(runs<100,"50+","100+")))%>%
  group_by(batsman,season,s_team,fifties)%>%
  summarize(no=n(),runs=sum(runs))%>%
  arrange(desc(batsman))%>%
  filter(fifties!=49)
fifties_h

fifties_gh <-fifties_h%>%
  filter(fifties!="100+")%>%
  group_by(batsman)%>%
  summarize(fifties_t=sum(no))
fifties_gh

fifties_ht <- merge(fifties_h,fifties_gh)
fifties_ht <- fifties_ht%>%
  filter(fifties!="100+")%>%
  arrange(desc(fifties_t))%>%
  filter(fifties_t>15)
fifties_ht

fifties_ht$season<-as.character(fifties_ht$season)
fifties_ht$batsman <- factor(fifties_ht$batsman, levels = fifties_gh$batsman[order(fifties_gh$fifties_t)])

ggplot(fifties_ht,aes(x=batsman,y=no,colour=season,fill=season))+
  geom_bar(position = "stack",stat = "identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="No of Fifties in a inning across all seasons")+
  scale_x_discrete(name="Batsman")+
  ggtitle("Batsman with highest no of Fifties across all seasons")


#### Players scoring most number of runs above 100 in a inning ####
century_gh <-fifties_h%>%
  filter(fifties!="50+")%>%
  group_by(batsman)%>%
  summarize(centuries_t=sum(no))
century_gh

centuries_ht <- merge(fifties_h,century_gh)
centuries_ht <- centuries_ht%>%
  filter(fifties!="50+")%>%
  arrange(desc(centuries_t))
centuries_ht

centuries_ht$season<-as.character(centuries_ht$season)
centuries_ht$batsman <- factor(centuries_ht$batsman, levels = century_gh$batsman[order(century_gh$centuries_t)])

ggplot(centuries_ht,aes(x=batsman,y=no,colour=season,fill=season))+
  geom_bar(position = "stack",stat = "identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="No of centuries in a inning across all seasons")+
  scale_x_discrete(name="Batsman")+
  ggtitle("Batsman with highest no of centuries across all seasons")

