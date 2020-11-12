install.packages('plotly')
install.packages("randomForest")
install.packages("randomForestExplainer")

library(ggplot2)
library(corrplot)
library(tidymodels)
library(tidyverse)
library(cluster) 
library(fpc)
library(gridExtra)
library(readr)
library(ggmap)
library(rworldmap)
library(ggpubr)
library(car)
library(caret)
library(forecast)
library(zoo)
library(ggfortify)
library(arules)
library(arulesViz)
library(plotly)
library(randomForest)
library(randomForestExplainer)

## IPL data upto season 2019

deliveries <- read_csv("deliveries.csv")
matches <- read_csv("matches.csv")

deliveries$wickets <- as.numeric(ifelse(deliveries$player_dismissed=="", "",1))

## Data wrangling

teams1<- deliveries %>%
  select(batting_team) %>%
  distinct()

teams1<- rename(teams1, team = batting_team)
teams1<- teams1[-c(14,15),]

s_team <- c("SRH","RCB","MI","RPS","GL","KKR","KXIP","DD","CSK","RR","DC","KTK","PWI")
teams1 <- cbind(teams1, s_team)

player_of_match <- matches%>% select(id,player_of_match,season) %>%
  distinct()
player_of_match <- rename(player_of_match, player=player_of_match)

Season <- data.frame(season=c(2008,2009,2010,2011,2012,2013,2014,2015,2016),T_winner=c("Rajasthan Royals","Deccan Chargers","Chennai Super Kings","Chennai Super Kings","Kolkata Knight Riders","Mumbai Indians","Kolkata Knight Riders","Mumbai Indians","Sunrisers Hyderabad"))

matches$city <- as.character(matches$city)
matches$city[matches$city==""] <- "Dubai"
venue_city <- matches %>%
  select(city)%>%
  distinct()

## Most successful teams in the ipl

success_team<-matches %>%
  group_by(winner) %>%
  summarise(winner_cnt = n())

success_team[success_team=="Delhi Daredevils"] <-"Delhi Capitals"

success_team[success_team=="Rising Pune Supergiant"]<- "Rising Pune Supergiants"

ggplot(data = success_team,aes(winner,winner_cnt,fill=winner_cnt))+
  geom_bar(position = 'dodge', stat = "identity")+
  scale_y_continuous(name = "Matches won",labels = scales::comma) +
  ggtitle("Most successfull teams in the IPL by number of wins")+
  ylab("Matches won")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

matches_won<-as.data.frame(table(matches$winner))
colnames(matches_won)[2]<-"Won"
matches_played<-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[2]<-"Played"

ggplot(left_join(matches_played,matches_won ),aes(reorder(Var1,-Won/Played),Won*100/Played,fill = Var1)) +geom_bar(stat = "identity", color="black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Win Percentage") +
  ggtitle("Most successfull teams in the IPL by win percentage")+
  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))

### Most successful batsmen

bastmen<- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  group_by(season,batsman)%>%
  summarize(runs =max(sum(batsman_runs,na.rm=TRUE)))%>%
  arrange(season,desc(runs))%>%
  filter(runs==max(runs))

ggplot(bastmen,aes(x=season,y=runs,colour=batsman,fill=batsman))+
  geom_bar(position = "stack",  show.legend = FALSE, width = .6,stat="identity", color="black")+
  geom_text(aes(label=batsman,hjust=-.25, colour="green"))+
  theme(legend.position="none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Highest run scorers by season")

### Highest wicket takers

bowler<- deliveries%>%
  left_join(matches, by=c("match_id"="id"))%>%
  group_by(season,bowler)%>%
  summarize(wicketstaken = max(sum(wickets,na.rm = TRUE)))%>%
  arrange(season,desc(wicketstaken))%>%
  filter(wicketstaken==max(wicketstaken))

ggplot(bowler,aes(x=season,y=wicketstaken,colour=bowler,fill=bowler))+
  geom_bar(position = "stack", show.legend = FALSE, width = .6, stat = "identity", color="black")+
  geom_text(aes(label=bowler,hjust=-.25,colour="green"))+
  theme(legend.position = "none")+
  coord_flip()+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Highest wicket takers by season")

### Most number of MOM awards

MOM<- player_of_match %>%
  group_by(season,player)%>%
  summarize(awards=n())%>%
  filter(awards==max(awards))

MOM_bat<- deliveries%>%
  left_join(matches,by=c("match_id"="id"))%>%
  group_by(season,batsman)%>%
  summarize(runs =sum(batsman_runs,na.rm=TRUE))

MOM_bat <- rename(MOM_bat,player=batsman)

MOM_ball<- deliveries%>%
  left_join(matches,by=c("match_id"="id" ))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(season,bowler)%>%
  summarize(wicket =sum(wickets,na.rm=TRUE))

MOM_ball <- rename(MOM_ball,player=bowler)

MOM_field <-deliveries%>%
  left_join(matches,by=c("match_id"="id" ))%>%
  group_by(season,fielder)%>%
  summarize(catches =sum(wickets,na.rm=TRUE))

MOM_field <- rename(MOM_field,player=fielder)

MOM_ply <- MOM%>%
  left_join(MOM_bat,by=c("player"="player","season"="season"))%>%
  left_join(MOM_ball,by=c("player"="player","season"="season"))%>%
  left_join(MOM_field,by=c("player"="player","season"="season"))%>%
  group_by(season,player)%>%
  summarize(awards=max(awards),runs=sum(runs,na.rm=TRUE),wickets=sum(wicket,na.rm=TRUE),catches=sum(catches,na.rm=TRUE))

MOM_ply <- gather(MOM_ply, type, scores,3:6)
type <- MOM_ply$type=="runs"
MOM_r <-MOM_ply[type,]
type_n <- MOM_ply$type !="runs"
MOM_n <- MOM_ply[type_n,]

ggplot(MOM_r,aes(x=season,y=scores,colour=player,fill=player))+
  geom_bar(position = "stack", show.legend = FALSE, width = .6,stat="identity", color="black")+
  theme(legend.position="none")+
  geom_text(aes(label=player,hjust=-.25, colour="green"))+
  coord_flip()+
  scale_y_continuous(name="Total Runs")+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Total Runs for Most no of Player of Match awards in a season")

ggplot(MOM_n,aes(x=season,y=scores,colour=type,fill=type))+
  geom_bar(position = "dodge", show.legend = TRUE, width = .6,stat="identity", color="black")+
  theme(legend.position="bottom")+
  coord_flip()+
  scale_y_continuous(name="Wickets,catches and NO of awards")+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  ggtitle("Most no of Player of Match awards in a season")

### Factors contributing to win or loss of a team

tossmatch <- matches
tossmatch<-tossmatch %>%
  filter(winner!='NA')
tossmatch<-tossmatch %>%
  filter(toss_winner!='NA')

y=0
n=0
for(i in seq(1,nrow(tossmatch)))
{
  if (tossmatch$toss_winner[i] == tossmatch$winner[i])
    y=y+1
  else 
    n=n+1
}

if (y >= n)
{
  print(paste("Yes, Toss-winning helped in winning matches."))
  print(paste("Matches won by toss_winner are: ", y, "& Total matches: ", nrow(matches)))
} else
  
{
  print(paste("No, Toss-winning didn't help in winning matches."))
  print(paste("Matches won by other team are: ", n, "& Total matches: ", nrow(matches))) 
}

winning_cnt = c(y,n)
teams = c("toss_win_team_won_match" , "toss_win_team_lost_match ")
df = data.frame(teams,winning_cnt, stringsAsFactors = F)

toss_sts = ggplot(df) + geom_bar(aes(teams,winning_cnt, fill = teams, color="black"), 
                                 stat = 'identity')
ggplotly(toss_sts)

### Home advantage

home<-matches[matches$season!="2009",]
home$date<- as.Date(home$date)
home1<-home[home$date < ("2014-04-16") | home$date > ("2014-04-30"),]
home1$home_team[home1$city=="Bangalore"]<- "Royal Challengers Bangalore"
home1$home_team[home1$city=="Chennai"]<- "Chennai Super Kings"
home1$home_team[home1$city=="Delhi"]<- "Delhi Daredevils"
home1$home_team[home1$city=="Chandigarh"]<- "Kings XI Punjab"
home1$home_team[home1$city=="Jaipur"]<- "Rajasthan Royals"
home1$home_team[home1$city=="Mumbai"]<- "Mumbai Indians"
home1$home_team[home1$city=="Kolkata"]<- "Kolkata Knight Riders"
home1$home_team[home1$city=="Kochi"]<- "Kochi Tuskers Kerala"
home1$home_team[home1$city=="Hyderabad" & home1$season <=2012]<- "Deccan Chargers"
home1$home_team[home1$city=="Hyderabad" & home1$season >2012]<- "Sunrisers Hyderabad"
home1$home_team[home1$city=="Ahmedabad"]<- "Rajasthan Royals"
home1$home_team[home1$city=="Dharamsala"]<- "Kings XI Punjab"
home1$home_team[home1$city=="Visakhapatnam" & home1$season== 2015]<- "Sunrisers Hyderabad"
home1$home_team[home1$city=="Ranchi" & home1$season== 2013]<- "Kolkata Knight Riders"
home1$home_team[home1$city=="Ranchi" & home1$season > 2013]<- "Chennai Super Kings"
home1$home_team[home1$city=="Rajkot" ]<- "Gujarat Lions"
home1$home_team[home1$city=="Kanpur" ]<- "Gujarat Lions"
home1$home_team[home1$city=="Raipur" ]<- "Delhi Daredevils"
home1$home_team[home1$city=="Nagpur" ]<- "Deccan Chargers"
home1$home_team[home1$city=="Indore" ]<- "Kochi Tuskers Kerala"
home1$home_team[home1$city=="Pune" & home1$season!= 2016]<- "Pune Warriors"
home1$home_team[home1$city=="Pune" & home1$season== 2016]<- "Rising Pune Supergiants"
home1<-home1[ which(!is.na(home1$home_team)),]
home1$win_host <- ifelse(as.character(home1$winner)==as.character(home1$home_team),"Home","Away")

home1 %>%
  group_by(win_host) %>%
  summarize((win_host=n()))

ggplot(home1[which(!is.na(home1$win_host)),],aes(win_host,fill= win_host))+geom_bar(color="black")+
  ggtitle("Is home advantage a real thing in IPL?")+
  xlab("Team")+
  ylab("Number of Matches won")+labs(aesthetic="Winner")

### Teams or players a company should endorse for its products

### Create a dataset with team names, number of wins, MOM awards, runs scored and wickets taken

comteams<-matches %>%
  group_by(winner) %>%
  summarise(winner_cnt = n())

comteams<- na.omit(comteams)

comteams[comteams=="Delhi Daredevils"] <-"Delhi Capitals"
comteams[comteams=="Rising Pune Supergiant"]<- "Rising Pune Supergiants"
comteams<-aggregate(data= comteams, winner_cnt~winner, sum)

comteams<- rename(comteams, team = winner)

team_runs <- deliveries %>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams1, by=c("batting_team"="team"))%>%
  group_by(season,batting_team,s_team)%>%
  summarize(runs= sum(total_runs))

team_runs<- rename(team_runs, team = batting_team)

team_runs<- team_runs %>%
  group_by(team)%>%
  summarize(runs= sum(runs))

comteams <- merge(comteams, team_runs, by.x= "team")

team_wickets <- deliveries %>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams1, by=c("batting_team"="team"))%>%
  group_by(season,bowling_team,s_team)%>%
  summarize(wicket= sum(wickets,na.rm = TRUE))

team_wickets<- rename(team_wickets, team = bowling_team)

team_wickets<- team_wickets %>%
  group_by(team)%>%
  summarize(wicket= sum(wicket))

comteams <- merge(comteams, team_wickets, by.x= "team")

tot_match<- matches_played

tot_match[tot_match=="Delhi Daredevils"] <-"Delhi Capitals"
tot_match[tot_match=="Rising Pune Supergiant"]<- "Rising Pune Supergiants"
tot_match<-aggregate(data= tot_match, Played~Var1, sum)

tot_match<- rename(tot_match, team= Var1)

comteams <- merge(comteams, tot_match, by.x= "team")

comteams$batting_avg<- comteams$runs / comteams$Played

dfplot <- comteams %>% gather(key, value, winner_cnt, batting_avg,wicket)

ggplot(dfplot, mapping = aes(x = team, y = value, color = key) )+
  geom_bar(position = "stack", width = .6,stat="identity", color="black")+
  theme(legend.position="none")+
  ggtitle("Probability of team getting endorsed")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
