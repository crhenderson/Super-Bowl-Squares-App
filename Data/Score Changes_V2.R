library(XML)
library(dplyr)

X<-list()
year <- 2002:2014
week <- 1:17
pb <- txtProgressBar(min = 0, max = length(year)*length(week), style = 3)
for(i in 1:length(year)) {
for(j in 1:length(week)) {
Sys.sleep(0.1)
#Pro-Football reference is missing week 2 of the 2001 season
#if(i != 4 | j!=2) {
url<-paste('http://www.pro-football-reference.com/play-index/play_finder.cgi?request=1&match=all&search=&player_id=&year_min=',year[i],'&year_max=',year[i],'&team_id=&opp_id=&game_type=&playoff_round=&game_num_min=0&game_num_max=99&week_num_min=',week[j],'&week_num_max=',week[j],'&quarter=1&quarter=2&quarter=3&quarter=4&quarter=5&tr_gtlt=lt&minutes=15&seconds=00&down=0&down=1&down=2&down=3&down=4&yds_to_go_min=&yds_to_go_max=&yg_gtlt=gt&yards=&is_first_down=-1&field_pos_min_field=team&field_pos_min=&field_pos_max_field=team&field_pos_max=&end_field_pos_min_field=team&end_field_pos_min=&end_field_pos_max_field=team&end_field_pos_max=&type=PASS&type=RUSH&type=PUNT&type=KOFF&type=ONSD&type=FG&type=XP&type=2PCR&type=2PCP&is_complete=-1&is_turnover=-1&turnover_type=interception&turnover_type=fumble&is_scoring=1&score_type=touchdown&score_type=field_goal&score_type=safety&is_sack=-1&include_kneels=-1&no_play=0&game_day_of_week=&game_location=&game_result=&margin_min=&margin_max=&order_by=yards&rush_direction=LE&rush_direction=LT&rush_direction=LG&rush_direction=M&rush_direction=RG&rush_direction=RT&rush_direction=RE&pass_location=SL&pass_location=SM&pass_location=SR&pass_location=DL&pass_location=DM&pass_location=DR',sep='')
Y<-readHTMLTable(url, stringsAsFactors = FALSE, header=T, skip.rows=1)
#}
X[[j+length(week)*(i-1)]]<-Y[[8]]
setTxtProgressBar(pb, j+length(week)*(i-1))
}}
close(pb)

Y<-do.call("rbind",lapply(X, 
          function(Data) {select(Data, Date,Tm,Opp,Quarter,Time,Score,Detail)%>%
                            mutate(Tm_Score = as.numeric(as.character(gsub("-.*","",Score))),
                                   Opp_Score = as.numeric(as.character(gsub(".*-","",Score))),
                                   Total_Score = Tm_Score+Opp_Score,
                                   First = apply(Data[,c("Tm","Opp")],1,min),
                                   Last = apply(Data[,c("Tm","Opp")],1,max),
                                   Matchup = paste(First,Last,sep="-"),
                                   Tm_Last = Tm_Score%%10,
                                   Opp_Last = Opp_Score%%10,
                                   Score_Type = ifelse(grepl("touchdown",Detail, fixed=T),'Touchdown',
                                                ifelse(grepl("extra point",Detail,fixed=T),'Extra Point',
                                                ifelse(grepl("field goal", Detail, fixed=T), 'Field Goal',
                                                ifelse(grepl("safety", Detail, fixed=T), 'Safety',
                                                ifelse(grepl("Two Point Attempt", Detail, fixed=T), 'Two-Point Conversion',
                                                            'Other')))))
                                   #Min = apply(Data[,c("Tm_Last","Opp_Last")],1,min),
                                   #Max = apply(Data[,c("Tm_Last","Opp_Last")],1,max),
                                   #Combo = paste(Min,Max,sep=" & ")
                                   )%>%
                            select(Date,Matchup,Tm,Opp,Quarter,Score,Tm_Score,Opp_Score,Total_Score,Tm_Last,Opp_Last,Score_Type)%>%
                            arrange(Date,Matchup,Quarter,Total_Score)
          }
                          ))%>%
  unique()

#Y$Min <- apply(Y[,c("Tm_Last","Opp_Last")],1,min)
#Y$Max <- apply(Y[,c("Tm_Last","Opp_Last")],1,max)
Z<-mutate(Y,
          #Combo = paste(Min,Max,sep=" & "),
          Game = paste(Date,Matchup,sep=": "))%>%
  select(Game,Date,Matchup,Tm,Opp,Quarter,Score,Total_Score,Tm_Last, Opp_Last, Tm_Score, Opp_Score,Score_Type)

remove <- unique(Z$Game[Z$Score_Type=='Other'])
Z <- Z %>% filter(!(Game %in% remove))


Max <- select(Z,Game, Total_Score)%>%
  group_by(Game)%>%
  summarize(Max_Total_Score = max(Total_Score))

Score_Data <- merge(Z,Max,by="Game")%>%
  mutate(Final_Score = ifelse(Total_Score==Max_Total_Score,1,0))

Score_Data <- Score_Data[complete.cases(Score_Data),]
Score_Data$Score <- factor(Score_Data$Score)
Score_Data$Score <- paste("[",Score_Data$Score,"]",sep="")
Score_Data <- select(Score_Data, Game, Quarter, Date, Matchup, Tm, Opp, Score, Total_Score, Tm_Last, Opp_Last, Final_Score, Score_Type, Tm_Score, Opp_Score)

Switch_Score_Data <- Score_Data %>%
  mutate(Tm2 = Opp, Opp2 = Tm, Tm_Last2 = Opp_Last, Opp_Last2 = Tm_Last, Tm_Score2 = Opp_Score, Opp_Score2 = Tm_Score)%>%
  mutate(Tm = Tm2, Opp = Opp2, Tm_Last = Tm_Last2, Opp_Last = Opp_Last2, Tm_Score = Tm_Score2, Opp_Score = Opp_Score2) %>%
  select(-Tm2, -Opp2, -Tm_Last2, -Opp_Last2,-Tm_Score2,-Opp_Score2)

Score_Data <- Score_Data %>%
  mutate(Game = paste(Game, Tm))
Switch_Score_Data <- Switch_Score_Data %>%
  mutate(Game = paste(Game, Tm))

Score_Data <- rbind(Score_Data,Switch_Score_Data)%>%
  arrange(Game,Total_Score)

unique.game <- Score_Data %>% group_by(Game, Date, Matchup, Tm, Opp)%>%summarise(MinQtr = min(Quarter))%>%
  mutate(Score = '[0-0]', Total_Score=0,Tm_Last = 0, Opp_Last = 0,
         Final_Score = 0, Quarter = 1,
         Score_Type = 'Start of Game', Tm_Score = 0, Opp_Score = 0)%>%
  select(Game, Quarter,Date, Matchup, Tm, Opp, Score, Total_Score, Tm_Last, Opp_Last, Final_Score, Score_Type,Tm_Score, Opp_Score)

Score_Data <- rbind(Score_Data, unique.game)%>%
  arrange(Game, Total_Score)

Numbered.Score.Data <- data.frame()
games <- unique(Score_Data$Game)
pb <- txtProgressBar(min = 0, max = length(games), style = 3)


for(k in 1:length(games)) {
  cache <- Score_Data%>%filter(Game==games[k])%>%arrange(Game,Total_Score)
  
  cache <- cache%>%mutate(Row = 0:(nrow(cache)-1))
  Numbered.Score.Data <- rbind(Numbered.Score.Data,cache)
  setTxtProgressBar(pb, k)
}
close(pb)


##Add who scored



priorscore <- Numbered.Score.Data %>%
  mutate(Row = Row+1)%>%
  select(Game, Prior_Tm_Score=Tm_Score, Prior_Opp_Score = Opp_Score, Row)

whoscored <- merge(Numbered.Score.Data, priorscore, by=c("Game","Row"),all.x=T)
whoscored <- whoscored %>%
  mutate(Score_Type = ifelse(Score_Type=='Start of Game','Start of Game',
                             ifelse(Tm_Score != Prior_Tm_Score, paste('Team A',Score_Type),
                                    paste('Team B', Score_Type))))%>%
  select(-Prior_Tm_Score,-Prior_Opp_Score)

priorscoretype <- whoscored %>%
  mutate(Row=Row+1)%>%
  select(Game, Prior_Score_Type = Score_Type, Row)

whoscored <- merge(whoscored, priorscoretype, by=c("Game","Row"),all.x=T)%>%
  mutate(Eligible = ifelse(Score_Type == 'Start of Game', 0,
                    ifelse(Prior_Score_Type == 'Team A Touchdown', 1,
                    ifelse(Prior_Score_Type == 'Team B Touchdown', 2, 0))))%>%
  select(-Prior_Score_Type)

nextscore <- whoscored %>%
  mutate(Row = Row-1)%>%
  select(Game, Next_Score_Type = Score_Type, Next_Eligible = Eligible,Next_Total = Total_Score, Row)

Numbered.Score.Data2 <- merge(whoscored, nextscore, by=c("Game","Row"), all.x=T)%>%select(-Eligible)
Numbered.Score.Data2$Next_Score_Type[is.na(Numbered.Score.Data2$Next_Score_Type)] <- 'End of Game'
Numbered.Score.Data2 <- Numbered.Score.Data2%>%
  mutate(Next_Eligible = ifelse(!is.na(Next_Eligible), Next_Eligible,
                                ifelse(Quarter==5,0,
                                ifelse(Score_Type %in% c("Team A Touchdown"), 1,
                                ifelse(Score_Type %in% c("Team B Touchdown"),2,3)))))%>%
  mutate(Next_Total = ifelse(!is.na(Next_Total),Next_Total,Total_Score+1))%>%
  mutate(Change = Next_Total - Total_Score)%>%
  filter(Change %in% c(1,2,3,6))%>%
  select(-Change, -Next_Total)


saveRDS(Numbered.Score.Data2, "../SquaresApp/Score_Data2.RDS")
