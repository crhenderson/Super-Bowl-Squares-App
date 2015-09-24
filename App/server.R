library(xtable)
library(dplyr)
library(reshape2)
library(ggplot2)
library(nnet)

#Read in data and multinomial models
data <- readRDS("../Data/Score_Data2.RDS")
fitN <- readRDS("../Models/nextfitN.RDS")
fitA <- readRDS("../Models/nextfitA.RDS")
fitB <- readRDS("../Models/nextfitB.RDS")

#Turn off Scientific Notation
options(scipen=999)


#Create probability of next score table
probability.table <- function(score1, score2, quarter, eligibility,team1,team2) {
#What is score difference between team A and team B
deficit <- score1 - score2
#What is the total score
total <- score1 + score2
#Create dataframe of new data to predict using models.
df <- data.frame(Quarter = factor(quarter, levels = c(1,2,3,4,5)), Total_Score = total, Deficit = deficit)

##Predict probabilities of next score type based on extra point elligibility.
if(eligibility == 0) {
  pred <- predict(fitN,df,"probs")
  squares <- data.frame(new.score1 = c(rep(score1,4), score1+2, score1+3,score1+6),
                        new.score2 = c(score2, score2+2, score2+3,score2+6, rep(score2,3)),
                        Probability = c(pred["End of Game"],pred["Team B Safety"],pred["Team B Field Goal"],
                                        pred["Team B Touchdown"],pred["Team A Safety"],pred["Team A Field Goal"],
                                        pred["Team A Touchdown"])
                        )
} else if (eligibility == 1) {
  pred <- predict(fitA,df,"probs")
  squares <- data.frame(new.score1 = c(rep(score1,5), score1+1, score1+2,score1+3, score1+6),
                        new.score2 = c(score2, score2+1,score2+2, score2+3,score2+6, rep(score2,4)),
                        Probability = c(pred["End of Game"],pred["Team B Extra Point"],pred["Team B Two-Point Conversionl"],
                                        pred["Team B Field Goal"],pred["Team B Touchdown"],pred["Team A Extra Point"],
                                        pred["Team A Saftey"]+pred["Team A Two-Point Conversion"],pred["Team A Field Goal"],
                                        pred["Team A Touchdown"])
  )
} else if (eligibility == 2) {
  pred <- predict(fitB,df,"probs")
  squares <- data.frame(new.score1 = c(rep(score1,5), score1+2,score1+3, score1+6),
                        new.score2 = c(score2, score2+1,score2+2, score2+3,score2+6, rep(score2,3)),
                        Probability = c(pred["End of Game"],pred["Team B Extra Point"],pred["Team B Two-Point Conversionl"]+pred["Team B Safety"],
                                        pred["Team B Field Goal"],pred["Team B Touchdown"],
                                        pred["Team A Saftey"],pred["Team A Field Goal"],
                                        pred["Team A Touchdown"])
  )
}  else pred <- NA


##Create table of all last digit combinations  
squares$new.score1 <- squares$new.score1%%10
squares$new.score2 <- squares$new.score2%%10
squares$Probability <- squares$Probability*100

options <- data.frame(new.score1 = c(rep(0,10),rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10)),
                      new.score2 = rep(0:9,10))

##Merge predicted probabilities with score combinations
final <- merge(options,squares,by=c("new.score1","new.score2"),all.x=T)%>%
  select(new.score1, new.score2, Probability)

final$Probability[is.na(final$Probability)] <- 0
final$Probability <- round(final$Probability,1)


##Create custom table using blank scatterplots
sc1 <- score1%%10
sc2 <- 9-score2%%10
par(font.lab=2)
p <- with(final, plot(new.score1, 9-new.score2, type='n',xlim=c(0,9),ylim=c(0,9),xlab='', ylab=team2, axes=FALSE))
points(sc1,sc2,pch=19,col="red",cex=4)
with(final, text(new.score1, 9-new.score2, ifelse(Probability==0,'--',paste0(Probability,'%')),offset=0.5, cex=1))
abline(v=seq(.5,8.5,1))
abline(h=seq(.5,8.5,1))
axis(side=3, at=0:9, tick=FALSE, labels=0:9)
axis(side=2, at=0:9, tick=FALSE, labels=9:0)
mtext(team1, side=3, line=3, font=2)
mtext("Red dot represents current square based on input score.",side=1,line=1,font=3)
box()

return(p)

}



# final.score.tables <- function(score1, score2) {
#   sc1 <- score1%%10
#   sc2 <- score2%%10
# 
#   games <- data %>%
#     filter(Tm_Last == sc1, Opp_Last == sc2)%>%
#     select(Game)%>%
#     unique
#   
#   final.scores <- data%>%
#     merge(games,by="Game")%>%
#     filter(Final_Score == 1)%>%
#     group_by(Tm_Last, Opp_Last)%>%
#     summarise(N=n())%>%
#     as.data.frame()%>%
#     mutate(Prob = N/sum(N)*100)%>%
#     select(Tm_Last,Opp_Last,Prob)
# 
#   options <- data.frame(Tm_Last = c(rep(0,10),rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10)),
#                         Opp_Last = rep(0:9,10))
#   final <- merge(options,final.scores,by=c("Tm_Last","Opp_Last"),all.x=T)%>%
#        select(Tm_Last, Opp_Last, Prob)
#   
#   final$Prob[is.na(final$Prob)] <- 0
# 
#   out1<-dcast(data=final,formula=Tm_Last~Opp_Last, value.var="Prob",fun.aggregate=sum)
#   rownames(out1) <- out1[,1]
#   out1 <- out1[,-1]
#   #out1 <- FlexTable(out1, add.rownames = T)
#   
#   return(out1)
# }




##Create table with historic game occurances
historic.data <- data %>% 
    filter(Score_Type != 'Start of Game')%>%
    group_by(Game, Tm_Last, Opp_Last)%>%
    summarise(N=n())%>%
    mutate(Combo = paste(Tm_Last, Opp_Last, sep=' & '))%>%
    as.data.frame%>%
    select(Combo, Tm_Last, Opp_Last, Game, N)
  
  unique.games <- historic.data%>%
    select(Game)%>%
    unique
  
  unique.combo <- historic.data%>%
    select(Combo, Tm_Last, Opp_Last)%>%
    unique%>%
    arrange(Combo)
  
  together <- merge(unique.games,unique.combo)%>%
    merge(historic.data,by=c("Game","Combo","Tm_Last","Opp_Last"),all.x=T)
  
  together$N[is.na(together$N)] <- 0
  
  historic.data <- together%>%  
    group_by(Combo,Tm_Last,Opp_Last,N)%>%
    summarise(Games = n())
  
games <- length(unique(data$Game))

  rm(together); rm(unique.combo); rm(unique.games);

##Historic Combinations

historic.table <- function(sc1, sc2) {
  hist.data <- historic.data %>% 
    filter(N != 0)%>%
    group_by(Combo,Tm_Last,Opp_Last)%>%
    summarise(Games=sum(Games))%>%
    as.data.frame%>%
    mutate(Probs = round(Games/games,4))
  
  par(font.lab=2)
  p <- with(hist.data, plot(Tm_Last, 9-Opp_Last, type='n',xlim=c(0,9),ylim=c(0,9),xlab='', ylab='Vertical Team', axes=FALSE))
  points(sc1,9-sc2,pch=19,col="firebrick1",cex=4)
  with(hist.data, text(Tm_Last, 9-Opp_Last, ifelse(Probs==0,'--',scales::percent(Probs)),offset=0.5, cex=1))
  abline(v=seq(.5,8.5,1))
  abline(h=seq(.5,8.5,1))
  axis(side=3, at=0:9, tick=FALSE, labels=0:9)
  axis(side=2, at=0:9, tick=FALSE, labels=9:0)
  mtext('Horizontal Team', side=3, line=3, font=2)
  box()
  
  return(p)  
  
}


##Create Histogram
historic.hist <- function(score1, score2) {
  hist.data <- historic.data
  
  input.combo <- paste(score1, score2, sep=' & ')
  
  plot.data <- hist.data %>%
    filter(Combo==input.combo)
  
  g <- ggplot(plot.data, aes(x=N,y=Games)) +
    theme_bw() +
    geom_bar(fill='red',stat="identity")+
    xlab("Occurrences Per Game") +
    scale_x_continuous(breaks=0:10)
    
  
  return(g)
    
}


historic.pergame <- function(score1, score2) {
  hist.data <- historic.data
  
  
  input.combo <- paste(score1, score2, sep=' & ')
  
  combo.data <- hist.data %>%
    as.data.frame%>%
    filter(Combo==input.combo)%>%
    select(-Tm_Last,-Opp_Last)
  
  total.games <- combo.data %>%
    group_by(Combo)%>%
    summarise(Games = sum(Games))%>%
    mutate(N = 'Total') %>%
    mutate(Probability = '100%')%>%
    select(Combo,N,Games,Probability)
  
  output <- combo.data %>%
    mutate(Probability = paste(round((Games/sum(Games)*100),1),'%',sep=''))%>%
    rbind(total.games)
  
  return(output)
  
}      

historic.total <- function(score1, score2) {
  hist.data <- historic.data
  
  
  input.combo <- paste(score1, score2, sep=' & ')
  
  combo.data <- hist.data %>%
    filter(Combo==input.combo)
  
  output <- combo.data %>%
    dplyr::mutate(Total.Occurances = sum(N*Games))%>%
    as.data.frame()%>%
    select(Combo, Total.Occurances)%>%
    unique
  
  return(output)
  
}  

#Example table for documentation page
example.table <- function() {
  par(font.lab=2)
  p <- plot(0:9, 0:9, type='n',xlim=c(0,9),ylim=c(0,9),xlab='', ylab='Patriots', axes=FALSE)
  points(4,2,pch=19,col="firebrick1",cex=4)
  abline(v=seq(.5,8.5,1))
  abline(h=seq(.5,8.5,1))
  axis(side=3, at=0:9, tick=FALSE, labels=0:9)
  axis(side=2, at=0:9, tick=FALSE, labels=9:0)
  mtext('Cowboys', side=3, line=3, font=2)
  box()
  
  return(p)  
  
}



##Shiny server output
shinyServer(
  function(input, output) {  
    output$table <-renderPlot({ 
            probability.table(input$num,input$num2,input$qtr, input$ep,input$tm1, input$tm2)
          })
    output$hist <- renderPlot({ 
      historic.hist(input$hor,input$ver)
    })
    output$total <- renderTable({ 
      historic.total(input$hor,input$ver)
    })
    output$pergame <- renderTable({ 
      historic.pergame(input$hor,input$ver)
    })
    output$table.hist <- renderPlot({ 
      historic.table(input$hor,input$ver)
    })
    output$example <- renderPlot({ 
      example.table()
    })
    } 
)
