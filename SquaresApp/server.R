library(xtable)
library(reshape2)
library(ggplot2)
library(nnet)

#Read in data and multinomial models
data <- readRDS("Score_Data2.RDS")
fitN <- readRDS("nextfitN.RDS")
fitA <- readRDS("nextfitA.RDS")
fitB <- readRDS("nextfitB.RDS")

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
final <- merge(options,squares,by=c("new.score1","new.score2"),all.x=T)
final <- final[,c("new.score1", "new.score2", "Probability")]

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



##Create table with historic game occurances
historic.data <- data[data$Score_Type != 'Start of Game',]
historic.data <- aggregate(Score~Game+ Tm_Last+ Opp_Last, data=historic.data,length)
names(historic.data)[4] <- "N"
historic.data$Combo <- paste(historic.data$Tm_Last, historic.data$Opp_Last, sep=' & ')
historic.data <- historic.data[,c("Combo", "Tm_Last", "Opp_Last", "Game", "N")]
  
unique.games <- unique(historic.data$Game)
  
unique.combo <- historic.data[,c("Combo", "Tm_Last", "Opp_Last")]
unique.combo <- unique(unique.combo[order(unique.combo$Combo),] )

together <- merge(unique.games,unique.combo)
names(together)[1] <- "Game"
together <- merge(together,historic.data,by=c("Game","Combo","Tm_Last","Opp_Last"),all.x=T)
together$N[is.na(together$N)] <- 0

historic.data <- aggregate(Game~Combo+Tm_Last+Opp_Last+N, data=together, length)
names(historic.data)[5] <- "Games"
  
games <- length(unique(data$Game))

rm(together); rm(unique.combo); rm(unique.games);

##Historic Combinations

historic.table <- function(sc1, sc2) {
  hist.data <- historic.data[historic.data$N != 0,]
  hist.data <- aggregate(Games~Combo+Tm_Last+Opp_Last,hist.data,sum)
  hist.data$Probs <- round(hist.data$Games/games,4)
  
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
  
  plot.data <- hist.data[hist.data$Combo==input.combo,]
  
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
  
  combo.data <- hist.data[hist.data$Combo==input.combo,c(-2,-3)]
  
  total.games <- aggregate(Games~Combo,data=combo.data,sum)
  total.games$N <- 'Total'
  total.games$Probability = '100%'
  total.games <- total.games[,c("Combo","N","Games","Probability")]
  
  combo.data$Probability <-  paste(round((combo.data$Games/sum(combo.data$Games)*100),1),'%',sep='')
    
  output <- rbind(combo.data,total.games)
  
  return(output)
  
}      

historic.total <- function(score1, score2) {
  hist.data <- historic.data
  
  
  input.combo <- paste(score1, score2, sep=' & ')
  
  combo.data <- hist.data[hist.data$Combo==input.combo,]
  
  combo.data$Total.Occurances <- sum(combo.data$N*combo.data$Games)
  
  output <- unique(combo.data[,c("Combo","Total.Occurances")])
  
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
