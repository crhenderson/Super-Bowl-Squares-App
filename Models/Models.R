library(nnet)
library(dplyr)
mdata <- readRDS("App/Score_Data2.RDS")

mdata$Deficit <- mdata$Tm_Score - mdata$Opp_Score

mdata <- mdata %>% 
  select(Quarter, Total_Score, Deficit, Next_Eligible, Next_Score_Type)


N <- mdata %>% filter(Next_Eligible == 0)%>%mutate(Next_Score_Type = factor(Next_Score_Type), Quarter = factor(Quarter))
A <- mdata %>% filter(Next_Eligible == 1)%>%mutate(Next_Score_Type = factor(Next_Score_Type), Quarter = factor(Quarter))
B <- mdata %>% filter(Next_Eligible == 2)%>%mutate(Next_Score_Type = factor(Next_Score_Type), Quarter = factor(Quarter))

set.seed(12345)
nextfitN <- multinom(Next_Score_Type~Quarter+Total_Score+Deficit,data=N)
nextfitA <- multinom(Next_Score_Type~Quarter+Total_Score+Deficit,data=A)
nextfitB <- multinom(Next_Score_Type~Quarter+Total_Score+Deficit,data=B)

saveRDS(nextfitN,"../SquaresApp/nextfitN.RDS")
saveRDS(nextfitA,"../SquaresApp/nextfitA.RDS")
saveRDS(nextfitB,"../SquaresApp/nextfitB.RDS")
