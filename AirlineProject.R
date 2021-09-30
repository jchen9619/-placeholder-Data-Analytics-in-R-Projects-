#Inputs
Fare<-200
Seats<-132
Refund<-0.5
BumpCost<-2.5
NSRate<-.1
Tickets<-150

#Simple Model - using expected outcomes
NoShows<-Tickets*NSRate
Arrivals<-Tickets-NoShows
Bumps<-max(0,Tickets-NoShows-Seats)
Profit<-Tickets*Fare-NoShows*Fare*Refund-Bumps*Fare*BumpCost
Profit
Profit

#
replications<-1000
NoShows<-rbinom(replications,Tickets,NSRate)
NoShows


#sketch of model

Profit<-c()
data <- data.frame(matrix(NA,    # Create empty data frame
nrow = 100,
ncol = 0))

for(i in 0:20){  
    set.seed(123)
    Tickets=Seats + i
    NoShows<-rbinom(replications,Tickets,NSRate)
    Arrivals=Tickets- NoShows
    Bumps<-pmax(0,Tickets-NoShows-Seats)
    Profit=Tickets*Fare-NoShows*Fare*Refund-Bumps*Fare*BumpCost
    ProfitDF<-as.data.frame(Profit)
    colnames(ProfitDF)<-paste(i)
    data<-cbind(data, ProfitDF)
}
View(data)
colMeans(data)  #Average profit of overbooking 0-20 tickets, 1000 replications
MeanProfit<-as.data.frame(colMeans(data))
MeanProfit$TixOverbooked <- 1:nrow(MeanProfit) 
colnames(MeanProfit)<-c("Average Profit", "Tickets Overbooked")
View(MeanProfit)
plot(MeanProfit[2:1]) #Plot of average profit of 1000 simulations, overbooking 0 to 20 tickets
