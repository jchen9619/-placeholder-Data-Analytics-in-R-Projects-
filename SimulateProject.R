install.packages("triangle")
library(triangle)
set.seed(999)

Task1<-c()
Task2<-c()
Task3<-c()
Task4<-c()
Task5<-c()
Total<-c()

replications<-1000
for (j in seq(replications)){
  Task1<-c(Task1, rtriangle(n=1, a = 1, b = 5, c = 2))
  Task2<-c(Task2, rtriangle(n=1, a = 1, b = 5, c = 3))
  Task3<-c(Task3, rtriangle(n=1, a = 1, b = 5, c = 4))
  Task4<-c(Task4, rtriangle(n=1, a = 1, b = 7, c = 3))
  Task5<-c(Task5, rtriangle(n=1, a = 0, b = 2, c = 1))
  Total<-c(Total,Task1[j]+Task2[j]+max(Task3[j],Task4[j])+Task5[j])
  }
Total

data<-cbind(Task1,Task2,Task3,Task4,Task5,Total)
data

prob<-sum(Total>10)/length(Total)
prob

#68% probability that the project will not be completed in 10 days.