# This activity uses MultiServerQueuing.R as a starting point (lines 1-50):
set.seed(123456)
avg_st<-1.25 #average service time ts
avg_tba<-2.5 #average time between customer arrivals ta
customers<-1000 #number of customers
servers<-2 #number of servers
TBA<-rexp(customers,1/avg_tba) #Time between customers (inter-arrival time)     
#as an exponentially distributed random variable
ST<-rexp(customers,1/avg_st) #service time as an exponentially distributed random variable.     
#The arguments of the rexp() function are n, the number of observations and rate,     
#the decay rate of an exponential function.
AT<-rep(0L, customers) #arrival time vector. The rep() function creates a vector of     
#length times filled with item x. Here, our vector of length 1000 is filled with 0     
#values  of the long variable type.
FT<-matrix(0L,customers,servers)#finish time matrix- this matrix will expand as we add servers, based 
#the if command below, to have 1 column more than the number of servers. The first column in 
#the matrix represents customer finish time, the rest represent the time at which server j 
#finishes serving customer i 
WT<-rep(0L, customers)#wait time vector
AT[1]<-TBA[1] #populates 1st customer's arrival time for AT vector
FT[1,1]<-AT[1]+ST[1] #populates 1st customer finish time 

if(servers>1){ #if there is more than one server, the code in the following for loop will run:
  for(i in 2:servers){ #loop will run for each i from 2 to the number of servers
    AT[i]<-AT[i-1]+TBA[i]  #the arrival time of customer i is the arrival time of the 
    #previous (i-1) customer + the time between customers i-1 and i
    FT[i,i]<-AT[i]+ST[i]  #the finish time for server i serving customer i is the arrival 
    #time of customer i + the service time for customer i 
  }
}
# Below is your nested for loop. The outer loop is represented by i the current time, i, and goes from the number 
# of servers + 1 to the number of customers. For each i in the outer loop, the inner loop, which is represented by j,
# goes from 1 to the number of servers. When the inner loop is done for a given i, the
# outer loop moves to the next i and the cycle begins again, until i = customers. 

for (i in (servers+1):customers){  
  AT[i]<-AT[i-1]+TBA[i]  # arrival time of customer i + time between arrival of customer 1 and customer 2 
  for(j in 1:servers){  
    #The if-else statement below says that if cust1 finishes more quickly with 
    #server j than with any other server, then: the finish time for customer 2 with server j is 
    # [the greater value of: cust2 arrival, or when cust1 is done] plus service time for cust2
    #and that the wait time to get served is the difference between the finish time of cust1 and the arrival 
    #time of the next customer. If cust1 finishes more slowly with server j, then the finish time is equal to 
    #the finish time of the previous customer
    if(FT[i-1,j]==min(FT[i-1,])){FT[i,j]<-max(AT[i],FT[i-1,j])+ST[i] 
    WT[i]<-max(0,FT[i-1,j]-AT[i])} else {FT[i,j]<-FT[i-1,j]}  
  }
} 

summary(WT)
plot(WT)
data<-as.data.frame(scale(cbind(TBA,ST,WT)))
summary(data)

#STEP 1: Sample code from Tornado.R, you need to run a regression to see which random input (TBA or ST)
# effects wait time the most. Modify and uncomment the following code: 
WT_LM<-lm(WT~TBA+ST,data=data)

#STEPS 2 and 3: Once you have the regession model you can compare the coefficients
# modify and uncomment this code: 
WT_LM$coefficients
cof<-WT_LM$coefficients[-1]
orcof<-cof[order(cof,decreasing=FALSE)]
cof
orcof

#STEP 4: Use the parplot function to create a Tornado diagram by uncommenting
#and modifying this code:
barplot(orcof, main="Tornado Graph", horiz = T, las=1, 
        xlim = c(-1,1))
