library(lme4)
#generic 4-fixed factor models

m0<-lm(R~1, data=data) #this one will be at the end to keep index number and model number the same.
m1<-lm(R~A, data=data) 
m2<-lm(R~B, data=data) 
m3<-lm(R~A+B, data=data) 
m4<-lm(R~A*B, data=data) 

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4,m0)

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
