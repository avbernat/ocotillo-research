library(lme4)
#generic 4-fixed factor models

m0<-lm(R~1, data=data) #this one will be at the end to keep index number and model number the same.
m1<-lm(R~A, data=data) 
m2<-lm(R~B, data=data) 
m3<-lm(R~C, data=data) 

m4<-lm(R~A+B, data=data) 
m5<-lm(R~A+C, data=data) 
m6<-lm(R~B+C, data=data) 

m7<-lm(R~A+B+C, data=data) 

m8<-lm(R~A*B, data=data) 
m9<-lm(R~A*C, data=data) 
m10<-lm(R~B*C, data=data) 


m11<-lm(R~A*B + C, data=data) 
m12<-lm(R~A*C + B, data=data) 
m13<-lm(R~B*C + A, data=data) 


m14<-lm(R~A*B + A*C, data=data) 
m15<-lm(R~A*B + B*C, data=data) 
m16<-lm(R~A*C + B*C, data=data) 

m17<-lm(R~A*B + A*C + B*C, data=data) 


#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17,m0)

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
