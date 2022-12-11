#generic three factor analysis with gaussian data
m0<-lmer(R~(1|X) + (1|Y), data=data, REML=FALSE)

m1<-lmer(R~A + (1|X) + (1|Y), data=data, REML=FALSE)

m2<-lmer(R~B + (1|X) + (1|Y), data=data, REML=FALSE)

m3<-lmer(R~C + (1|X) + (1|Y), data=data, REML=FALSE)

m4<-lmer(R~A+B + (1|X) + (1|Y), data=data, REML=FALSE)

m5<-lmer(R~A+C + (1|X) + (1|Y), data=data, REML=FALSE)

m6<-lmer(R~B+C + (1|X) + (1|Y), data=data, REML=FALSE)

m7<-lmer(R~A+B+C + (1|X) + (1|Y), data=data, REML=FALSE)

m8<-lmer(R~A*B + (1|X) + (1|Y), data=data, REML=FALSE)

m9<-lmer(R~A*C + (1|X) + (1|Y), data=data, REML=FALSE)

m10<-lmer(R~B*C + (1|X) + (1|Y), data=data, REML=FALSE)

m11<-lmer(R~A*B + C + (1|X) + (1|Y), data=data, REML=FALSE)

m12<-lmer(R~A*C + B + (1|X) + (1|Y), data=data, REML=FALSE)

m13<-lmer(R~B*C + A + (1|X) + (1|Y), data=data, REML=FALSE)

m14<-lmer(R~A*B + A*C + (1|X) + (1|Y), data=data, REML=FALSE)

m15<-lmer(R~A*B + B*C + (1|X) + (1|Y), data=data, REML=FALSE)

m16<-lmer(R~A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)

m17<-lmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)



#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these

