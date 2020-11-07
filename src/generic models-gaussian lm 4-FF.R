library(lme4)
#generic 4-fixed factor models

m0<-lm(R~1, data=data) #this one will be at the end to keep index number and model number the same.
m1<-lm(R~A, data=data) 
m2<-lm(R~B, data=data) 
m3<-lm(R~C, data=data) 
m4<-lm(R~D, data=data) 

m5<-lm(R~A+B, data=data) 
m6<-lm(R~A+C, data=data) 
m7<-lm(R~A+D, data=data) 
m8<-lm(R~B+C, data=data) 
m9<-lm(R~B+D, data=data) 
m10<-lm(R~C+D, data=data) 
m11<-lm(R~A+B+C, data=data) 
m12<-lm(R~A+B+D, data=data) 
m13<-lm(R~A+C+D, data=data) 
m14<-lm(R~B+C+D, data=data) 
m15<-lm(R~A+B+C+D, data=data) 

m16<-lm(R~A*B, data=data) 
m17<-lm(R~A*C, data=data) 
m18<-lm(R~A*D, data=data) 
m19<-lm(R~B*C, data=data) 
m20<-lm(R~B*D, data=data) 
m21<-lm(R~C*D, data=data) 

m22<-lm(R~A*B + C, data=data) 
m23<-lm(R~A*B + D, data=data) 
m24<-lm(R~A*C + B, data=data) 
m25<-lm(R~A*C + D, data=data) 
m26<-lm(R~A*D + B, data=data) 
m27<-lm(R~A*D + C, data=data) 
m28<-lm(R~B*C + A, data=data) 
m29<-lm(R~B*C + D, data=data)
m30<-lm(R~B*D + A, data=data) 
m31<-lm(R~B*D + C, data=data) 
m32<-lm(R~C*D + A, data=data) 
m33<-lm(R~C*D + B, data=data) 

m34<-lm(R~A*B + C + D, data=data) 
m35<-lm(R~A*C + B + D, data=data) 
m36<-lm(R~A*D + B + C, data=data) 
m37<-lm(R~B*C + A + D, data=data) 
m38<-lm(R~B*D + A + C, data=data) 
m39<-lm(R~C*D + A + B, data=data) 


m40<-lm(R~A*B + A*C, data=data) 
m41<-lm(R~A*B + A*D, data=data) 
m42<-lm(R~A*B + B*C, data=data) 
m43<-lm(R~A*B + B*D, data=data) 
m44<-lm(R~A*B + C*D, data=data) 
m45<-lm(R~A*C + A*D, data=data) 
m46<-lm(R~A*C + B*C, data=data) 
m47<-lm(R~A*C + B*D, data=data)
m48<-lm(R~A*C + C*D, data=data) 
m49<-lm(R~A*D + B*C, data=data) 
m50<-lm(R~A*D + B*D, data=data) 
m51<-lm(R~A*D + C*D, data=data) 
m52<-lm(R~B*C + B*D, data=data) 
m53<-lm(R~B*C + C*D, data=data) 
m54<-lm(R~B*D + C*D, data=data) 

m55<-lm(R~A*B + A*C + D, data=data) 
m56<-lm(R~A*B + A*D + C, data=data) 
m57<-lm(R~A*B + B*C + D, data=data) 
m58<-lm(R~A*B + B*D + C, data=data) 
m59<-lm(R~A*C + A*D + B, data=data) 
m60<-lm(R~A*C + B*C + D, data=data) 
m61<-lm(R~A*C + C*D + B, data=data) 
m62<-lm(R~A*D + B*D + C, data=data) 
m63<-lm(R~A*D + C*D + B, data=data) 
m64<-lm(R~B*C + B*D + A, data=data) 
m65<-lm(R~B*C + C*D + A, data=data) 
m66<-lm(R~B*D + C*D + A, data=data) 

m67<-lm(R~A*B + A*C + A*D, data=data) 
m68<-lm(R~A*B + A*C + B*C, data=data) 
m69<-lm(R~A*B + A*C + B*D, data=data) 
m70<-lm(R~A*B + A*C + C*D, data=data) 
m71<-lm(R~A*B + A*D + B*C, data=data) 
m72<-lm(R~A*B + A*D + B*D, data=data) 
m73<-lm(R~A*B + A*D + C*D, data=data) 
m74<-lm(R~A*B + B*C + B*D, data=data) 
m75<-lm(R~A*B + B*C + C*D, data=data) 
m76<-lm(R~A*B + B*D + C*D, data=data) 
m77<-lm(R~A*C + A*D + B*C, data=data) 
m78<-lm(R~A*C + A*D + B*D, data=data) 
m79<-lm(R~A*C + A*D + C*D, data=data) 
m80<-lm(R~A*C + B*C + B*D, data=data) 
m81<-lm(R~A*C + B*C + C*D, data=data) 
m82<-lm(R~A*C + B*D + C*D, data=data) 
m83<-lm(R~A*D + B*C + B*D, data=data) 
m84<-lm(R~A*D + B*C + C*D, data=data) 
m85<-lm(R~A*D + B*D + C*D, data=data) 
m86<-lm(R~B*C + B*D + C*D, data=data) 


m87<-lm(R~A*B + A*C + B*C + D, data=data) 
m88<-lm(R~A*B + A*D + B*D + C, data=data) 
m89<-lm(R~A*C + A*D + C*D + B, data=data) 
m90<-lm(R~B*C + B*D + C*D + A, data=data) 


m91<-lm(R~A*B + A*C + A*D + B*C, data=data) 
m92<-lm(R~A*B + A*C + A*D + B*D, data=data) 
m93<-lm(R~A*B + A*C + A*D + C*D, data=data) 
m94<-lm(R~A*B + A*C + B*C + B*D, data=data) 
m95<-lm(R~A*B + A*C + B*C + C*D, data=data) 
m96<-lm(R~A*B + A*C + B*D + C*D, data=data) 
m97<-lm(R~A*B + A*D + B*C + B*D, data=data) 
m98<-lm(R~A*B + A*D + B*C + C*D, data=data) 
m99<-lm(R~A*B + A*D + B*D + C*D, data=data) 
m100<-lm(R~A*B + B*C + B*D + C*D, data=data) 
m101<-lm(R~A*C + A*D + B*C + B*D, data=data)
m102<-lm(R~A*C + A*D + B*C + C*D, data=data) 
m103<-lm(R~A*C + A*D + B*D + C*D, data=data) 
m104<-lm(R~A*C + B*C + B*D + C*D, data=data) 
m105<-lm(R~A*D + B*C + B*D + C*D, data=data) 


m106<-lm(R~A*B + A*C + A*D + B*C + B*D, data=data) 
m107<-lm(R~A*B + A*C + A*D + B*C + C*D, data=data) 
m108<-lm(R~A*B + A*C + A*D + B*D + C*D, data=data) 
m109<-lm(R~A*B + A*C + B*C + B*D + C*D, data=data) 
m110<-lm(R~A*B + A*D + B*C + B*D + C*D, data=data) 
m111<-lm(R~A*C + A*D + B*C + B*D + C*D, data=data) 

m112<-lm(R~A*B + A*C + A*D + B*C + B*D + C*D, data=data) 

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, 
             m59, m60, m61, m62, m63, m64, m65, m66, m67, m68, m69, m70, m71, m72, 
             m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m83, m84, m85, m86, 
             m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, m99, m100, 
             m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, m0)

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
