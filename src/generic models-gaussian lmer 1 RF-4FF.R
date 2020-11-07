library(lme4)
#generic 4-fixed factor and 1 random-factor models

m0<-lmer(R~1 + (1|X), data=data) #this one will be at the end to keep index number and model number the same.
m1<-lmer(R~A + (1|X), data=data)
m2<-lmer(R~B + (1|X), data=data)
m3<-lmer(R~C + (1|X), data=data)
m4<-lmer(R~D + (1|X), data=data)

m5<-lmer(R~A+B + (1|X), data=data)
m6<-lmer(R~A+C + (1|X), data=data)
m7<-lmer(R~A+D + (1|X), data=data)
m8<-lmer(R~B+C + (1|X), data=data)
m9<-lmer(R~B+D + (1|X), data=data)
m10<-lmer(R~C+D + (1|X), data=data)
m11<-lmer(R~A+B+C + (1|X), data=data)
m12<-lmer(R~A+B+D + (1|X), data=data)
m13<-lmer(R~A+C+D + (1|X), data=data)
m14<-lmer(R~B+C+D + (1|X), data=data)
m15<-lmer(R~A+B+C+D + (1|X), data=data)

m16<-lmer(R~A*B + (1|X), data=data)
m17<-lmer(R~A*C + (1|X), data=data)
m18<-lmer(R~A*D + (1|X), data=data)
m19<-lmer(R~B*C + (1|X), data=data)
m20<-lmer(R~B*D + (1|X), data=data)
m21<-lmer(R~C*D + (1|X), data=data)

m22<-lmer(R~A*B + C + (1|X), data=data)
m23<-lmer(R~A*B + D + (1|X), data=data)
m24<-lmer(R~A*C + B + (1|X), data=data)
m25<-lmer(R~A*C + D + (1|X), data=data)
m26<-lmer(R~A*D + B + (1|X), data=data)
m27<-lmer(R~A*D + C + (1|X), data=data)
m28<-lmer(R~B*C + A + (1|X), data=data)
m29<-lmer(R~B*C + D + (1|X), data=data)
m30<-lmer(R~B*D + A + (1|X), data=data)
m31<-lmer(R~B*D + C + (1|X), data=data)
m32<-lmer(R~C*D + A + (1|X), data=data)
m33<-lmer(R~C*D + B + (1|X), data=data)

m34<-lmer(R~A*B + C + D + (1|X), data=data)
m35<-lmer(R~A*C + B + D + (1|X), data=data)
m36<-lmer(R~A*D + B + C + (1|X), data=data)
m37<-lmer(R~B*C + A + D + (1|X), data=data)
m38<-lmer(R~B*D + A + C + (1|X), data=data)
m39<-lmer(R~C*D + A + B + (1|X), data=data)


m40<-lmer(R~A*B + A*C + (1|X), data=data)
m41<-lmer(R~A*B + A*D + (1|X), data=data)
m42<-lmer(R~A*B + B*C + (1|X), data=data)
m43<-lmer(R~A*B + B*D + (1|X), data=data)
m44<-lmer(R~A*B + C*D + (1|X), data=data)
m45<-lmer(R~A*C + A*D + (1|X), data=data)
m46<-lmer(R~A*C + B*C + (1|X), data=data)
m47<-lmer(R~A*C + B*D + (1|X), data=data)
m48<-lmer(R~A*C + C*D + (1|X), data=data)
m49<-lmer(R~A*D + B*C + (1|X), data=data)
m50<-lmer(R~A*D + B*D + (1|X), data=data)
m51<-lmer(R~A*D + C*D + (1|X), data=data)
m52<-lmer(R~B*C + B*D + (1|X), data=data)
m53<-lmer(R~B*C + C*D + (1|X), data=data)
m54<-lmer(R~B*D + C*D + (1|X), data=data)

m55<-lmer(R~A*B + A*C + D + (1|X), data=data)
m56<-lmer(R~A*B + A*D + C + (1|X), data=data)
m57<-lmer(R~A*B + B*C + D + (1|X), data=data)
m58<-lmer(R~A*B + B*D + C + (1|X), data=data)
m59<-lmer(R~A*C + A*D + B + (1|X), data=data)
m60<-lmer(R~A*C + B*C + D + (1|X), data=data)
m61<-lmer(R~A*C + C*D + B + (1|X), data=data)
m62<-lmer(R~A*D + B*D + C + (1|X), data=data)
m63<-lmer(R~A*D + C*D + B + (1|X), data=data)
m64<-lmer(R~B*C + B*D + A + (1|X), data=data)
m65<-lmer(R~B*C + C*D + A + (1|X), data=data)
m66<-lmer(R~B*D + C*D + A + (1|X), data=data)

m67<-lmer(R~A*B + A*C + A*D + (1|X), data=data)
m68<-lmer(R~A*B + A*C + B*C + (1|X), data=data)
m69<-lmer(R~A*B + A*C + B*D + (1|X), data=data)
m70<-lmer(R~A*B + A*C + C*D + (1|X), data=data)
m71<-lmer(R~A*B + A*D + B*C + (1|X), data=data)
m72<-lmer(R~A*B + A*D + B*D + (1|X), data=data)
m73<-lmer(R~A*B + A*D + C*D + (1|X), data=data)
m74<-lmer(R~A*B + B*C + B*D + (1|X), data=data)
m75<-lmer(R~A*B + B*C + C*D + (1|X), data=data)
m76<-lmer(R~A*B + B*D + C*D + (1|X), data=data)
m77<-lmer(R~A*C + A*D + B*C + (1|X), data=data)
m78<-lmer(R~A*C + A*D + B*D + (1|X), data=data)
m79<-lmer(R~A*C + A*D + C*D + (1|X), data=data)
m80<-lmer(R~A*C + B*C + B*D + (1|X), data=data)
m81<-lmer(R~A*C + B*C + C*D + (1|X), data=data)
m82<-lmer(R~A*C + B*D + C*D + (1|X), data=data)
m83<-lmer(R~A*D + B*C + B*D + (1|X), data=data)
m84<-lmer(R~A*D + B*C + C*D + (1|X), data=data)
m85<-lmer(R~A*D + B*D + C*D + (1|X), data=data)
m86<-lmer(R~B*C + B*D + C*D + (1|X), data=data)


m87<-lmer(R~A*B + A*C + B*C + D + (1|X), data=data)
m88<-lmer(R~A*B + A*D + B*D + C + (1|X), data=data)
m89<-lmer(R~A*C + A*D + C*D + B + (1|X), data=data)
m90<-lmer(R~B*C + B*D + C*D + A + (1|X), data=data)


m91<-lmer(R~A*B + A*C + A*D + B*C + (1|X), data=data)
m92<-lmer(R~A*B + A*C + A*D + B*D + (1|X), data=data)
m93<-lmer(R~A*B + A*C + A*D + C*D + (1|X), data=data)
m94<-lmer(R~A*B + A*C + B*C + B*D + (1|X), data=data)
m95<-lmer(R~A*B + A*C + B*C + C*D + (1|X), data=data)
m96<-lmer(R~A*B + A*C + B*D + C*D + (1|X), data=data)
m97<-lmer(R~A*B + A*D + B*C + B*D + (1|X), data=data)
m98<-lmer(R~A*B + A*D + B*C + C*D + (1|X), data=data)
m99<-lmer(R~A*B + A*D + B*D + C*D + (1|X), data=data)
m100<-lmer(R~A*B + B*C + B*D + C*D + (1|X), data=data)
m101<-lmer(R~A*C + A*D + B*C + B*D + (1|X), data=data)
m102<-lmer(R~A*C + A*D + B*C + C*D + (1|X), data=data)
m103<-lmer(R~A*C + A*D + B*D + C*D + (1|X), data=data)
m104<-lmer(R~A*C + B*C + B*D + C*D + (1|X), data=data)
m105<-lmer(R~A*D + B*C + B*D + C*D + (1|X), data=data)


m106<-lmer(R~A*B + A*C + A*D + B*C + B*D + (1|X), data=data)
m107<-lmer(R~A*B + A*C + A*D + B*C + C*D + (1|X), data=data)
m108<-lmer(R~A*B + A*C + A*D + B*D + C*D + (1|X), data=data)
m109<-lmer(R~A*B + A*C + B*C + B*D + C*D + (1|X), data=data)
m110<-lmer(R~A*B + A*D + B*C + B*D + C*D + (1|X), data=data)
m111<-lmer(R~A*C + A*D + B*C + B*D + C*D + (1|X), data=data)

m112<-lmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X), data=data)

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
