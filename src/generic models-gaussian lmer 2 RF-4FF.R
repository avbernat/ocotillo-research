library(lme4)
#generic 4-fixed factor and 2 random-factor models

## First Random Factor 

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

## Second Random Factor 

m113<-lmer(R~1 + (1|Y), data=data) 
m114<-lmer(R~A + (1|Y), data=data)
m115<-lmer(R~B + (1|Y), data=data)
m116<-lmer(R~C + (1|Y), data=data)
m117<-lmer(R~D + (1|Y), data=data)

m118<-lmer(R~A+B + (1|Y), data=data)
m119<-lmer(R~A+C + (1|Y), data=data)
m120<-lmer(R~A+D + (1|Y), data=data)
m121<-lmer(R~B+C + (1|Y), data=data)
m122<-lmer(R~B+D + (1|Y), data=data)
m123<-lmer(R~C+D + (1|Y), data=data)
m124<-lmer(R~A+B+C + (1|Y), data=data)
m125<-lmer(R~A+B+D + (1|Y), data=data)
m126<-lmer(R~A+C+D + (1|Y), data=data)
m127<-lmer(R~B+C+D + (1|Y), data=data)
m128<-lmer(R~A+B+C+D + (1|Y), data=data)

m129<-lmer(R~A*B + (1|Y), data=data)
m130<-lmer(R~A*C + (1|Y), data=data)
m131<-lmer(R~A*D + (1|Y), data=data)
m132<-lmer(R~B*C + (1|Y), data=data)
m133<-lmer(R~B*D + (1|Y), data=data)
m134<-lmer(R~C*D + (1|Y), data=data)

m135<-lmer(R~A*B + C + (1|Y), data=data)
m136<-lmer(R~A*B + D + (1|Y), data=data)
m137<-lmer(R~A*C + B + (1|Y), data=data)
m138<-lmer(R~A*C + D + (1|Y), data=data)
m139<-lmer(R~A*D + B + (1|Y), data=data)
m140<-lmer(R~A*D + C + (1|Y), data=data)
m141<-lmer(R~B*C + A + (1|Y), data=data)
m142<-lmer(R~B*C + D + (1|Y), data=data)
m143<-lmer(R~B*D + A + (1|Y), data=data)
m144<-lmer(R~B*D + C + (1|Y), data=data)
m145<-lmer(R~C*D + A + (1|Y), data=data)
m146<-lmer(R~C*D + B + (1|Y), data=data)

m147<-lmer(R~A*B + C + D + (1|Y), data=data)
m148<-lmer(R~A*C + B + D + (1|Y), data=data)
m149<-lmer(R~A*D + B + C + (1|Y), data=data)
m150<-lmer(R~B*C + A + D + (1|Y), data=data)
m151<-lmer(R~B*D + A + C + (1|Y), data=data)
m152<-lmer(R~C*D + A + B + (1|Y), data=data)


m153<-lmer(R~A*B + A*C + (1|Y), data=data)
m154<-lmer(R~A*B + A*D + (1|Y), data=data)
m155<-lmer(R~A*B + B*C + (1|Y), data=data)
m156<-lmer(R~A*B + B*D + (1|Y), data=data)
m157<-lmer(R~A*B + C*D + (1|Y), data=data)
m158<-lmer(R~A*C + A*D + (1|Y), data=data)
m159<-lmer(R~A*C + B*C + (1|Y), data=data)
m160<-lmer(R~A*C + B*D + (1|Y), data=data)
m161<-lmer(R~A*C + C*D + (1|Y), data=data)
m162<-lmer(R~A*D + B*C + (1|Y), data=data)
m163<-lmer(R~A*D + B*D + (1|Y), data=data)
m164<-lmer(R~A*D + C*D + (1|Y), data=data)
m165<-lmer(R~B*C + B*D + (1|Y), data=data)
m166<-lmer(R~B*C + C*D + (1|Y), data=data)
m167<-lmer(R~B*D + C*D + (1|Y), data=data)

m168<-lmer(R~A*B + A*C + D + (1|Y), data=data)
m169<-lmer(R~A*B + A*D + C + (1|Y), data=data)
m170<-lmer(R~A*B + B*C + D + (1|Y), data=data)
m171<-lmer(R~A*B + B*D + C + (1|Y), data=data)
m172<-lmer(R~A*C + A*D + B + (1|Y), data=data)
m173<-lmer(R~A*C + B*C + D + (1|Y), data=data)
m174<-lmer(R~A*C + C*D + B + (1|Y), data=data)
m175<-lmer(R~A*D + B*D + C + (1|Y), data=data)
m176<-lmer(R~A*D + C*D + B + (1|Y), data=data)
m177<-lmer(R~B*C + B*D + A + (1|Y), data=data)
m178<-lmer(R~B*C + C*D + A + (1|Y), data=data)
m179<-lmer(R~B*D + C*D + A + (1|Y), data=data)

m180<-lmer(R~A*B + A*C + A*D + (1|Y), data=data)
m181<-lmer(R~A*B + A*C + B*C + (1|Y), data=data)
m182<-lmer(R~A*B + A*C + B*D + (1|Y), data=data)
m183<-lmer(R~A*B + A*C + C*D + (1|Y), data=data)
m184<-lmer(R~A*B + A*D + B*C + (1|Y), data=data)
m185<-lmer(R~A*B + A*D + B*D + (1|Y), data=data)
m186<-lmer(R~A*B + A*D + C*D + (1|Y), data=data)
m187<-lmer(R~A*B + B*C + B*D + (1|Y), data=data)
m188<-lmer(R~A*B + B*C + C*D + (1|Y), data=data)
m189<-lmer(R~A*B + B*D + C*D + (1|Y), data=data)
m190<-lmer(R~A*C + A*D + B*C + (1|Y), data=data)
m191<-lmer(R~A*C + A*D + B*D + (1|Y), data=data)
m192<-lmer(R~A*C + A*D + C*D + (1|Y), data=data)
m193<-lmer(R~A*C + B*C + B*D + (1|Y), data=data)
m194<-lmer(R~A*C + B*C + C*D + (1|Y), data=data)
m195<-lmer(R~A*C + B*D + C*D + (1|Y), data=data)
m196<-lmer(R~A*D + B*C + B*D + (1|Y), data=data)
m197<-lmer(R~A*D + B*C + C*D + (1|Y), data=data)
m198<-lmer(R~A*D + B*D + C*D + (1|Y), data=data)
m199<-lmer(R~B*C + B*D + C*D + (1|Y), data=data)


m200<-lmer(R~A*B + A*C + B*C + D + (1|Y), data=data)
m201<-lmer(R~A*B + A*D + B*D + C + (1|Y), data=data)
m202<-lmer(R~A*C + A*D + C*D + B + (1|Y), data=data)
m203<-lmer(R~B*C + B*D + C*D + A + (1|Y), data=data)


m204<-lmer(R~A*B + A*C + A*D + B*C + (1|Y), data=data)
m205<-lmer(R~A*B + A*C + A*D + B*D + (1|Y), data=data)
m206<-lmer(R~A*B + A*C + A*D + C*D + (1|Y), data=data)
m207<-lmer(R~A*B + A*C + B*C + B*D + (1|Y), data=data)
m208<-lmer(R~A*B + A*C + B*C + C*D + (1|Y), data=data)
m209<-lmer(R~A*B + A*C + B*D + C*D + (1|Y), data=data)
m210<-lmer(R~A*B + A*D + B*C + B*D + (1|Y), data=data)
m211<-lmer(R~A*B + A*D + B*C + C*D + (1|Y), data=data)
m212<-lmer(R~A*B + A*D + B*D + C*D + (1|Y), data=data)
m213<-lmer(R~A*B + B*C + B*D + C*D + (1|Y), data=data)
m214<-lmer(R~A*C + A*D + B*C + B*D + (1|Y), data=data)
m215<-lmer(R~A*C + A*D + B*C + C*D + (1|Y), data=data)
m216<-lmer(R~A*C + A*D + B*D + C*D + (1|Y), data=data)
m217<-lmer(R~A*C + B*C + B*D + C*D + (1|Y), data=data)
m218<-lmer(R~A*D + B*C + B*D + C*D + (1|Y), data=data)


m219<-lmer(R~A*B + A*C + A*D + B*C + B*D + (1|Y), data=data)
m220<-lmer(R~A*B + A*C + A*D + B*C + C*D + (1|Y), data=data)
m221<-lmer(R~A*B + A*C + A*D + B*D + C*D + (1|Y), data=data)
m222<-lmer(R~A*B + A*C + B*C + B*D + C*D + (1|Y), data=data)
m223<-lmer(R~A*B + A*D + B*C + B*D + C*D + (1|Y), data=data)
m224<-lmer(R~A*C + A*D + B*C + B*D + C*D + (1|Y), data=data)

m225<-lmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|Y), data=data)

## Both Random Factors 

m226<-lmer(R~1 + (1|X) + (1|Y), data=data) #this one will be at the end to keep index number and model number the same.
m227<-lmer(R~A + (1|X) + (1|Y), data=data)
m228<-lmer(R~B + (1|X) + (1|Y), data=data)
m229<-lmer(R~C + (1|X) + (1|Y), data=data)
m230<-lmer(R~D + (1|X) + (1|Y), data=data)

m231<-lmer(R~A+B + (1|X) + (1|Y), data=data)
m232<-lmer(R~A+C + (1|X) + (1|Y), data=data)
m233<-lmer(R~A+D + (1|X) + (1|Y), data=data)
m234<-lmer(R~B+C + (1|X) + (1|Y), data=data)
m235<-lmer(R~B+D + (1|X) + (1|Y), data=data)
m236<-lmer(R~C+D + (1|X) + (1|Y), data=data)
m237<-lmer(R~A+B+C + (1|X) + (1|Y), data=data)
m238<-lmer(R~A+B+D + (1|X) + (1|Y), data=data)
m239<-lmer(R~A+C+D + (1|X) + (1|Y), data=data)
m240<-lmer(R~B+C+D + (1|X) + (1|Y), data=data)
m241<-lmer(R~A+B+C+D + (1|X) + (1|Y), data=data)

m242<-lmer(R~A*B + (1|X) + (1|Y), data=data)
m243<-lmer(R~A*C + (1|X) + (1|Y), data=data)
m244<-lmer(R~A*D + (1|X) + (1|Y), data=data)
m245<-lmer(R~B*C + (1|X) + (1|Y), data=data)
m246<-lmer(R~B*D + (1|X) + (1|Y), data=data)
m247<-lmer(R~C*D + (1|X) + (1|Y), data=data)

m248<-lmer(R~A*B + C + (1|X) + (1|Y), data=data)
m249<-lmer(R~A*B + D + (1|X) + (1|Y), data=data)
m250<-lmer(R~A*C + B + (1|X) + (1|Y), data=data)
m251<-lmer(R~A*C + D + (1|X) + (1|Y), data=data)
m252<-lmer(R~A*D + B + (1|X) + (1|Y), data=data)
m253<-lmer(R~A*D + C + (1|X) + (1|Y), data=data)
m254<-lmer(R~B*C + A + (1|X) + (1|Y), data=data)
m255<-lmer(R~B*C + D + (1|X) + (1|Y), data=data)
m256<-lmer(R~B*D + A + (1|X) + (1|Y), data=data)
m257<-lmer(R~B*D + C + (1|X) + (1|Y), data=data)
m258<-lmer(R~C*D + A + (1|X) + (1|Y), data=data)
m259<-lmer(R~C*D + B + (1|X) + (1|Y), data=data)

m260<-lmer(R~A*B + C + D + (1|X) + (1|Y), data=data)
m261<-lmer(R~A*C + B + D + (1|X) + (1|Y), data=data)
m262<-lmer(R~A*D + B + C + (1|X) + (1|Y), data=data)
m263<-lmer(R~B*C + A + D + (1|X) + (1|Y), data=data)
m264<-lmer(R~B*D + A + C + (1|X) + (1|Y), data=data)
m265<-lmer(R~C*D + A + B + (1|X) + (1|Y), data=data)


m266<-lmer(R~A*B + A*C + (1|X) + (1|Y), data=data)
m267<-lmer(R~A*B + A*D + (1|X) + (1|Y), data=data)
m268<-lmer(R~A*B + B*C + (1|X) + (1|Y), data=data)
m269<-lmer(R~A*B + B*D + (1|X) + (1|Y), data=data)
m270<-lmer(R~A*B + C*D + (1|X) + (1|Y), data=data)
m271<-lmer(R~A*C + A*D + (1|X) + (1|Y), data=data)
m272<-lmer(R~A*C + B*C + (1|X) + (1|Y), data=data)
m273<-lmer(R~A*C + B*D + (1|X) + (1|Y), data=data)
m274<-lmer(R~A*C + C*D + (1|X) + (1|Y), data=data)
m275<-lmer(R~A*D + B*C + (1|X) + (1|Y), data=data)
m276<-lmer(R~A*D + B*D + (1|X) + (1|Y), data=data)
m277<-lmer(R~A*D + C*D + (1|X) + (1|Y), data=data)
m278<-lmer(R~B*C + B*D + (1|X) + (1|Y), data=data)
m279<-lmer(R~B*C + C*D + (1|X) + (1|Y), data=data)
m280<-lmer(R~B*D + C*D + (1|X) + (1|Y), data=data)

m281<-lmer(R~A*B + A*C + D + (1|X) + (1|Y), data=data)
m282<-lmer(R~A*B + A*D + C + (1|X) + (1|Y), data=data)
m283<-lmer(R~A*B + B*C + D + (1|X) + (1|Y), data=data)
m284<-lmer(R~A*B + B*D + C + (1|X) + (1|Y), data=data)
m285<-lmer(R~A*C + A*D + B + (1|X) + (1|Y), data=data)
m286<-lmer(R~A*C + B*C + D + (1|X) + (1|Y), data=data)
m287<-lmer(R~A*C + C*D + B + (1|X) + (1|Y), data=data)
m288<-lmer(R~A*D + B*D + C + (1|X) + (1|Y), data=data)
m289<-lmer(R~A*D + C*D + B + (1|X) + (1|Y), data=data)
m290<-lmer(R~B*C + B*D + A + (1|X) + (1|Y), data=data)
m291<-lmer(R~B*C + C*D + A + (1|X) + (1|Y), data=data)
m292<-lmer(R~B*D + C*D + A + (1|X) + (1|Y), data=data)

m293<-lmer(R~A*B + A*C + A*D + (1|X) + (1|Y), data=data)
m294<-lmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data)
m295<-lmer(R~A*B + A*C + B*D + (1|X) + (1|Y), data=data)
m296<-lmer(R~A*B + A*C + C*D + (1|X) + (1|Y), data=data)
m297<-lmer(R~A*B + A*D + B*C + (1|X) + (1|Y), data=data)
m298<-lmer(R~A*B + A*D + B*D + (1|X) + (1|Y), data=data)
m299<-lmer(R~A*B + A*D + C*D + (1|X) + (1|Y), data=data)
m300<-lmer(R~A*B + B*C + B*D + (1|X) + (1|Y), data=data)
m301<-lmer(R~A*B + B*C + C*D + (1|X) + (1|Y), data=data)
m302<-lmer(R~A*B + B*D + C*D + (1|X) + (1|Y), data=data)
m303<-lmer(R~A*C + A*D + B*C + (1|X) + (1|Y), data=data)
m304<-lmer(R~A*C + A*D + B*D + (1|X) + (1|Y), data=data)
m305<-lmer(R~A*C + A*D + C*D + (1|X) + (1|Y), data=data)
m306<-lmer(R~A*C + B*C + B*D + (1|X) + (1|Y), data=data)
m307<-lmer(R~A*C + B*C + C*D + (1|X) + (1|Y), data=data)
m308<-lmer(R~A*C + B*D + C*D + (1|X) + (1|Y), data=data)
m309<-lmer(R~A*D + B*C + B*D + (1|X) + (1|Y), data=data)
m310<-lmer(R~A*D + B*C + C*D + (1|X) + (1|Y), data=data)
m311<-lmer(R~A*D + B*D + C*D + (1|X) + (1|Y), data=data)
m312<-lmer(R~B*C + B*D + C*D + (1|X) + (1|Y), data=data)


m313<-lmer(R~A*B + A*C + B*C + D + (1|X) + (1|Y), data=data)
m314<-lmer(R~A*B + A*D + B*D + C + (1|X) + (1|Y), data=data)
m315<-lmer(R~A*C + A*D + C*D + B + (1|X) + (1|Y), data=data)
m316<-lmer(R~B*C + B*D + C*D + A + (1|X) + (1|Y), data=data)


m317<-lmer(R~A*B + A*C + A*D + B*C + (1|X) + (1|Y), data=data)
m318<-lmer(R~A*B + A*C + A*D + B*D + (1|X) + (1|Y), data=data)
m319<-lmer(R~A*B + A*C + A*D + C*D + (1|X) + (1|Y), data=data)
m320<-lmer(R~A*B + A*C + B*C + B*D + (1|X) + (1|Y), data=data)
m321<-lmer(R~A*B + A*C + B*C + C*D + (1|X) + (1|Y), data=data)
m322<-lmer(R~A*B + A*C + B*D + C*D + (1|X) + (1|Y), data=data)
m323<-lmer(R~A*B + A*D + B*C + B*D + (1|X) + (1|Y), data=data)
m324<-lmer(R~A*B + A*D + B*C + C*D + (1|X) + (1|Y), data=data)
m325<-lmer(R~A*B + A*D + B*D + C*D + (1|X) + (1|Y), data=data)
m326<-lmer(R~A*B + B*C + B*D + C*D + (1|X) + (1|Y), data=data)
m327<-lmer(R~A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data)
m328<-lmer(R~A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data)
m329<-lmer(R~A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data)
m330<-lmer(R~A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data)
m331<-lmer(R~A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data)


m332<-lmer(R~A*B + A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data)
m333<-lmer(R~A*B + A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data)
m334<-lmer(R~A*B + A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data)
m335<-lmer(R~A*B + A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data)
m336<-lmer(R~A*B + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data)
m337<-lmer(R~A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data)

m338<-lmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data)


#identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, 
             m59, m60, m61, m62, m63, m64, m65, m66, m67, m68, m69, m70, m71, m72, 
             m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m83, m84, m85, m86, 
             m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, m99, m100, 
             m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, 
             m113, m114, m115, m116, m117, m118, m119, m120, m121, m122, m123, m124,
             m125, m126, m127, m128, m129, m130, m131, m132, m133, m134, m135, m136,
             m137, m138, m139, m140, m141, m142, m143, m144, m145, m146, m147, m148, 
             m149, m150, m151, m152, m153, m154, m155, m156, m157, m158, m159, m160,
             m161, m162, m163, m164, m165, m166, m167, m168, m169, m170, m171, m172, 
             m173, m174, m175, m176, m177, m178, m179, m180, m181, m182, m183, m184,
             m185, m186, m187, m188, m189, m190, m191, m192, m193, m194, m195, m196, 
             m197, m198, m199, m200, m201, m202, m203, m204, m205, m206, m207, m208,
             m209, m210, m211, m212, m213, m214, m215, m216, m217, m218, m219, m220,
             m221, m222, m223, m224, m225, m226, m227, m228, m229, m230, m231, m232,
             m233, m234, m235, m236, m237, m238, m239, m240, m241, m242, m243, m244,
             m245, m246, m247, m248, m249, m250, m251, m252, m253, m254, m255, m256, 
             m257, m258, m259, m260, m261, m262, m263, m264, m265, m266, m267, m268,
             m269, m270, m271, m272, m273, m274, m275, m276, m277, m278, m279, m280,
             m281, m282, m283, m284, m285, m286, m287, m288, m289, m290, m291, m292,
             m293, m294, m295, m296, m297, m298, m299, m300, m301, m302, m303, m304,
             m305, m306, m307, m308, m309, m310, m311, m312, m313, m314, m315, m316,
             m317, m318, m319, m320, m321, m322, m323, m324, m325, m326, m327, m328,
             m329, m330, m331, m332, m333, m334, m335, m336, m337, m338, m0) 

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
