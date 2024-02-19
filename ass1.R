require(sasLM)
require(nlme)
setwd("C:/Users/Leon/Desktop/PHST")
BEdata = read.csv("./DTA01.csv", as.is=TRUE)
colnames(BEdata)[1]="SUBJ"  
BEdata = af(BEdata, c("SUBJ", "SEQ", "PERIOD", "DRUG", "REPL"))


min(BEdata[BEdata$DRUG==1,"CMAX"])
max(BEdata[BEdata$DRUG==1,"CMAX"])
mean(BEdata[BEdata$DRUG==1,"CMAX"])
sd(BEdata[BEdata$DRUG==1,"CMAX"])
sd(BEdata[BEdata$DRUG==1,"CMAX"]) / mean(BEdata[BEdata$DRUG==1,"CMAX"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==1,"CMAX"])))

min(BEdata[BEdata$DRUG==1,"AUCT"])
max(BEdata[BEdata$DRUG==1,"AUCT"])
mean(BEdata[BEdata$DRUG==1,"AUCT"])
sd(BEdata[BEdata$DRUG==1,"AUCT"])
sd(BEdata[BEdata$DRUG==1,"AUCT"]) / mean(BEdata[BEdata$DRUG==1,"AUCT"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==1,"AUCT"])))

min(BEdata[BEdata$DRUG==1,"AUCINF"])
max(BEdata[BEdata$DRUG==1,"AUCINF"])
mean(BEdata[BEdata$DRUG==1,"AUCINF"])
sd(BEdata[BEdata$DRUG==1,"AUCINF"])
sd(BEdata[BEdata$DRUG==1,"AUCINF"]) / mean(BEdata[BEdata$DRUG==1,"AUCINF"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==1,"AUCINF"])))

min(BEdata[BEdata$DRUG==2,"CMAX"])
max(BEdata[BEdata$DRUG==2,"CMAX"])
mean(BEdata[BEdata$DRUG==2,"CMAX"])
sd(BEdata[BEdata$DRUG==2,"CMAX"])
sd(BEdata[BEdata$DRUG==2,"CMAX"]) / mean(BEdata[BEdata$DRUG==2,"CMAX"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==2,"CMAX"])))

min(BEdata[BEdata$DRUG==2,"AUCT"])
max(BEdata[BEdata$DRUG==2,"AUCT"])
mean(BEdata[BEdata$DRUG==2,"AUCT"])
sd(BEdata[BEdata$DRUG==2,"AUCT"])
sd(BEdata[BEdata$DRUG==2,"AUCT"]) / mean(BEdata[BEdata$DRUG==2,"AUCT"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==2,"AUCT"])))

min(BEdata[BEdata$DRUG==2,"AUCINF"])
max(BEdata[BEdata$DRUG==2,"AUCINF"])
mean(BEdata[BEdata$DRUG==2,"AUCINF"])
sd(BEdata[BEdata$DRUG==2,"AUCINF"])
sd(BEdata[BEdata$DRUG==2,"AUCINF"]) / mean(BEdata[BEdata$DRUG==2,"AUCINF"]) * 100
exp(mean(log(BEdata[BEdata$DRUG==2,"AUCINF"])))


Result1 = lme(log(CMAX) ~ DRUG + PERIOD + SEQ, random=~1|SUBJ,
             data=BEdata)
summary(Result1)
anova(Result1)
VarCorr(Result1)
ci = intervals(Result1, 0.90) # 90% CI of log scale difference
exp(ci$fixed["DRUG2",])

Result2 = lme(log(AUCT) ~ DRUG + PERIOD + SEQ , random=~1|SUBJ,
              data=BEdata)
summary(Result2)
anova(Result2)
VarCorr(Result2)
ci = intervals(Result2, 0.90) # 90% CI of log scale difference
exp(ci$fixed["DRUG2",])


Result3 = lme(log(AUCINF) ~ DRUG + PERIOD + SEQ , random=~1|SUBJ,
              data=BEdata)
summary(Result3)
anova(Result3)
VarCorr(Result3)
ci = intervals(Result3, 0.90) # 90% CI of log scale difference
exp(ci$fixed["DRUG2",])

