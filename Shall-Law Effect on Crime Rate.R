install.packages("foreign")
install.packages("plm")
install.packages("lmtest")

library(foreign)
library(plm)
library(lmtest)
file_data <- read.dta("guns.dta")

P_OLS <- plm(log(vio)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
               density,index=c("stateid","year"), model="pooling",data=file_data)
summary(P_OLS)

FEM <- plm(log(vio)~shall+log(incarc_rate)+pb1064+pw1064+pm1029+pop+avginc+I(avginc*avginc)+
             density,index=c("stateid","year"), model="within",data=file_data)
summary(FEM)
coeftest(FEM, vcovHC)

FEM_T <- plm(log(vio)~factor(year)+shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
               density,index=c("stateid","year"), model="within",data=file_data)
summary(FEM_T)


P_OLS_rob <- plm(log(rob)~shall, data = file_data, model = "pooling", index = c("stateid","year"))
summary(P_OLS_rob)

P_OLS_rob1 <- plm(log(rob)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
               density,index=c("stateid","year"), model="pooling",data=file_data)
summary(P_OLS_rob1)

P_OLS_rob2 <- plm(log(rob)~shall+log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+
                    log(density),index=c("stateid","year"), model="pooling",data=file_data)
summary(P_OLS_rob2)


EFE_rob1 <- plm(log(rob)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
                    density,index=c("stateid","year"), model="within",data=file_data)
summary(EFE_rob1)


EFE_rob2 <- plm(log(rob)~shall+log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+
                    log(density),index=c("stateid","year"), model="within",data=file_data)
summary(EFE_rob2)


ETFE_rob1 <- plm(log(rob)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
                  density+factor(year),index=c("stateid","year"), model="within",data=file_data)
summary(ETFE_rob1)

P_OLS_mur <- plm(log(mur)~shall, data = file_data, model = "pooling", index = c("stateid","year"))
summary(P_OLS_mur)

P_OLS_mur1 <- plm(log(mur)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
                    density,index=c("stateid","year"), model="pooling",data=file_data)
summary(P_OLS_mur1)

EFE_mur1 <- plm(log(mur)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
                  density,index=c("stateid","year"), model="within",data=file_data)
summary(EFE_mur1)

Basemodel <- lm(guns$vio~guns$shall)

ETFE_mur1 <- plm(log(mur)~shall+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+
                   density+factor(year),index=c("stateid","year"), model="within",data=file_data)
summary(ETFE_mur1)
