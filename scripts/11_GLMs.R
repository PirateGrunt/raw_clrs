library("insuranceData")
data(dataCar)

dim(dataCar)
colnames(dataCar)

head(dataCar)

str(dataCar)

summary(dataCar)

table(dataCar$numclaims)
hist(dataCar$numclaims)

avg <- function(x) {
  dat <- aggregate(dataCar$numclaims, by = list(dataCar[, x]), FUN = mean)
  barplot(dat$x, xlab = x, ylab = "Claim Count Averages")
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

par(mfrow=c(2,2))
avg(x = "veh_age")
avg(x = "area")
avg(x = "agecat")
avg(x = "veh_body")

summaries<-function(x) {
  means<-aggregate(dataCar$numclaims, by = list(dataCar[, x]), FUN = mean)
  lengths<-aggregate(dataCar$numclaims, by = list(dataCar[, x]), FUN = length)
  means_lengths<-merge(means,lengths,by="Group.1"  )
  colnames(means_lengths)<-c(x,'numclaims average','count')
  means_lengths[order(means_lengths[,"numclaims average"]),]
}
summaries("veh_body")

library("plyr")
dataCar$veh_body_cat<-mapvalues(dataCar$veh_body
                                , from = c("COUPE", "MCARA", "MIBUS", "PANVN", "RDSTR")
                                , to = c("Others","Others","Others","Others","Others"))
summaries("veh_body_cat")

par(mfrow=c(2,2))
avg(x = "veh_body")
avg(x = "veh_body_cat")

summaries("veh_age")

dataCar$veh_age_cat<-factor(ifelse(dataCar$veh_age =='2','2','Others'))
aggregate(dataCar$numclaims, by = list(dataCar[, "veh_age_cat"]), FUN = mean)
summaries("veh_age_cat")

par(mfrow=c(2,2))
avg(x = "veh_age")
avg(x = "veh_age_cat")

summaries("area")

dataCar$area_cat<-mapvalues(dataCar$area
                            , from = c("A","B","C","E")
                            , to = c("Others","Others","Others","Others"))
aggregate(dataCar$numclaims, by = list(dataCar[, "area_cat"]), FUN = mean)
summaries("area_cat")

par(mfrow=c(2,2))
avg(x = "area")
avg(x = "area_cat")

summaries("agecat")

dataCar$agecat_cat<-factor(ifelse(dataCar$agecat =='1','1'
                                  ,ifelse(dataCar$agecat %in% c('5','6'),'5-6','Others')))
aggregate(dataCar$numclaims, by = list(dataCar[, "agecat_cat"]), FUN = mean)
summaries("agecat_cat")

par(mfrow=c(2,2))
avg(x = "agecat")
avg(x = "agecat_cat")

formulas<-"numclaims ~ veh_body_cat"
poisson_reg1 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg1)
dataCar <- within(dataCar, veh_body_cat <- relevel(veh_body_cat, ref = 'Others'))
poisson_reg1 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg1)

formulas<-"numclaims ~ veh_age_cat"
dataCar <- within(dataCar, veh_age_cat <- relevel(veh_age_cat, ref = 'Others'))
poisson_reg2 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg2)

formulas<-"numclaims ~ area_cat"
dataCar <- within(dataCar, area_cat <- relevel(area_cat, ref = 'Others'))
poisson_reg3 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg3)

formulas<-"numclaims ~ agecat_cat"
dataCar <- within(dataCar, agecat_cat <- relevel(agecat_cat, ref = 'Others'))
poisson_reg4 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg4)

formulas<-"numclaims ~ veh_body_cat+veh_age_cat+area_cat+agecat_cat"
dataCar <- within(dataCar, agecat_cat <- relevel(agecat_cat, ref = 'Others'))
poisson_reg5 <- glm(formulas, data =dataCar, family=poisson)
summary(poisson_reg5)

dataCar$numclaims_bin <- ifelse(dataCar$numclaims == 0, 0, 1)
table(dataCar$numclaims_bin)

formulas<-"numclaims_bin ~ veh_body_cat"
dataCar <- within(dataCar, veh_body_cat <- relevel(veh_body_cat, ref = 'Others'))
logistic_reg1 <- glm(formulas, data =dataCar, family=binomial)
summary(logistic_reg1)

formulas<-"numclaims_bin ~ veh_age_cat"
dataCar <- within(dataCar, veh_age_cat <- relevel(veh_age_cat, ref = 'Others'))
logistic_reg2 <- glm(formulas, data =dataCar, family=binomial)
summary(logistic_reg2)

formulas<-"numclaims_bin ~ area_cat"
dataCar <- within(dataCar, area_cat <- relevel(area_cat, ref = 'Others'))
logistic_reg3 <- glm(formulas, data =dataCar, family=binomial)
summary(logistic_reg3)

formulas<-"numclaims_bin ~ agecat_cat"
dataCar <- within(dataCar, agecat_cat <- relevel(agecat_cat, ref = 'Others'))
logistic_reg4 <- glm(formulas, data =dataCar, family=binomial)
summary(logistic_reg4)

formulas<-"numclaims_bin ~ veh_body_cat+veh_age_cat+area_cat+agecat_cat"
dataCar <- within(dataCar, agecat_cat <- relevel(agecat_cat, ref = 'Others'))
logistic_reg5 <- glm(formulas, data =dataCar, family=binomial)
summary(logistic_reg5)

anova(poisson_reg1,poisson_reg5, test="Chisq")
anova(poisson_reg2,poisson_reg5, test="Chisq")
anova(poisson_reg3,poisson_reg5, test="Chisq")
anova(poisson_reg4,poisson_reg5, test="Chisq")

AICs<-c(poisson_reg1$aic,poisson_reg2$aic,poisson_reg3$aic,poisson_reg4$aic)
AICs

anova(logistic_reg1,logistic_reg5, test="Chisq")
anova(logistic_reg2,logistic_reg5, test="Chisq")
anova(logistic_reg3,logistic_reg5, test="Chisq")
anova(logistic_reg4,logistic_reg5, test="Chisq")

AICs<-c(logistic_reg1$aic,logistic_reg2$aic,logistic_reg3$aic,logistic_reg4$aic)
AICs

MSEs<-c(mean(poisson_reg4$residuals^2),mean(poisson_reg5$residuals^2)
        ,mean(logistic_reg4$residuals^2),mean(logistic_reg5$residuals^2))
MSEs
