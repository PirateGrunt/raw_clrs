## ----echo = FALSE--------------------------------------------------------
knitr::opts_knit$set(root.dir = "../")
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  fig.pos="t"
)

## ----echo=FALSE, results='hide'------------------------------------------
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
library(dplyr)

dfUpper <- read.csv("./data/upper.csv", stringsAsFactors = FALSE) %>% 
  arrange(AccidentYear, Lag) %>% 
  mutate(IncrementalPaid = CumulativePaid - lag(CumulativePaid)
         , IncrementalPaid = ifelse(Lag == 1, CumulativePaid, IncrementalPaid)
         , PriorCumulativePaid = lag(CumulativePaid)
         , PriorCumulativePaid = ifelse(Lag == 1, NA, PriorCumulativePaid))

## ------------------------------------------------------------------------
dfCheck <- dfUpper %>% 
  group_by(AccidentYear) %>% 
  mutate(Check = cumsum(IncrementalPaid)) %>% 
  filter(Check != CumulativePaid)

## ------------------------------------------------------------------------
dfUpper <- dfUpper %>% 
  mutate(LagGroup = factor(Lag))

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))

## ------------------------------------------------------------------------
library(ggplot2)

plt <- ggplot(dfUpper, aes(NetEP, IncrementalPaid, color = LagGroup)) + geom_point()
plt <- plt + geom_smooth(method = "lm", se = FALSE)
plt

## ------------------------------------------------------------------------

plt <- ggplot(dfUpper, aes(PriorCumulativePaid, IncrementalPaid, color = LagGroup)) + geom_point()
plt <- plt + geom_smooth(method = "lm", se = FALSE)
plt

## ------------------------------------------------------------------------
fitPaidAM <- lm(data = dfUpper
                , IncrementalPaid ~ 0 + NetEP:LagGroup)

## ------------------------------------------------------------------------
summary(fitPaidAM)

## ------------------------------------------------------------------------
fitPaidCL <- lm(data = dfUpper
                , IncrementalPaid ~ 0 + PriorCumulativePaid:LagGroup)

## ------------------------------------------------------------------------
summary(fitPaidCL)

## ------------------------------------------------------------------------
dfUpper <- dfUpper %>% 
  mutate(PredictedPaidAM = predict(fitPaidAM)
         , ResidualPaidAM = residuals(fitPaidAM)
         , R_StandardPaidAM = rstandard(fitPaidAM))

## ------------------------------------------------------------------------
pltResid <- ggplot(dfUpper, aes(PredictedPaidAM, R_StandardPaidAM)) + geom_point()
pltResid

pltResid + geom_smooth(method = "lm")

## ------------------------------------------------------------------------
summary(fitPaidAM)$fstatistic
summary(fitPaidCL)$fstatistic

## ------------------------------------------------------------------------
library(raw)
data(NJM_WC)

## ------------------------------------------------------------------------
NJM_WC <- NJM_WC %>% 
  arrange(AccidentYear, Lag) %>% 
  mutate(IncrementalPaid = CumulativePaid - lag(CumulativePaid), 
         IncrementalPaid = ifelse(Lag == 1, CumulativePaid, IncrementalPaid),
         PriorCumulativePaid = dplyr::lag(CumulativePaid), 
         PriorCumulativePaid = ifelse(Lag == 1, NA, PriorCumulativePaid),
         LagGroup = factor(Lag)) 

## ------------------------------------------------------------------------
NJM_WC$PredictAM <- predict(fitPaidAM, newdata = NJM_WC)
NJM_WC$PredictCL <- NA
LagGt1 <- NJM_WC$Lag > 1
NJM_WC$PredictCL[LagGt1] <- predict(fitPaidCL, newdata = NJM_WC[LagGt1, ])

## ------------------------------------------------------------------------
NJM_WC <- NJM_WC %>% 
  mutate(AM_Error = PredictAM - IncrementalPaid, 
         CL_Error = PredictCL - IncrementalPaid, 
         Upper = DevelopmentYear <= 1997)

sqrt(sum(NJM_WC$AM_Error[!NJM_WC$Upper]^2))
sqrt(sum(NJM_WC$CL_Error[!NJM_WC$Upper]^2))

## ----eval=FALSE----------------------------------------------------------
## devtools::install_github("PirateGrunt/MRMR")

