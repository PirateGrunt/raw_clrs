## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE, 
  fig.align="center",
  fig.pos="t"
)

## ------------------------------------------------------------------------
library(raw)
data(NJM_WC)

paidLoss <- NJM_WC$CumulativePaid
summary(paidLoss)

ay <- NJM_WC$AccidentYear
lag <- NJM_WC$Lag
dy <- NJM_WC$DevelopmentYear
ibnr <- NJM_WC$IBNR
incurredLoss <- NJM_WC$CumulativeIncurred

## ------------------------------------------------------------------------
sum(ibnr[dy == 1997])

whichCell <- ay == 1990 & lag == 4

incurredLoss[whichCell] - paidLoss[whichCell]

whichCell

## ----results='hide'------------------------------------------------------
summary(NJM_WC)
str(NJM_WC)

## ------------------------------------------------------------------------
var1 <- NJM_WC$CumulativePaid
var2 <- NJM_WC[, "CumulativePaid"]
var3 <- NJM_WC[, 7]

## ------------------------------------------------------------------------
upperTri <- NJM_WC$DevelopmentYear <= 1997
dfUpper <- NJM_WC[upperTri, ]

## ------------------------------------------------------------------------
dfUpper <- subset(NJM_WC, DevelopmentYear <= 1997)

## ----echo=FALSE----------------------------------------------------------
suppressMessages(library(dplyr))

## ------------------------------------------------------------------------
library(dplyr)
dfEx <- NJM_WC %>% 
  group_by(AccidentYear) %>% 
  summarise(MaxPaid = max(CumulativePaid)
            , MinPaid = min(CumulativePaid)
            , AvgPaid = mean(CumulativePaid))

## ----results='asis', echo=FALSE------------------------------------------
pander::pandoc.table(dfEx, big.mark=",")

## ------------------------------------------------------------------------
data("wkcomp")

dfAvgPaid <- wkcomp %>%
  raw::CasColNames(FALSE) %>% 
  filter(Lag == 1) %>% 
  group_by(Company) %>% 
  summarise(AvgPaid = mean(CumulativePaid)) %>% 
  arrange(desc(AvgPaid)) %>% 
  slice(1:5)

## ----results='asis', echo=FALSE------------------------------------------
pander::pandoc.table(dfAvgPaid, big.mark=",")

## ------------------------------------------------------------------------
library(dplyr)

dfUpper <- NJM_WC %>% 
  mutate(Upper = DevelopmentYear <= 1997) %>% 
  filter(Upper)

## ------------------------------------------------------------------------
dfMyData <- NJM_WC %>% 
  select(-GroupCode, -Company)

dfTimeSeries <- NJM_WC %>% 
  mutate(CessionPct = NetEP / DirectEP) %>% 
  unique()

myFit <- lm(CessionPct ~ AccidentYear, data = dfTimeSeries)

## ------------------------------------------------------------------------
write.csv(NJM_WC, "MyData.csv", row.names = FALSE)

## ------------------------------------------------------------------------
dfMyData <- read.csv("MyData.csv", stringsAsFactors = FALSE)

## ----eval=FALSE----------------------------------------------------------
## setwd("This_is_where_my_project_is")

## ----eval=FALSE----------------------------------------------------------
## read.csv("./data/MyData.csv")
## save(importantStuff, file = "./MyImportantStuff/Stuff.rda")

## ------------------------------------------------------------------------
if (!dir.exists("./data/")) dir.create("./data/")

write.csv(dfUpper, "./data/upper.csv", row.names = FALSE)

dfUpper <- read.csv("./data/upper.csv", stringsAsFactors = FALSE)

