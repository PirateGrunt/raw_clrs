## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE, 
  fig.pos="t"
)

## ------------------------------------------------------------------------
dfUpper <- read.csv("./data/upper.csv", stringsAsFactors = FALSE)

plot(dfUpper$CumulativePaid, dfUpper$NetEP)

## ------------------------------------------------------------------------
library(raw)
data(RegionExperience)
library(ggplot2)

basePlot <- ggplot(RegionExperience)
class(basePlot)

## ------------------------------------------------------------------------
basePlot <- basePlot + aes(x = PolicyYear, y = NumClaims, color=Region)

## ------------------------------------------------------------------------
p <- basePlot + geom_line()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_point()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_point() + geom_line()
p

## ------------------------------------------------------------------------
p <- ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims, group=Region, color=Region)) + geom_line()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_bar(stat="identity", aes(fill = Region))
p

## ------------------------------------------------------------------------
p <- basePlot + geom_bar(stat="identity", position="dodge", aes(fill=Region))
p

## ------------------------------------------------------------------------
data(StateExperience)
p <- ggplot(StateExperience, aes(x = PolicyYear, y = NumClaims, color = State)) + geom_point() + facet_wrap(~ Region)
p <- p + theme(legend.position = "none")
p

## ------------------------------------------------------------------------
p <- ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims, group=Region, color=Region)) + geom_point()
p + geom_smooth(se = FALSE)

## ------------------------------------------------------------------------
p + geom_smooth(method = lm)

## ------------------------------------------------------------------------
data("wkcomp")

## ----results='hide', messages=FALSE--------------------------------------
suppressMessages(library(dplyr))
data("wkcomp")

dfTwo <- wkcomp %>% 
  raw::CasColNames(FALSE)

set.seed(1234)
dfTwo <- dfTwo %>% 
  filter(Company %in% sample(unique(dfTwo$Company), 2)) %>% 
  mutate(PaidLR = CumulativePaid / NetEP)

## ------------------------------------------------------------------------
plt <- ggplot(dfTwo, aes(NetEP, CumulativePaid, color = factor(Lag))) + geom_point() + facet_wrap(~Company, scales="free")
plt

## ------------------------------------------------------------------------
plt + geom_smooth(method = lm, se=FALSE)

## ------------------------------------------------------------------------
pltDensity <- ggplot(filter(dfTwo, Lag == 10), aes(PaidLR, fill = Company)) + geom_density(alpha = 0.7)
pltDensity

## ------------------------------------------------------------------------
pltDensity + facet_wrap(~ Company)

## ------------------------------------------------------------------------
library(maps)
map('state')

## ------------------------------------------------------------------------
data(Hurricane)
 
dfKatrina = subset(Hurricane, Name == 'KATRINA')
dfKatrina = dfKatrina[dfKatrina$Year == max(dfKatrina$Year), ]

 
dfHugo = subset(Hurricane, Name == 'HUGO')
dfHugo = dfHugo[dfHugo$Year == max(dfHugo$Year), ]
 
dfDonna = Hurricane[Hurricane$Name == 'DONNA', ]
dfDonna = dfDonna[dfDonna$Year == max(dfDonna$Year), ]

## ------------------------------------------------------------------------
map('state')
points(dfKatrina$Longitude, dfKatrina$Latitude, pch=19, col = 'red')
points(dfHugo$Longitude, dfHugo$Latitude, pch = 19, col = 'blue')
points(dfDonna$Longitude, dfDonna$Latitude, pch = 19, col = 'green')

