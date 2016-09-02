## ------------------------------------------------------------------------
library(methods)

## ---- echo=TRUE, message=FALSE, warning=FALSE, results='asis'------------
library(ChainLadder)
citation('ChainLadder')$textVersion

## ------------------------------------------------------------------------
head(GenInsLong)

## ------------------------------------------------------------------------
class(GenInsLong)
gen_ins_tri <- as.triangle(Triangle = GenInsLong, origin = "accyear", dev = "devyear", 
  value = "incurred claims")
class(gen_ins_tri)


## ------------------------------------------------------------------------
str(gen_ins_tri)

## ------------------------------------------------------------------------
gen_ins_tri[,1:8]

## ---- message=FALSE, warning=FALSE---------------------------------------
library(ChainLadder)
library(raw)
library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)

class(NJM_WC)


## ------------------------------------------------------------------------
njm_wc_tri <- NJM_WC %>% 
  dplyr::filter(DevelopmentYear <= 1997) 
class(njm_wc_tri)


## ------------------------------------------------------------------------
njm_wc_tri <- as.triangle(Triangle = njm_wc_tri, origin = 'AccidentYear', 
  dev = 'Lag', value =  'CumulativeIncurred'); njm_wc_tri[,1:8]


## ------------------------------------------------------------------------
class(njm_wc_tri)
dim(njm_wc_tri)
njm_wc_df <- njm_wc_tri[,2:10] / njm_wc_tri[,1:9] %>% 
  as_tibble()
njm_wc_df <- dplyr::mutate(njm_wc_df, accident_year = as.character(1988:1997))
names(njm_wc_df)[1:9] <- paste(1:9, "to", 2:10)

## ------------------------------------------------------------------------
njm_wc_df <- gather(njm_wc_df, key = interval, value = development_factor, 1:9, 
  na.rm = TRUE)

head(njm_wc_df)


## ------------------------------------------------------------------------
ggplot(data = njm_wc_df, mapping = aes(x = interval, y = development_factor, 
  color = accident_year)) + geom_point()


## ------------------------------------------------------------------------
liab_tri <- as.triangle(Triangle = 
    othliab[othliab$GRNAME == 'State Farm Mut Grp' & 
        othliab$DevelopmentYear <= 1997, ],
  origin = 'AccidentYear', dev = 'DevelopmentLag', value = 'CumPaidLoss_h1')

## ------------------------------------------------------------------------
clark <- ClarkLDF(Triangle = liab_tri, maxage = 20, adol = FALSE)
clark$Total$FutureValue
clark$Ldf[2]
clark$TruncatedLdf[2]

## ------------------------------------------------------------------------
unname(1 + (clark$THETAG[2]/15)^clark$THETAG[1])
unname(1 + (clark$THETAG[2]/15)^clark$THETAG[1] / 
    1 + (clark$THETAG[2]/20)^clark$THETAG[1])

## ------------------------------------------------------------------------
init_df <- MackChainLadder(Triangle = liab_tri)$f
sel_df <- init_df
sel_df[3] <- 1.5
alphas <- CLFMdelta(Triangle = liab_tri, selected = sel_df[1:9])
full_tri <- predict(chainladder(liab_tri, delta = alphas))

## ------------------------------------------------------------------------
full_tri[7:10,1:5]
full_tri[,4] / full_tri[,3]

## ------------------------------------------------------------------------
# ?qpaid #not run
dim(qpaid)
## MackChainLadder expects a quadratic matrix so let's expand 
## the triangle to a quarterly origin period.
n <- ncol(qpaid) # Number of quarterly valuations
Paid <- matrix(NA, n, n) # create a matrix with NAs
Paid[seq(from = 1, to = n, by = 4),] <- qpaid # fill in every 4th row with the annual data

