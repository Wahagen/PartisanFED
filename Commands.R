library(plm)
library(tidyverse) # Modern data science library 
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest) 

FEDD[1:13] <- NULL
FEDF[1:13] <- NULL

FEDD <- FEDFUNDS$DATE
FEDF <- FEDFUNDS$FEDFUNDS

#Regression

pdata1 <- pdata.frame(DF, index = c("t", "id"))
pn <- plm(rate ~ republican, data = pdata1, model = "within", effect = "time")
summary(pn)

pdata2 <- pdata.frame(Df, index = c("id","t5"))
pn2 <- plm(rate ~ republican, data = pdata2, model = "within", effect = "time")
summary(pn2)

pdata2 <- pdata.frame(Df, index = c("t3","t2"))
pn3 <- plm(rate ~ republican, data = pdata2, model = "within")
summary(pn2)

pn4 <- plm(rate ~ HWBush + Clinton + Trump + Obama, data = pdata2, model = "within")
summary(pn4)




fixed.dum <-lm(rate ~ rep + factor(id) -1, data = FEDFUNDS22)
summary(fixed.dum)

reg_ind_year <- plm(rate ~ republican + factor(id), data= pdata1, index=c("t1", "t2"), model="within", effect="individual")


#Plots

coplot(y ~ year|country, type="b", data=dataPanel101) 

plotmeans(rate ~ t1, data = pdata1)


ggplot(pdata1, aes(x = republican, y = rate))+
  geom_point() +
  geom_smooth(method=lm)

#Time-Series
ggplot(data = economics, aes(x = date, y = pop))+
  geom_line(color = "#00AFBB", size = 2)
