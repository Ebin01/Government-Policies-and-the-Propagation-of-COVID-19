library (readr)
library(tidyverse)
library(fixest)
library(dplyr)

urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
mydata<-read_csv(url(urlfile))



View(mydata)

#subset the data to include only 2020 dates, first day of every month, National data 
mydataClean <-  subset(mydata, (
    Date %in% c(20200101, 20200201, 20200301, 20200401, 20200501, 20200601, 
                20200701, 20200801, 20200901, 20201001, 20201101, 20201201) &
    Jurisdiction == "NAT_TOTAL") &
      !is.na(StringencyIndex)&
      !is.na(ConfirmedDeaths))

mydataClean = mydataClean %>% select("CountryName", "Date", "ConfirmedDeaths", "StringencyIndex")

View(mydataClean)

head(mydataClean)

govCovid = lm(ConfirmedDeaths ~ StringencyIndex, mydataClean)
coef(govCovid)[2]
#increasing the stringency by 1 unit increases the deaths by 103.5116

ggplot(mydataClean,
       aes(x = StringencyIndex, 
           y = ConfirmedDeaths)) + 
  geom_point() + 
  geom_smooth(method="lm",
              se=FALSE) + 
  theme_bw() +
  labs(x = 'StringencyIndex', 
       y = 'ConfirmedDeaths')  #make graph smooth?

# wierd, as the stringency in response goes up, confirmed deaths goes up


govCovid$ols = feols(log(ConfirmedDeaths) ~ log(StringencyIndex) | CountryName, mydataClean)
etable(govCovid$ols, cluster = "CountryName")




