Impacts from Severe Weather Events
========================================================
## Synopsis
This analysis will look at different weather events types accross the United States. The goal is determine the economic impact of weather events and also the impact on population health. The data used for this anlysis is from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.
## Data Processing
The data was first read in from the original bz2 file. The function exp_factor converted the differnt letters and numbers in the CROPDMGEXP and PROPDMGEXP to actual assumed representations and were multiplied with CROPDMG and PROPDMG to convert them to their original values. Here we assume a number means to multiply the damge by ten to the power of that number. We also asumme "B" stands for billion, "M" statnds for million, "K" stands for Kilo or 1,000, and "H" is 100 or hecto.
A table called populationHealth was made that summed the total injuries, total fatalities and total casualties for each EVTYPE. Another table called economicHealth was made that summed the total property damage, total crop damage and total damage for each EVTYPE. These tables were then arranged in descending order by total damage for economicHealth and casualties for population health.

```r
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, desc, failwith, id, mutate, summarise, summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(stringr)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following object is masked from 'package:dplyr':
## 
##     last
```

```r
library(ggplot2)
stormData<-bzfile("repdata-data-StormData.csv.bz2", open = "r")
stormData<-read.csv(stormData)
exp_factor <- function(x){suppressWarnings(
                       ifelse(x %in% as.character(0:8), 10^as.numeric(x),
                       ifelse(x %in% c("b","B"), 10^9,    # billion
                       ifelse(x %in% c("m","M"), 10^6,    # million/mega
                       ifelse(x %in% c("k","K"), 10^3,    # kilo   
                       ifelse(x %in% c("h","H"), 10^2,    # hecto
                       1))))))
                      }
stormData$PROPDMG<-exp_factor(stormData$PROPDMGEXP)*stormData$PROPDMG
stormData$CROPDMG<-exp_factor(stormData$CROPDMGEXP)*stormData$CROPDMG
populationHealth<-ddply(stormData,.(EVTYPE),summarise,TOTALINJURIES=sum(INJURIES),TOTALFATALITIES=sum(FATALITIES),CASUALTIES=sum(FATALITIES)+sum(INJURIES))
populationHealth<-arrange(populationHealth, desc(CASUALTIES))
economicHealth<-ddply(stormData,.(EVTYPE),summarise,TOTALPROPDMG=sum(PROPDMG),TOTALCROPDMG=sum(CROPDMG),TOTAL=sum(CROPDMG)+sum(PROPDMG))
economicHealth<-arrange(economicHealth, desc(TOTAL))
fiveEconomicHealth<-head(economicHealth,5)
fiveEconomicHealth[grep("THUNDERSTORM WINDS",fiveEconomicHealth$EVTYPE),]$EVTYPE<-"THUNDERSTORM WIND"
fivePopulationHealth<-head(populationHealth,5)
fivePopulationHealth[grep("TSTM WIND",fivePopulationHealth$EVTYPE),]$EVTYPE<-"THUNDERSTORM WIND"
```


## Results
### Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

Here are the top five most harmul to the population. Tornados seem to do the most damage to the population. 

```r
fivePopulationHealth
```

```
##              EVTYPE TOTALINJURIES TOTALFATALITIES CASUALTIES
## 1           TORNADO         91346            5633      96979
## 2    EXCESSIVE HEAT          6525            1903       8428
## 3 THUNDERSTORM WIND          6957             504       7461
## 4             FLOOD          6789             470       7259
## 5         LIGHTNING          5230             816       6046
```

```r
qplot(EVTYPE, CASUALTIES, data = fivePopulationHealth, geom = "bar", fill = factor(EVTYPE), 
    xlab = "Event", ylab = "Casualties")
```

```
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Across the United States, which types of events have the greatest economic consequences?
Here are the top five most harmul in terms of economic damage. Flash floods seem to do the most damage to the economy.

```r
fiveEconomicHealth
```

```
##              EVTYPE TOTALPROPDMG TOTALCROPDMG     TOTAL
## 1       FLASH FLOOD    6.820e+13    1.421e+09 6.820e+13
## 2 THUNDERSTORM WIND    2.087e+13    1.907e+08 2.087e+13
## 3           TORNADO    1.079e+12    4.151e+08 1.079e+12
## 4              HAIL    3.158e+11    3.026e+09 3.188e+11
## 5         LIGHTNING    1.729e+11    1.209e+07 1.730e+11
```

```r
qplot(EVTYPE, TOTAL, data = fiveEconomicHealth, geom = "bar", fill = factor(EVTYPE), 
    xlab = "Event", ylab = "Total Damage(US Dollars)")
```

```
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 






