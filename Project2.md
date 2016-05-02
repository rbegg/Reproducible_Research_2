# Impact of Severe Weather on US Population Health and Economic Consequence
Robert Begg  
April 28, 2016  
# Synopsis
This report uses the data from the National Oceanic and Atmospheric Administration (NOAA) Storm Database to addresses the impact of severe weather on the US population health and economic consequnence.  Two key questions are answered:

1.Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2.Across the United States, which types of events have the greatest economic consequences?
# Data Processing

##Load libraries


```r
library(utils)
library(R.utils)
library(ggplot2)
library(dplyr)
library(tidyr)
```


## Get source Data

The data was download from [Coursera project](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).  The data is a text file in CSV format, with missing data represented as empty fields.

[Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) about the National Weather Service Storm data and an [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

```r
if(!file.exists("repdata-data-StormData.csv.bz2")){
        download.file(
                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                "repdata-data-StormData.csv.bz2" )    
}
if(!file.exists("repdata-data-StormData.csv")){
        bunzip2("repdata-data-StormData.csv.bz2")    
}
```

## Loading the Data

The CSV data was read into a data frame object.  

```r
StormData <- read.csv("repdata-data-StormData.csv")

dim(StormData)
```

```
## [1] 902297     37
```
There was 902297 observations of 37 variables.
The first 10 rows of the data:

```r
head(StormData)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

## Subsetting the Data

We are only interested in observations with non-zero values for population health and economic variables.


```r
# Subset data for population health impact
HealthData <- StormData %>%
        filter(FATALITIES !=0 | INJURIES !=0) %>%
        select(EVTYPE,FATALITIES,INJURIES)
dim(HealthData)
```

```
## [1] 21929     3
```

```r
# Subset data for Econmic Impact
EconomicData <- StormData %>%
        filter(PROPDMG !=0 | CROPDMG !=0) %>%
        select(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG, CROPDMGEXP)
dim(EconomicData)
```

```
## [1] 245031      5
```


## Converting Damage Values

The original data uses 3 significant digits in the PROPDMG and CROPDMG variables, and use the PROPDMGEXP and CROPDMGEXP variables to define the exponents.  Although the documentation indicates the exponent columns should only contain K (thousands), M (Millions), B (Billions) the levels command below shows the data actually contains a number of values.


```r
levels(EconomicData$PROPDMGEXP) # Unique values used in Property Damage Exponent Variable
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
levels(EconomicData$CRPODMGEXP) # Unique values used in Crop Damage Exponent Variable
```

```
## NULL
```

The following code was used to convert the damage variables to numeric values.  Values are intepreted as follows:

1. 0 - 8 are interpreted as character representation of the exponents
2. Lower and upper case values for K, M, B follow the documentation (1e+03, 1e+06, 1e+09 repsectively)
3. Lower and upper case H is interpreted as Hundred or 1e+02 
4. Empty "", Blank " ", "-","?" and "+" characters are treated as missing, and treated as zero in the analysis.
First we convert the damage estimates into numeric values.


```r
# Create lookup table to convert exponents to consisten exponent format
lookup <- list( Factor = c(""," ","?","+","-","H","h","k","K","m","M","b","B",
                            "0","1","2","3","4","5","6","7","8","9"),
                Value = c(1,1,1,1,1,1e+02,1e+02,1e+03,1e+03,1e+06,1e+06,1e+09,1e+09,
                          0,1e+01,1e+02,1e+03,1e+04,1e+05,1e+06,1e+07,1e+08,1e+9))
print(lookup)        
```

```
## $Factor
##  [1] ""  " " "?" "+" "-" "H" "h" "k" "K" "m" "M" "b" "B" "0" "1" "2" "3"
## [18] "4" "5" "6" "7" "8" "9"
## 
## $Value
##  [1] 1e+00 1e+00 1e+00 1e+00 1e+00 1e+02 1e+02 1e+03 1e+03 1e+06 1e+06
## [12] 1e+09 1e+09 0e+00 1e+01 1e+02 1e+03 1e+04 1e+05 1e+06 1e+07 1e+08
## [23] 1e+09
```

```r
# Function to convert exponents
myConvert <- function(old) {
        new <- array(dim = length(old))
        
        for (i in 1:length(old)) {
                new[i] <- lookup$Value[lookup$Factor == old[i]]
        }
        return(new)
}

# Modify the Factor values by converting to consistent exponent format
levels(EconomicData$PROPDMGEXP) <- myConvert(levels(EconomicData$PROPDMGEXP))
levels(EconomicData$CROPDMGEXP) <- myConvert(levels(EconomicData$CROPDMGEXP))

# Add calculated fields for Property, Crop, and Total costs (in billions $) 
EconomicData <- mutate(EconomicData, 
                       Property = PROPDMG * as.numeric(levels(PROPDMGEXP))[PROPDMGEXP] / 1e+09,
                       Crop = CROPDMG * as.numeric(levels(CROPDMGEXP))[CROPDMGEXP] / 1e+09,
                       Total = Property + Crop )
```


# Results

First we aggregate all fatalities and injuries by EVTYPE. 


```r
# Aggregate number of fatalities and injuries by event type, order result
healthType <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, StormData, sum)
```

Then we remove observations that have zero fatalities or injuries, and order the results.


```r
# Remove observations with no fatalitites, and order
fType <- healthType %>%
        filter( FATALITIES > 0 ) %>%
        select(EVTYPE, FATALITIES) %>%
        arrange(desc(FATALITIES))

# Remove observations with no injuries, and order
iType <- healthType %>%
        filter( INJURIES > 0 ) %>%
        select(EVTYPE, INJURIES) %>%
        arrange(desc(INJURIES))
```


## Which Events are most harmful with respect to population health?

The data has two variables, fatalities and injuries, that indicate harm to population health.  Since it is hard to compare the impact of a fatality over an injury, the two variables are analyzed sperately.


```r
#set number of top events
n <- 35
```



### Fatalities

The following plot shows the Top 35 Weather events (EVTYPE) causing fatalities:

```r
# Set Consistent Theme for Bar Plots
barTheme <- theme(axis.title.x = element_text(face="bold", colour="black", size=12),
                  axis.text.x  = element_text(angle=90, vjust=0.5, size=12)) +
        theme(axis.title.y = element_text(face="bold", colour="black", size=14),
              axis.text.y  = element_text(size=12)) +
        theme(title = element_text(face="bold", colour="black", size=16)) +
        theme( legend.title = element_blank()) +
        theme(legend.position=c(1, 1), legend.justification=c(1,1))

fPlot <- ggplot(fType[1:n,], aes(EVTYPE, FATALITIES)) + 
        geom_bar(stat="identity", fill="firebrick",colour="yellow") +
        scale_x_discrete(limits=(fType[1:n,]$EVTYPE)) +
        ggtitle(paste("Fatalities by by Top", n, "Weather Event Types")) +
        ylab("Number of Fatalities") +
        xlab("Weather Event Type") +
        barTheme

fPlot
```

![](Project2_files/figure-html/Plot_Fatalities-1.png)\


### Injuries

The following plot shows the Top 35 Weather events (EVTYPE) causing inuries:


```r
iPlot <- ggplot(iType[1:n,], aes(EVTYPE, INJURIES)) + 
        geom_bar(stat="identity", fill="navyblue",colour="yellow") +
        scale_x_discrete(limits=(iType[1:n,]$EVTYPE)) +
        ggtitle(paste("Injuries by Top", n, "Weather Event Types")) +
        ylab("Number of Injuries") +
        xlab("Weather Event Type") +
        barTheme
           
iPlot
```

![](Project2_files/figure-html/Plot_Injuries-1.png)\


### Conclusion

Tornados cause both the largest number of deaths and injuries by a wide margin.  Floods, Flash Floods, Heat, Excessive Heat and lighting cause both a large number of deaths and injuries.  Interestingly, there are some events (Rip Current, Avalanche) that cause a high number of deaths but few injuries, perhaps indicating a more lethal event.

## Which types of events have the greatest economic consequences?

The data has two variables, property damage and crop damage , that indicate economic consequence.  In this case, we will combine the two variables in the analyses.


```r
#set number of top events
n <- 35
```

Next we aggregate all damage by EVTYPE. 


```r
# Aggregate number of fatalities and injuries by event type, order result

dmgType <- aggregate(cbind(Property, Crop) ~ EVTYPE, EconomicData, sum) %>%
        gather(dType, Damage, Property, Crop) %>%
        arrange(desc(Damage))

# Order the Weather Events in order of damage, remove duplicates
orderedEcoEvents <- aggregate(cbind(Total) ~ EVTYPE, EconomicData, sum) %>%
        arrange(desc(Total))
        
# Refacttor EVTYPE to order by total damage
dmgType$EVTYPE <- factor(dmgType$EVTYPE, levels = unique(orderedEcoEvents$EVTYPE))
```


### Total Damage

The following plot shows the Top 35 Weather events (EVTYPE) causing combined property and crop damage:

```r
p <- ggplot(dmgType[dmgType$EVTYPE %in% orderedEcoEvents$EVTYPE[1:n],], 
            aes(x=EVTYPE, y=Damage, fill=dType)) +
        ggtitle(paste("Econonmic Impact by Top", n, "Weather Event Types")) +
        ylab("Damage (in billions $)") +
        xlab("Weather Event Type") +
        geom_bar(stat="identity") + barTheme
p
```

![](Project2_files/figure-html/Plot_Total_Damage-1.png)\


### Conclusion

Floods, Hurricane/Typhons, Tornados, Storm Surges, Hail, Flash Floods, Drought and Hurricanes case the largest economic impact to US populations. 
