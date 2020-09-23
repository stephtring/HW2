
### Learning Goals

\-Due Date -September 23, 2020 by midnight Pacific time.

\-For this assignment, we will be analyzing data from USC’s Children’s
Health Study. The learning objectives are to conduct data wrangling and
visualize the data with key questions in mind.

### HW2 Description

\-Data Wrangling: -You will need to download two datasets from
<https://github.com/USCbiostats/data-science-data>. The individual and
regional CHS datasets in 01\_chs. The individual data includes personal
and health characteristics of children in 12 communities across Southern
California. The regional data include air quality measurements at the
community level. Once downloaded, you can merge these datasets using the
location variable. Once combined, you will need to do the following:

###### Part A Data Wrangling

``` {r}
# Read in dataset
library (data.table)
library (dplyr)
library (dtplyr)
library(leaflet)
library(tidyverse)
library(lubridate)

chs_ind <- read.csv2("chs_individual.csv",header = TRUE,sep = ",")
View(chs_ind)
chs_ind <- as.data.table(chs_ind)

chs_reg <- read.csv2("chs_regional.csv",header = TRUE,sep = ",")
View(chs_reg)
chs_reg <- as.data.table(chs_reg)
```

The CHS Individual dataset has 1200 observation and 23 variables and the
CHS regional dataset has 12 observation and 27 variables

``` {r}
# Dealing with missing and 999999 
##chs_ind[, sid   := fifelse(sid == "999999", NA_integer_, sid)]
##chs_ind[, agepft   := fifelse(agepft == "", NA_integer_, agepft)]
##chs_ind[, bmi  := fifelse(bmi == "", NA_character_, bmi)]
##chs_ind[, fev   := fifelse(fev == "", NA_character_, fev)]
##chs_ind[, fvc  := fifelse(fvc == "", NA_character_, fvc)]
##chs_ind[, mmef  := fifelse(mmef == "", NA_character_, mmef)]
chs_ind[chs_ind$sid==""] <- NA
chs_ind[chs_ind$agepft==""] <- NA
chs_ind[chs_ind$bmi==""] <- NA
chs_ind[chs_ind$fev==""] <- NA
chs_ind[chs_ind$fvc==""] <- NA
chs_ind[chs_ind$mmef==""] <- NA

# Selecting the three relevant columns, and keeping unique records
chs_ind <- unique(chs_ind[, list(sid,townname,male ,asthma,hispanic,smoke,gasstove,agepft, bmi, fev, fvc, mmef)])

# Dropping NAs
chs_ind <- chs_ind[!is.na(sid)]

# Removing duplicates
chs_ind[, n := 1:.N, by = .(sid)]
chs_ind <- chs_ind[n == 1,][, n := NULL]
```

## Step 1: Merging

\-After merging the data, make sure you don’t have any duplicates by
counting the number of rows. Make sure it matches.

``` {r}
chs <- merge(
  x = chs_reg, y = chs_ind,
  by.x = "townname", by.y = "townname",
  all.x = TRUE, all.y = FALSE
  )

# Print out a sample of the data
chs[1:5, .(townname, pm25_mass, male, bmi, fev, fvc, lat, lon)]
```

Based on the number of observations, I do not find duplicates

## Step 2: Creating new categorical variables:

\-Create a new categorical variable named “obesity\_level” using the BMI
measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI
22-24; obese BMI\>24). To make sure the variable is rightly coded,
create a summary table that contains the minimum BMI, maximum BMI, and
the total number of observations per category.

Based on the total number of observations for this new category
“obesity\_level,” there are 1089 observation, which is correct.

``` {r}
##Creating categorical variable called obesity_level
chs$bmi <- as.character(unlist(chs$bmi))
chs_ldt<-lazy_dt(chs, immutable = FALSE)
chs_ldt %>%
  mutate(obesity_level= ifelse(chs$bmi< 14, "underweight",
                         ifelse(chs$bmi > 13 & chs$bmi < 23, "normal",
                                ifelse(chs$bmi > 21 & chs$bmi < 25, "overweight", "obese"))))
table(chs$obesity_level)
```

## Step 3: Creating another categorical variable

\-Create another categorical variable named “smoke\_gas\_exposure” that
summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have
four categories in total.

``` {r}
##Creating categorical variable called smoke_gas_exposure
chs$smoke <- as.numeric(unlist(chs$smoke))
chs$gasstove <- as.numeric(unlist(chs$gasstove))

chs_ldt<-lazy_dt(chs, immutable = FALSE)
chs_ldt %>%
  mutate(smoke_gas_exposure =ifelse(chs$smoke  > 0 & chs$gasstove <1, "only_secondhand",
                         ifelse(chs$smoke <1 & chs$gasstove >0, "only_gasstove",
                               ifelse(chs$smoke >0 & chs$gasstove >0, "both", "no"))))
chs <- chs[!is.na(smoke_gas_exposure)]
table(chs$smoke_gas_exposure)
```

After removing “NA”, the new categorical variable “smoke\_gas\_exposure”
has a total of 1038 obs which is correct.

\#\#Step 4: Create four summary tables -Create four summary table
showing the average (or proportion, if binary) and sd of “Forced
expiratory volume in 1 second (ml)” and asthma indicator by town, sex,
obesity level, and “smoke\_gas\_exposure.”

``` {r}
#summary table
chs <- chs[!is.na(fev)]
chs <- chs[!is.na(asthma)]
chs$fev <- as.double(unlist(chs$fev))
chs$asthma <- as.logical(unlist(chs$asthma))

#summary table for townname
chs[, .(
    fev_avg      = mean(fev, na.rm=TRUE),
    fev_sd      = sd(fev, na.rm=FALSE),
    asthma_prop   = prop.table(asthma, margin = NULL)
    ),
    by = townname
    ][order(townname)] %>% head(n = 4)

#summary table for sex
chs[, .(
    fev_avg      = mean(fev, na.rm=TRUE),
    fev_sd      = sd(fev, na.rm=FALSE),
    asthma_prop   = prop.table(asthma, margin = NULL)
    ),
    by = male
    ][order(male)] %>% head(n = 4)

#summary table for obesity level
chs$obesity_level <- as.character(unlist(chs$obesity_level))
chs[, .(
    fev_avg      = mean(fev, na.rm=TRUE),
    fev_sd      = sd(fev, na.rm=FALSE),
    asthma_prop   = prop.table(asthma, margin = NULL)
    ),
    by = obesity_level
    ][order(obesity_level)] %>% head(n = 4)

#summary table for smoke_gas_exposure
chs$smoke_gas_exposure <- as.character(unlist(chs$smoke_gas_exposure))
chs[, .(
    fev_avg      = mean(fev, na.rm=TRUE),
    fev_sd      = sd(fev, na.rm=FALSE),
    asthma_prop   = prop.table(asthma, margin = NULL)
    ),
    by = smoke_gas_exposure
    ][order(smoke_gas_exposure)] %>% head(n = 4)
```

##### Part B: Looking at the data (EDA)

\-Primary Questions:1. What is the association between BMI and FEV
(forced expiratory volume)? 2. What is the association between smoke and
gas exposure and FEV? 3. What is the association between PM2.5 exposure
and FEV?

``` {r}
dim(chs)
head(chs)
tail(chs)
str(chs)
```

After removing all NA and merging the chs individual and regional
dataset, there are 1022 observation and 40 variables.

``` {r}
##take a closer look at key variables
#BMI
summary(as.double(chs$bmi))
#PM2.5
summary(as.numeric(chs$pm25_mass))
summary(as.numeric(chs$pm25_so4))
summary(as.numeric(chs$pm25_no3))
summary(as.numeric(chs$pm25_nh4))
summary(as.numeric(chs$pm25_oc))
summary(as.numeric(chs$pm25_om))
#FEV
summary(as.double(chs$fev))

```

Based on google BMI range can go up to \>30, therefore the range looks
good. Based on google, the PM2.5mass also looks within range.The FEV
measured in seconds ml is also within range.

``` {r}
# Check Summary Statistics
summary(chs)
```

``` {r}
##Correlation between key variables
#BMI and FEV
cor(as.numeric(chs$bmi), as.numeric(chs$fev), use="complete")


#PM2.5 and FEV
cor(as.numeric(chs$pm25_mass), as.numeric(chs$fev), use="complete")
```

There is not much correlation between pm25\_mass and fev at 0.063. There
is a slight correlation between BMI and FEV at 0.365.

\#\#Visualization of key variables \#Step 1: Facet plot showing
scatterplots with regression lines of BMI vs FEV by “townname”.

``` {r}
#Scatter plot of BMI vs FEV by 'townname'
chs %>% 
  filter(!(townname %in% NA)) %>% 
  ggplot(mapping = aes(x=as.numeric(bmi), y=as.numeric(fev), color=townname))+
  geom_point()+
  stat_smooth(method=lm)
```

The association between the BMI and FEV is positive in all towns (an
increase in BMI is associated with an increase in FEV). The association
is stronger in Santa Maria and
Upland.

## Step 2: Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.

``` {r}
#Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure.

#Computing quartiles to categorize FEV for graphing
chs[, Fev50   := quantile(as.numeric(fev), probs = .5, na.rm = TRUE)]

chs_ldt<-lazy_dt(chs, immutable = FALSE)
chs_ldt %>%
  mutate(Fev_cat =ifelse(as.numeric(chs$fev) < 545.5, "Fev_belowMed", "Fev_aboveMed"))

#FEV by BMI category
chs %>% 
  filter(!(obesity_level %in% NA)) %>% 
  ggplot()+
  geom_bar(mapping = aes(x= Fev_cat, fill = obesity_level))+
  scale_fill_brewer(palette = "PuOr")+
  labs(title="Proportion of FEV by BMI category")

#FEV by smoke/gas exposure
chs %>% 
  filter(!(smoke_gas_exposure %in% NA)) %>% 
  ggplot()+
  geom_bar(mapping = aes(x= Fev_cat, fill = smoke_gas_exposure))+
  scale_fill_brewer(palette = "PuOr")+
  labs(title="Proportion of FEV by Smoke/Gas Exposure")

```

Based on the stacked histogram, there is an association between FEV and
BMI. It looks like the lower the FEV, the higher the chances are to be
underweight. And the higher the FEV, the higher chances of being
overweight.

Based on the stacked histogram, there is minimal association between FEV
levels and smoke/gas exposure.

## Step 3: Barchart of BMI by smoke/gas exposure.

``` {r}
#Barchart of BMI by Smoke/Gas Exposure
chs %>% 
  filter(!(smoke_gas_exposure %in% NA)) %>% 
  ggplot()+
  geom_boxplot(mapping=aes(y=as.numeric(bmi), fill=smoke_gas_exposure))+
facet_wrap(~smoke_gas_exposure, nrow=2)
```

Based on the barcharts, there is a slight association between smoke/gas
exposure and BMI levels. It looks like there is higher BMI levels for
those who experienced both smoke and gas exposure.

#Step 4: Statistical summary graphs of FEV by BMI and FEV by smoke/gas
exposure category.

``` {r}
#Stat_summary of FEV by BMI category
chs %>%
  filter(!(obesity_level %in% NA)) %>% 
  ggplot(mapping=aes(x=obesity_level, y=as.numeric(fev))) + 
    stat_summary(fun.data = "mean_sdl")

#Stat_summary of FEV by Smoke/Gas Exposure Category
chs %>%
  filter(!(smoke_gas_exposure %in% NA)) %>% 
  ggplot(mapping=aes(x=smoke_gas_exposure, y=as.numeric(fev))) + 
    stat_summary(fun.data = "mean_sdl")
```

Based on these stat\_summary plots, we can see that there is an
association between higher FEV with obese and overweight status. We do
not see an association between FEV and smoke/gas
exposure.

## Step 5:A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities.

``` {r}
#leaflet map of PM2.5mass by townname
###ahhhh cannot get this right!

PM_pal= colorFactor(c('blue', 'purple', 'red'), domain = as.factor(chs_reg$pm25_mass))

leaflet(chs_reg) %>%
  addProviderTiles('OpenStreetMap') %>%
  addCircles(lat=~as.numeric(lat), lng=~as.numeric(lon), color=~PM_pal(pm25_mass), opacity = 1, fillOpacity =1, radius=500) %>%
  addLegend('bottomleft', pal=PM_pal, values=chs_reg$pm25_mass, title="PM2.5Mass")
```

## Step 6: Visualisation to examine whether PM2.5 mass is associated with FEV

\-Based on the visualization graphs, it does not seem that PM2.5 mass is
associated with Fev.

``` {r}
chs %>% 
  filter(!(Fev_cat %in% NA)) %>% 
  ggplot()+
  geom_boxplot(mapping=aes(y=as.numeric(pm25_mass), fill=Fev_cat))+
facet_wrap(~Fev_cat, nrow=2)
```

``` {r}
#PM2.5 vs FEV
ggplot(data=chs) +
  geom_point(mapping=aes(x=pm25_mass, y=fev))
```
