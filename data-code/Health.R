library(foreign)
library(stats)
library(tidyverse)
library(plyr)

#Load health datasets for 2012-2019
#Filter for children == 0, income <= 5 (income <= 5 means less than 35,000), X_AGE65YR == 1 means 18 < age < 65
#Choose low income , eliminate over 65

year_2008 <- read.xport('data/input/CDBRFS08.XPT')
year_2008<- filter(year_2008, CHILDREN == 88 & INCOME2<= 5) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLAN, MEDCOST) 
  
year_2009 <- read.xport('data/input/CDBRFS09.XPT')  
year_2009<- filter(year_2009, CHILDREN == 88 & INCOME2<= 5) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLAN, MEDCOST) 

year_2010 <- read.xport('data/input/CDBRFS10.XPT')
year_2010<- filter(year_2010, CHILDREN == 88 & INCOME2<= 5) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLAN, MEDCOST) 

year_2011 <- read.xport('data/input/LLCP2011.XPT')
year_2011<- filter(year_2011, CHILDREN == 88 & INCOME2<= 5) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2012<- read.xport('data/input/LLCP2012.XPT')
year_2012<- filter(year_2012, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2013<- read.xport('data/input/LLCP2013.XPT')
year_2013<- filter(year_2013, CHILDREN == 88 & INCOME2<= 5  & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH,INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2014<- read.xport('data/input/LLCP2014.XPT_')
year_2014<- filter(year_2014, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE, HLTHPLN1, MEDCOST)  

year_2015<- read.xport('data/input/LLCP2015.XPT_')
year_2015<- filter(year_2015, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE,HLTHPLN1, MEDCOST) 

year_2016<- read.xport('data/input/LLCP2016.XPT_')
year_2016 <- filter(year_2016, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2017<- read.xport('data/input/LLCP2017.XPT_')
year_2017 <- filter(year_2017, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2018<- read.xport('data/input/LLCP2018.XPT_')
year_2018 <- filter(year_2018, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1) %>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

year_2019<- read.xport('data/input/LLCP2019.XPT_')
year_2019 <- filter(year_2019, CHILDREN == 88 & INCOME2<= 5 & X_AGE65YR == 1)%>% 
  dplyr::select(GENHLTH, INCOME2, X_STATE, HLTHPLN1, MEDCOST) 

#Kentucky = 21 , Georgia = 3 

year_2008_new<- filter(year_2008, X_STATE == 21 | X_STATE == 3)
year_2009_new<- filter(year_2009, X_STATE == 21 | X_STATE == 3)
year_2010_new<- filter(year_2010, X_STATE == 21 | X_STATE == 3)
year_2011_new<- filter(year_2011, X_STATE == 21 | X_STATE == 3)
year_2012_new<- filter(year_2012,  X_STATE == 21 | X_STATE == 3)
year_2013_new<- filter(year_2013,  X_STATE == 21 | X_STATE == 3)
year_2014_new<- filter(year_2014,  X_STATE == 21 | X_STATE == 3)
year_2015_new<- filter(year_2015,  X_STATE == 21 | X_STATE == 3)
year_2016_new<- filter(year_2016,  X_STATE == 21 | X_STATE == 3)
year_2017_new<- filter(year_2017, X_STATE == 21 | X_STATE == 3)
year_2018_new<- filter(year_2018, X_STATE == 21 | X_STATE == 3)
year_2019_new<- filter(year_2019,  X_STATE == 21 | X_STATE == 3)

year_2008_new<- year_2008_new %>% dplyr::rename(HLTHPLN1 = HLTHPLAN)
year_2009_new<- year_2009_new %>% dplyr::rename(HLTHPLN1 = HLTHPLAN)
year_2010_new<- year_2010_new %>% dplyr::rename(HLTHPLN1 = HLTHPLAN)

pre_treatment <- rbind(year_2008_new, year_2009_new, year_2010_new, year_2011_new, year_2012_new,year_2013_new)
post_treatment <- rbind(year_2014_new, year_2015_new, year_2016_new, year_2017_new, year_2018_new, year_2019_new)

pre_treatment$time <- 0
post_treatment$time <- 1

health_data <- rbind.fill(pre_treatment, post_treatment)

# Kentucky=1, Georgia=0
State <- health_data$X_STATE
health_data$treatment <- case_when(State == 21 ~ 1, 
                                   State == 3 ~ 0 )
write_tsv(health_data,'data/output/health_data.txt')



