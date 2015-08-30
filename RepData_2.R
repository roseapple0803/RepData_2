library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(stringr)

readData <- function()
{
  targetFile <- "stormData.Rds"
	zipFile <- "repdata-data-StormData.csv.bz2"

	if (file.exists(targetFile)) 
	{
		print("Storm data file exists already.")
		thedf <- readRDS(targetFile)	
	}
	else
	{	
		fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
		download.file(fileUrl, zipFile)
		thedf <- read.csv(zipFile)
	}
		
}




findType <- function(x)
{
	max <- nrow(noaa) ## 48 event types
	NAMEMAX <- 4 ## up to 4 different names under each event type

	for (i in 1:max)
	{
		word <- noaa[i, "name1"]
		EVFOUND <- FALSE

		## for now there are only "name1", "name2", "name3" and "name4"	
		for (j in 1:NAMEMAX)
		{
		
			COL <- paste0("name", j)
			str <- noaa[i, COL]	

			if (nchar(str) == 0) break

			x <- str_trim(x)
			criteria <- grepl(str, x, ignore.case=T)	

			foundIt <- length(criteria[criteria]) > 0
			if (foundIt)
			{
				x[criteria] <- toupper(word)
				EVFOUND <- TRUE
				break	
			}
		}
		if (EVFOUND) break	
	}

	return (x)
}




checkAstryExp <- function(x)
{
  isAstry <-  !x %in% convertDollar$abbreSign	
}


## Loading and preprocessing the data
## use a smaller version of the data set containing all the relevant information

##############################################################
##############################################################

storm <- readData()
mydf <- storm %>% 
	select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
	filter(!(FATALITIES == 0 & INJURIES == 0 & PROPDMG == 0 & CROPDMG == 0)) %>%
	mutate(YEAR=year(mdy_hms(BGN_DATE)))

storm.year <- ddply(mydf, c("YEAR"), summarize, total=n())

g <- ggplot(storm.year, aes(x=YEAR, y=total), origin=0) 
theplot <- g + geom_point(color="black") + geom_line(color="red")
theplot <- theplot + scale_x_continuous(limits=c(1950, 2011), breaks=seq(1950,2011,6))
theplot <- theplot +  scale_y_continuous(limits=c(0, 21000), breaks=seq(0,21000,2000))
theplot <- theplot + ylab("Count") + ggtitle("Storm Frequency")
print(theplot)

## Based on the plot, year "1992" is a threshold year
## there are way less storm events being recorded before 1992, but it doesn't mean that the damages caused
## by the storm events are less severe during those early years.


#####################
##  HUMAN DAMAGE 	 ##
#####################

## read in "NOAA.csv", which have 48 event types based on NATIONAL WEATHER SERVICE INSTRUCTION
## see https://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
noaa <- read.csv("NOAA.csv", header=T, stringsAsFactors=F)


## only retrieve records containing damages related to human health
humanDF <- mydf %>%
	select(EVTYPE, FATALITIES, INJURIES, YEAR) %>% 
	filter(FATALITIES > 0 | INJURIES > 0) %>%
	mutate(DMG = FATALITIES + INJURIES) %>%
	arrange(desc(DMG), desc(FATALITIES), desc(INJURIES)) 

						


## transformation work on re-sorting and cleaning up the messy EVTYPEs
## It requires reading in "NOAA.csv" as noaa
EVTYPE <- as.character(unique(humanDF$EVTYPE))
human <- data.frame(EVTYPE)

## this might be un-necessary 
human <- human %>% mutate(newtype = EVTYPE)

human$newtype <- unlist(lapply(human$EVTYPE, findType))
humanDF <- join(humanDF, human, by="EVTYPE")
humanDF <- humanDF %>% select(-EVTYPE) %>% rename(EVTYPE = newtype)
humanDF$EVTYPE <- as.factor(humanDF$EVTYPE)

 



## get ready for plotting
humanTally <- ddply(humanDF, c("EVTYPE"), summarize, FATALITIES=sum(FATALITIES), INJURIES=sum(INJURIES))
humanTally <- humanTally %>% mutate(DMG = FATALITIES + INJURIES) %>% arrange(desc(DMG), desc(FATALITIES), desc(INJURIES))

s.humanTally <- humanTally[1:20,]
s.humanTally$EVTYPE <- factor(s.humanTally$EVTYPE) ##re-factor "EVTYPE"
s.humanTally$EVTYPE <- reorder(s.humanTally$EVTYPE, s.humanTally$DMG) ##re-order "EVTYPE" based on the order of DMG

mt <- melt(s.humanTally, id=c("EVTYPE", "DMG"), variable.name = c("HUMAN_DMG"))


g <- ggplot(mt, aes(x=EVTYPE,y=value, fill=HUMAN_DMG), origin=0)
theplot <- g + geom_bar(color="darkred",stat="identity")
theplot <- theplot + coord_flip()
theplot <- theplot + scale_fill_manual(values=c("lightpink", "lightskyblue2"))
theplot <- theplot +  ylab("Count") + xlab("Storm Event") + ggtitle("Most harmful storm events with respect to human health")
theplot <- theplot + scale_y_continuous(limits=c(0, 100000), breaks=seq(0,100000,20000))
print(theplot) 


## Taking both injuries and fatalities into account, tornado has caused the most damages to human health, and then excessive heat.
 


#####################
## ECONOMIC DAMAGE ##
#####################


## create a lookup list to convert the symbols in both CROPDMGEXP and PROPDMGEXP fields
df1 <- data.frame(abbreSign = c("B", "k", "K","m", "M", "h", "H"), multiplier=c(9, 3, 3, 6, 6, 2, 2)) 
df2 <- data.frame(abbreSign = c("0", "1", "2","3", "4", "5", "6", "7", "8"), multiplier=c(0, 1, 2, 3, 4, 5, 6, 7, 8)) 
convertDollar <- rbind(df1, df2)



## only retrieve records containing damages related to human health
ecoDF <- mydf %>%
	select(EVTYPE, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP, YEAR) %>% 
	filter(PROPDMG > 0 | CROPDMG > 0) 


## transformation work on re-sorting the messy EVTYPE

EVTYPE <- as.character(unique(ecoDF$EVTYPE))
eco <- data.frame(EVTYPE)
eco <- eco %>% mutate(newtype = EVTYPE)

eco$newtype <- unlist(lapply(eco$EVTYPE, findType))
ecoDF <- join(ecoDF, eco, by="EVTYPE")
ecoDF <- ecoDF %>% select(-EVTYPE) %>% rename(EVTYPE = newtype)
ecoDF$EVTYPE <- as.factor(ecoDF$EVTYPE)




## reset all the astry symbols in the exponent fields to "0"
ecoDF[checkAstryExp(ecoDF$PROPDMGEXP), "PROPDMGEXP"] <- "0"
ecoDF[checkAstryExp(ecoDF$CROPDMGEXP), "CROPDMGEXP"] <- "0"


## calc the actual damage amount
ecoDF <- merge(ecoDF, convertDollar, by.x = "PROPDMGEXP", by.y = "abbreSign")
ecoDF <- ecoDF %>% mutate(PROPDMG_V = 10 ** as.integer(multiplier) * PROPDMG) %>% select(-PROPDMG, -PROPDMGEXP, -multiplier) %>% arrange(desc(PROPDMG_V))

ecoDF <- merge(ecoDF, convertDollar, by.x = "CROPDMGEXP", by.y = "abbreSign")
ecoDF <- ecoDF %>% mutate(CROPDMG_V = 10 ** as.integer(multiplier) * CROPDMG) %>% select(-CROPDMG, -CROPDMGEXP, -multiplier) %>% arrange(desc(CROPDMG_V))



## downsize the dataset; perhaps downsizing should take place earlier
ecoDF %>% select(EVTYPE, PROPDMG_V, CROPDMG_V) %>% arrange(desc(PROPDMG_V), desc(CROPDMG_V))





## ecoTally <- ddply(ecoDF, c("EVTYPE"), summarize, totalPROPDMG=sum(PROPDMG_V), totalCROPDMG=sum(CROPDMG_V)) ## decided to change the column names
ecoTally <- ddply(ecoDF, c("EVTYPE"), summarize, PROPERTY=sum(PROPDMG_V), CROP=sum(CROPDMG_V))
ecoTally <- ecoTally %>% mutate(DMG = PROPERTY + CROP) %>% arrange(desc(DMG), desc(PROPERTY), desc(CROP)) 

s.ecoTally <- ecoTally[1:20,]

s.ecoTally <- s.ecoTally %>% mutate(PROPERTY=PROPERTY/1e+9, CROP=CROP/1e+9, DMG=DMG/1e+9)


s.ecoTally$EVTYPE <- factor(s.ecoTally$EVTYPE)
s.ecoTally$EVTYPE <- reorder(s.ecoTally$EVTYPE, s.ecoTally$DMG)
mt <- melt(s.ecoTally, id=c("EVTYPE", "DMG"), variable.name = c("ECO_DMG"))


g <- ggplot(mt, aes(x=EVTYPE,y=value, fill=ECO_DMG), origin=0)
theplot <- g + geom_bar(color="darkred",stat="identity")
theplot <- theplot + coord_flip()
theplot <- theplot + scale_fill_manual(values=c("grey95", "olivedrab4"))
theplot <- theplot +  ylab("Dollar (in 1000 million)") + xlab("Storm Event") + ggtitle("Most harmful storm events with respect to human economics")
##theplot <- theplot + scale_y_continuous(limits=c(0, 1.5e+11), breaks=seq(0,1.5e+11,3.0e+10))
theplot <- theplot + scale_y_continuous(limits=c(0, 180), breaks=seq(0,180,20))
print(theplot)


