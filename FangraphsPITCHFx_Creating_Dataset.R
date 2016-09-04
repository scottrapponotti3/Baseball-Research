#Fangraphs and PITCHFx Creating the Dataset and Applying Models
#Scott Rapponotti 	8/29/2016

#Code that connects Fangraphs swing data to that of Lahman Database and PITCHFx
#Dataframe that contains the Pitcher's name, Team, O.Swing%, Z.Swing%, Swing%, O.Contact%, Z.Contact%,
#Contact%  Zone% F.Strike% SwStr% year  innings  SO  throws(R or L),  age, 
#msex (MSE for model predicting the final horizontal location), and msez (MSE for model predicting the final vertical position)

library(RMySQL)
library(dplyr)
library(Lahman)
library(splines)
library(gam)
drv = dbDriver("MySQL") #query from the PITCHFx database I created, the dataframe baseball_data
MLB = dbConnect(drv,user = "root", password = "keno2829", port = 3306, dbname = "pitchfx_data", host = "localhost")
baseball_data = dbGetQuery(MLB,"select * from baseball_data") #start with the default baseball_data R file
baseball_data=subset(baseball_data, pitch_type %in% c("SI","FF","KC","CH","SL","CU","FT","FC","FS")) #remove unwanted pitches
baseball_data$pitcher_name = gsub("[.]","", baseball_data$pitcher_name) 
baseball_data$pitch_type = gsub("SI","FT", baseball_data$pitch_type) #Assume that sinkers and 2 seam fastballs are equivalent
baseball_data$year = as.numeric(substr(baseball_data$date, 1, 4))
setwd("C:/Baseball/REsearch") #set working directory. Place Fangraph files in here or will error

#Reads csv files from fangraphs into data frames in R and creates a new column year
#These csv files contain data from Fangraphs of Plate Displine and Batted Balls from 2008 to 2014
colnames1 = c("pitcher_name","Team","O-Swing%","Z-Swing%","Swing%","O-Contact%","Z-Contact%","Contact%","Zone%","F-Strike%","SwStr%","playerid")
colnames2 = c("pitcher_name","Team","BABIP","GB/FB","LD%","GB%","FB%","IFFB%","HR/FB","RS","RS/9","Balls","Strikes","Pitches","Pull%","Cent%","Oppo%","Soft%","Med%","Hard%","playerid")
data08 = read.csv("FanGraphsLeaderboard2008.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2008.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2008)
data09 = read.csv("FanGraphsLeaderboard2009.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2009.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2009)
data10 = read.csv("FanGraphsLeaderboard2010.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2010.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2010)
data11 = read.csv("FanGraphsLeaderboard2011.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2011.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2011)
data12 = read.csv("FanGraphsLeaderboard2012.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2012.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2012)
data13 = read.csv("FanGraphsLeaderboard2013.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2013.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2013)
data14 = read.csv("FanGraphsLeaderboard2014.csv", col.names = colnames1,stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2014.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2014)
Fangraphs = rbind(data08, data09, data10, data11, data12, data13, data14)
Fangraphs$playerid = NULL

#Load data table Pitching from the Lahman database 
pitching = filter(Pitching, yearID >= 2008)
pitching = pitching %>% left_join(Master, by = "playerID") %>% arrange(playerID)
pitching$nameFirst = gsub(" ","", pitching$nameFirst)
pitching = pitching %>% mutate(pitcher_name = paste(nameFirst, nameLast)) #sets idenitfier for pitcher_name to be the same as Fangraphs and PITCHFx
pitching1 = pitching %>% select(pitcher_name, as.numeric(yearID), throws, teamID, IPouts, ERA, BAOpp, BB, SO, birthYear)
colnames(pitching1)[2] = "year"
pitching1$age = pitching1$year - pitching1$birthYear
pitching1$innings = signif(with(pitching1, IPouts / 3))

#aggregates the data by pitcher name and year and sums up innings and strikeouts for pitchers who are traded midseason
agg = aggregate(pitching1[,c("innings","SO")], by = list(pitching1$pitcher_name,pitching1$year), FUN = sum)
colnames(agg)[1]="pitcher_name"
colnames(agg)[2]="year"


pitch_data = pitching1[!duplicated(pitching1[c("pitcher_name","year")]),] %>% select(pitcher_name, year, throws, age) 
#remove all duplicated pitch data values from Lahman database
pitch_data = agg %>% left_join(pitch_data, by = c("pitcher_name","year")) #sets season stats for each pitcher and the year they pitched
pitch_data = subset(pitch_data, innings >= 50) #want pitchers who have pitched at least 50 innings in a season
pitch_data = Fangraphs %>% left_join(pitch_data, by = c("pitcher_name", "year")) #Combines fangraphs data with each pitcher's season stats
pitch_data$Kper9 = signif(with(pitch_data,(SO / innings) * 9), 3) 
pitch_data = with(pitch_data, pitch_data[order(pitcher_name, year),]) #order the data alphabetically and numerically by year
pitch_data$pitcher_name = gsub("[.]","", pitch_data$pitcher_name)
pitcher_names = unique(as.vector(pitch_data$pitcher_name)) #find all the unique pitchers in Fangraphs data
baseball_data = subset(baseball_data, pitcher_name %in% pitcher_names) #Only Pitchfx data for pitchers in Fangraphs data
baseball_data$year = as.numeric(substr(baseball_data$date, 1, 4)) #creates the year of the pitch in Pitchfx data
