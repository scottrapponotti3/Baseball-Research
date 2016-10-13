#Fangraphs and PITCHFx Creating the Dataset and Applying Models
#Scott Rapponotti 	8/29/2016

#Code that connects Fangraphs swing data to that of Lahman Database and PITCHFx
#Dataframe that contains the Pitcher's name, Team, O.Swing%, Z.Swing%, Swing%, O.Contact%, Z.Contact%,
#Contact%  Zone% F.Strike% SwStr% year  innings  SO  throws(R or L),  age, 

library(RMySQL)
library(dplyr)
library(Lahman)
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
data08 = read.csv("FanGraphsLeaderboard2008.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2008.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2008)
data09 = read.csv("FanGraphsLeaderboard2009.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2009.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2009)
data10 = read.csv("FanGraphsLeaderboard2010.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2010.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2010)
data11 = read.csv("FanGraphsLeaderboard2011.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2011.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2011)
data12 = read.csv("FanGraphsLeaderboard2012.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2012.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2012)
data13 = read.csv("FanGraphsLeaderboard2013.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2013.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2013)
data14 = read.csv("FanGraphsLeaderboard2014.csv", col.names = colnames1, stringsAsFactors = FALSE) %>% inner_join(read.csv("FanGraphsLeaderboard_HARDHIT2014.csv", col.names=colnames2, stringsAsFactors = FALSE), by = "pitcher_name") %>% mutate(year = 2014)
Fangraphs = rbind(data08, data09, data10, data11, data12, data13, data14)
Fangraphs$playerid.x = NULL
Fangraphs$playerid.y = NULL

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
pitch_data = subset(pitch_data, pitcher_name %in% baseball_data$pitcher_name) #Only Pitchfx data for pitchers in Fangraphs data

#Clean up data set by converting from character to numeric data types
pitch_data$O.Swing = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$O.Swing.)
pitch_data$Z.Swing. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Z.Swing.)
pitch_data$Swing. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Swing.)
pitch_data$O.Contact. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$O.Contact.)
pitch_data$Z.Contact. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Z.Contact.)
pitch_data$Contact. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Contact.)
pitch_data$Zone. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Zone.)
pitch_data$F.Strike. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$F.Strike.)
pitch_data$SwStr. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$SwStr.)
pitch_data$LD. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$LD.)
pitch_data$GB. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$GB.)
pitch_data$FB. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$FB.)
pitch_data$IFFB. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$IFFB.)
pitch_data$HR.FB = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$HR.FB)
pitch_data$Pull. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Pull.)
pitch_data$Cent. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Cent.)
pitch_data$Oppo. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Oppo.)
pitch_data$Soft. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Soft.)
pitch_data$Med. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Med.)
pitch_data$Hard. = mapply(function(x){as.numeric(strsplit(x, " ")[[1]][1])}, pitch_data$Hard.)

inplay_data = subset(baseball_data, type == "X") #find only the balls put in play
inplay_data$result = with(inplay_data, ifelse(grepl("singles", atbat_des), "1B", ifelse(grepl("doubles", atbat_des),
	"2B", ifelse(grepl("triples", atbat_des), "3B", ifelse(grepl("homers", atbat_des), "HR", "Out")))))

#Now that we have the result of every ball in play (assume any ball put in play that is not a hit is an out), we can use a table
#OBA linear weights coefficients for each year from 2008 to 2014
OBA_weights = data.frame(rep(2008:2014,each = 5), rep(c("1B", "2B", "3B", "HR", "Out"), times = 7), 
	c(0.896,1.259,1.587,2.024,0,0.895,1.258,1.585,2.023,0,0.895,1.27,1.608,2.072,0,0.89,1.27,1.611,2.086,0,0.884,1.257,1.593,2.058,0,0.888,1.271,1.616,2.101,0,0.892,1.283,1.635,2.135,0))
colnames(OBA_weights) = c("year", "result", "RunsValue")
inplay_data = inplay_data %>% inner_join(OBA_weights, by = c("year", "result")) #gives the linear weight coefficients for every single atbat result
