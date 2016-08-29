#Fangraphs and PITCHFx Creating the Dataset and Applying Models
#Scott Rapponotti 	8/29/2016

#Code that connects Fangraphs swing data to that of Lahman Database and PITCH/fx
#Dataframe that contains the Pitcher's name, Team, O.Swing%, Z.Swing%, Swing%, O.Contact%, Z.Contact%,
#Contact%  Zone% F.Strike% SwStr% year  innings  SO  throws(R or L),  age, 
#msex (MSE for model predicting the final horizontal location), and msez (MSE for model predicting the final vertical position)

library(RMySQL)
library(dplyr)
library(Lahman)
drv = dbDriver("MySQL") #query from the PITCH/fx database I created, the dataframe baseball_data
MLB = dbConnect(drv,user = "root", password = "keno2829", port = 3306, dbname = "pitchfx_data", host = "localhost")
baseball_data = dbGetQuery(MLB,"select * from baseball_data") #start with the default baseball_data R file
baseball_data=subset(baseball_data, pitch_type %in% c("SI","FF","KC","CH","SL","CU","FT","FC","FS")) #remove unwanted pitches
baseball_data$pitcher_name = gsub("[.]","", baseball_data$pitcher_name) 
baseball_data$pitch_type = gsub("SI","FT", baseball_data$pitch_type) #Assume that sinkers and 2 seam fastballs are equivalent
setwd("C:/Baseball/REsearch") #set working directory. Place Fangraph files in here or will error

#Reads csv files from fangraphs into data frames in R and creates a new column year
data08 = read.csv("FanGraphsLeaderboard2008.csv") %>% mutate(year = 2008) 
data09 = read.csv("FanGraphsLeaderboard2009.csv") %>% mutate(year = 2009)
data10 = read.csv("FanGraphsLeaderboard2010.csv") %>% mutate(year = 2010)
data11 = read.csv("FanGraphsLeaderboard2011.csv") %>% mutate(year = 2011)
data12 = read.csv("FanGraphsLeaderboard2012.csv") %>% mutate(year = 2012)
data13 = read.csv("FanGraphsLeaderboard2013.csv") %>% mutate(year = 2013)
data14 = read.csv("FanGraphsLeaderboard2014.csv") %>% mutate(year = 2014)
Fangraphs = rbind(data08,data09,data10,data11,data12,data13,data14)
colnames(Fangraphs)[1] = "pitcher_name"
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

#remove all duplicated pitch data values from the Lahman database
pitch_data = pitching1[!duplicated(pitching1[c("pitcher_name","year")]),] %>% select(pitcher_name, year, throws, age)
pitch_data = agg %>% left_join(pitch_data, by = c("pitcher_name","year"))
pitch_data = subset(pitch_data, innings >= 50) #want pitchers who have pitched at least 50 innings in a season
pitch_data = pitch_data[!is.na(pitch_data$throws),] #remove empty data
pitch_data = Fangraphs %>% left_join(pitch_data, by = c("pitcher_name", "year"))
pitch_data$Kper9 = signif(with(pitch_data,(SO / innings) * 9), 3) 
p_t20 = t_predictor(baseball_data, 0.2) #apply the t_predictor function I created to calculate the pitch location and velocity at 0.2 seconds
p_t20 = cbind(baseball_data, p_t20)
pitch_data = with(pitch_data, pitch_data[order(pitcher_name, year),]) #order the data alphabetically and numerically by year
pitch_data$pitcher_name = gsub("[.]","", pitch_data$pitcher_name)
pitcher_names = unique(as.vector(pitch_data$pitcher_name)) #find all the unique pitchers in Fangraphs data
p_t20 = subset(p_t20, pitcher_name %in% pitcher_names) #Only Pitchfx data for pitchers in Fangraphs data
p_t20$year = as.numeric(substr(p_t20$date, 1, 4)) #creates the year of the pitch in Pitchfx data

#**********************************************************************************
#Functions
#This function was created so that for any time t_d from the release of the pitch and for any data frame, outputs a dataframe that 
#for every pitch, calculates the position and velocities at that time

#x0, vx0, ax are the initial position, velocity and acceleration in the x direction per standard PITCHFx
#z0, vz0, az are the initial position, velocity and acceleration in the z direction per standard PITCHFx
t_predictor = function(data, t_d) {
	x_t = signif(data$x0 + data$vx0*t_d + 0.5 * data$ax * (t_d) ^ 2 , 4)
	z_t = signif(data$z0 + data$vz0 * t_d + 0.5 * data$az * (t_d) ^ 2, 4)
	y = signif(data$y0 + data$vy0 * t_d + 0.5 * data$ay * (t_d) ^ 2, 4)
	vx = signif(data$vx0 + (data$ax) * t_d, 4)
	vz = signif(data$vz0 + (data$az) * t_d, 4)
	new = data.frame(x_t, z_t, vx, vz)
}

##This function was created so that for any distance d from the plate and for any data frame, outputs a dataframe that for 
#every pitch, calculates the positions and velocities at that distance

distance_predictor = function(data, d) {
	t_d = (-1 * data$vy0 - sqrt((data$vy0) ^ 2 - 2 * data$ay * (50 - d))) / data$ay
	x_d = signif(data$x0 + data$vx0 * t_d + 0.5 * data$ax * (t_d) ^ 2, 4)
	z_d = signif(data$z0 + data$vz0 * t_d + 0.5 * data$az * (t_d) ^ 2, 4)
	t_tot = (-1 * sqrt((data$vy0 ^ 2) + 2 * data$ay * (-48.583)) - data$vy0) / data$ay
	t_react=signif(t_tot - t_d, 4)
	vx = signif(data$vx0 + (data$ax) * t_react, 4)
	vz = signif(data$vz0 + (data$az) * t_react, 4)
	new = data.frame(x_d, z_d, vx, vz)
}
