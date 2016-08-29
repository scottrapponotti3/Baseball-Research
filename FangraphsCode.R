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
agg = aggregate(pitching1[,c("innings","SO")], by = list(pitching1$pitcher_name,pitching1$year),FUN=sum)
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

#Models for predicting the final horizontal and vertical location of the pitches using smoothing splines
#The values for each variable are the degrees of freedom that were obtained by using cross validation to minimize the test MSE
model.2 = gam(px ~ pitch_type + s(x_t,46.63848) + s(vx,36.40766) + s(spin_rate,14.73384) + s(spin_dir,36.40757), data = p_t20, subset = train)
model.2z = gam(pz ~ pitch_type + s(z_t,37.02998) + s(vz,54.01132) + s(spin_rate,18.66466) + s(spin_dir,36.27269), data = p_t20, subset = train)

#Apply the models to calculate the residuals of the predicted horizontal and vertical location of the pitches
p_t20$predictx = (px - predict(model.2,p_t20)) ^ 2
p_t20$predictz = (pz - predict(model.2z,p_t20)) ^ 2

#Applies the t_ratio function to pitch_data as a list so calculates the mse values for every pitcher and the year they pitched
mse = mapply(t_ratio,pitch_data$pitcher_name,pitch_data$year) %>% do.call(rbind, lapply(a, head, 1))
msex = mse[c(TRUE, FALSE),] #gives the MSE of the the final horizontal position
msez = mse[c(FALSE, TRUE),] #gives the MSE of the final vertical position
pitch_data$msex = msex
pitch_data$msez = msez

pitch_data[order(pitch_data$msex,decreasing=T)[1:10],] #orders the dataframe to find the greatest MSE values in the x direction
pitch_data[order(pitch_data$msez,decreasing=T)[1:10],] #orders the dataframe to find the greatest MSE values in the z direction

#**********************************************************************************
#Functions
#t_ratio takes in the name and year that the pitcher pitched and takes the average of the predicted 
#x and z values for that given year and pitcher
t_ratio = function(pitcher, years) {
	data = subset(p_t20, pitcher_name == pitcher & year == years)
	predict_x = mean(data$predictx, na.rm = TRUE)
	predict_z = mean(data$predictz, na.rm = TRUE)
	out = data.frame(predict_x, predict_z)
}

#This function was created so that for any time t from the release of the pitch and for any data frame, outputs a dataframe that 
#for every pitch, calculates the position and velocities at a given time t_d

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