#Access the SQL database to retrieve all the pitchfx data into a data frame called baseball_data
##This removes all unwanted pitches and pitchers who haven't thrown at least 800 pitches in the dataset
library(RMySQL)
library(dplyr)
drv = dbDriver("MySQL")
MLB = dbConnect(drv,user="root",password="keno2829",port=3306,dbname="pitchfx_data",host="localhost")
baseball_data = dbGetQuery(MLB,"select * from baseball_data")
baseball_data = subset(baseball_data, pitch_type!="AB")
baseball_data = subset(baseball_data, pitch_type!="UN")
baseball_data = subset(baseball_data, pitch_type!="IN")
baseball_data = subset(baseball_data, pitch_type!="PO")
baseball_data = subset(baseball_data, pitch_type!="FO")	
Number.Pitches = count(baseball_data, pitcher_name)
Pitchers = unique(baseball_data$pitcher_name)
Boo = with(Number.Pitches, ifelse(Number.Pitches$n < 800,TRUE,FALSE))


#Creates a vector of boolean values based on whether pitcher exceeded 800 pitches in career
for (i in 1:1505)
{
	pitcher = Pitchers[i,1]
	if (Boo[i]) {
		baseball_data = subset(baseball_data,pitcher_name!=pitcher)
	}
}
#This loops through to remove all pitchers who haven't thrown at least 800 pitches.

#This function was created so that for any time t from the release of the pitch and for any data frame, outputs a dataframe that for every pitch, calculates the
#final position and velocities of each pitch that will be used for the model

t_predictor = function(data, t_d) {
	x_t = signif(data$x0 + data$vx0*t_d + 0.5 * data$ax * (t_d) ^ 2 , 4)
	z_t = signif(data$z0 + data$vz0 * t_d + 0.5 * data$az * (t_d) ^ 2, 4)
	y = signif(data$y0 + data$vy0 * t_d + 0.5 * data$ay * (t_d) ^ 2, 4)
	vx = signif(data$vx0 + (data$ax) * t_d, 4)
	vz = signif(data$vz0 + (data$az) * t_d, 4)
	new = data.frame(x_t, z_t, vx, vz)
}

#This function was created so that for any distance d from the plate and for any data frame, outputs a dataframe that for every pitch, calculates the 
#final positions and velocities of each pitch that will be used for the model

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

#This block of code was created to connect up the pitch fx by pitch data to the overall season statistics for each pitcher from the Lahman database
library(Lahman)
pitching = filter(Pitching, yearID >= 2008) #limits the pitching data to after 2008
pitching = pitching %>% left_join(Master, by = "playerID") %>% arrange(playerID)
pitching$nameFirst = gsub(" ","", pitching$nameFirst)
pitching = pitching %>% mutate(pitcher_name = paste(nameFirst, nameLast)) #change the pitcher id to be similiar to that of pitchfx data
pitching1 = pitching %>% select(pitcher_name, as.numeric(yearID), throws, teamID, IPouts, ERA, BAOpp, BB, SO, birthYear)
colnames(pitching1)[2] = "year"
baseball_data$year = as.numeric(substr(baseball_data$date, 1, 4)) #find the year from the date of each pitch
pitching2 = baseball_data %>% left_join(pitching1, by = c("pitcher_name", "year"))#combines the pitch fx data with the season statistics of the Lahman database
pitching2$age = pitching2$year - pitching2$birthYear
pitching2$Kper9 = signif(with(pitching2, SO / (IPouts / 3) * 9), 3)
pitching2$BBper9 = signif(with(pitching2, BB / (IPouts / 3) * 9), 3)#calculate the age, K rate and BB rate of each year for each pitcher
p = pitching2[,c("pitcher_name","type","pitch_type","batter_name","count","px","pz","x0","z0","vx0","vy0","vz0","ax","ay","az","spin_rate","spin_dir","year","teamID","throws","ERA","BAOpp","age","Kper9","BBper9")]
p = subset(p, pitch_type %in% c("CH","CU","FC","FF","FS","FT","KC","SI","SL"))
p = subset(p, pz < 5 & px < 3 & px > -3) #subsets out outliers of the final location of the pitch
p_t20 = t_predictor(p, 0.2) #applies the function to find the positions and velocities of the ball at 0.20 seconds after release
p_t20 = cbind(p, p_t20)
p_t20$pitch_type = gsub("SI", "FT", p_t20$pitch_type) #replace all sinkers with 2 seam fastballs