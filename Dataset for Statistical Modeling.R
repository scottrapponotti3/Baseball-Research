##This removes all unwanted pitches and pitchers and creates a data frame of every pitcher and their pitches:

library(RMySQL)
drv=dbDriver("MySQL")
MLB=dbConnect(drv,user="root",password="keno2829",port=3306,dbname="pitchfx_data",host="localhost")
baseball_data=dbGetQuery(MLB,"select * from baseball_data")
baseball_data=subset(baseball_data,pitch_type!="AB")
baseball_data=subset(baseball_data,pitch_type!="UN")
baseball_data=subset(baseball_data,pitch_type!="IN")
baseball_data=subset(baseball_data,pitch_type!="PO")
baseball_data=subset(baseball_data,pitch_type!="FO")	
Number.Pitches=count(baseball_data, "pitcher_name")
Boo=with(Number.Pitches,ifelse(Number.Pitches$freq < 800,TRUE,FALSE)) 

#Creates a vector of boolean values based on whether pitcher exceeded 800 pitches in career

for (i in 1:1505)
{
	pitcher=Number.Pitches[i,1]
	if (Boo[i]){
		baseball_data=subset(baseball_data,pitcher_name!=pitcher)
	}
}
#This loops through to remove all pitchers who haven't thrown at least 800 pitches.

#This function was created so that for any time t from the release of the pitch and for any data frame, outputs a dataframe that for every pitch, calculates the
#final position and velocities of each pitch that will be used for the model

t_predictor=function(data,t) {
	t_d=t
	x_t=signif(data$x0+data$vx0*t_d+0.5*data$ax*(t_d)^2,4)
	z_t=signif(data$z0+data$vz0*t_d+0.5*data$az*(t_d)^2,4)
	y=signif(data$y0+data$vy0*t_d+0.5*data$ay*(t_d)^2,4)
	vx=signif(data$vx0+(data$ax)*t_d,4)
	vz=signif(data$vz0+(data$az)*t_d,4)
	new=data.frame(x_t,z_t,vx,vz)
}

#This function was created so that for any distance d from the plate and for any data frame, outputs a dataframe that for every pitch, calculates the 
#final positions and velocities of each pitch that will be used for the model

distance_predictor=function(data,d){
	t_d=(-1*data$vy0-sqrt((data$vy0)^2-2*data$ay*(50-d)))/data$ay
	x_d=signif(data$x0+data$vx0*t_d+0.5*data$ax*(t_d)^2,4)
	z_d=signif(data$z0+data$vz0*t_d+0.5*data$az*(t_d)^2,4)
	t_tot=(-1*sqrt((data$vy0^2)+2*data$ay*(-48.583))-data$vy0)/data$ay
	t_react=signif(t_tot-t_d,4)
	vx=signif(data$vx0+(data$ax)*t_react,4)
	vz=signif(data$vz0+(data$az)*t_react,4)
	new=data.frame(x_d,z_d,vx,vz)
}

#This function was created so that for any data frame data and for a given pitcher, creates a model of the final position
#of each pitch and compares the results for the pitcher's age, K rate, BB rate, ERA, BAOpp 

t_ratio=function(d,pitcher){
	a=subset(d,pitcher_name==pitcher)
	years=unique(a$year)
	for (i in 1:length(years)) {
		pitch=subset(d,pitcher_name==pitcher & year==years[i])
		fitx=lm(px~x_t+vx,data=pitch)
		fitz=lm(pz~z_t+vz,data=pitch)
		ratiox=summary(fitx)$r.squared
		ratioz=summary(fitz)$r.squared
		if (i==1) {
			new = data.frame(pitcher_name=unique(pitch$pitcher_name), year=unique(pitch$year), age=unique(pitch$age), ratiox,ratioz,ERA=unique(pitch$ERA),BAOpp=unique(pitch$BAOpp),BBper9=unique(pitch$BBper9),Kper9=unique(pitch$Kper9))
		} else {
			new = rbind(new,data.frame(pitcher_name=unique(pitch$pitcher_name), year=unique(pitch$year), age=unique(pitch$age), ratiox,ratioz,ERA=unique(pitch$ERA),BAOpp=unique(pitch$BAOpp),BBper9=unique(pitch$BBper9),Kper9=unique(pitch$Kper9)))
		}
	}
}

#This block of code was created to connect up the pitch fx by pitch data to the overall season statistics for each pitcher from the Lahman database
library(Lahman)
library(dplyr)
library(car)
pitching = filter(Pitching,yearID>=2008)
pitching = pitching %>% left_join(Master, by="playerID")
pitching = pitching %>% arrange(playerID)
for (i in 1:5014) {
	pitching[i,"nameFirst"]=gsub(" ","",pitching[i,"nameFirst"])
} #changes the form of the pitcher's name to be equivalent to that of the pitch fx data
pitching = pitching %>% mutate(pitcher_name=paste(nameFirst,nameLast))
pitching1=pitching %>% select(pitcher_name,as.numeric(yearID),throws,teamID,IPouts,ERA,BAOpp,BB,SO,birthYear)
colnames(pitching1)[2]="year"
baseball_data$year = as.numeric(substr(baseball_data$date,1,4))
pitching2=baseball_data %>% left_join(pitching1,by=c("pitcher_name","year")) #joins the pitchfx datat with the seasonal data
pitching2$age=pitching2$year-pitching2$birthYear
pitching2$Kper9=signif(with(pitching2,SO/(IPouts/3)*9),3)
pitching2$BBper9=signif(with(pitching2,BB/(IPouts/3)*9),3) #calculates age, K rate and BB rate which will be used for comparison with results of the model
p=pitching2[,c("pitcher_name","type","pitch_type","px","pz","x0","z0","vx0","vy0","vz0","ax","ay","az","spin_rate","year","teamID","throws","ERA","BAOpp","age","Kper9","BBper9")]
p_t20=t_predictor(p,0.2) #data of the pitch position and velocity 0.2 seconds after release
p_t20=cbind(p,p_t20)
p_t20=subset(p_t20,pitch_type %in% c("CH","CU","FC","FF","FS","FT","KC","SI","SL"))
all_pitch_train=p_t20[sample(nrow(p_t20),size=5000),] #creates a training set of data used to create the model
all_pitch_test=p_t20[sample(nrow(p_t20),size=20000),] #creates a testing set of data used to test the model
R_pitchers=subset(all_pitch_train, throws=="R")
L_pitchers=subset(all_pitch_train, throws=="L")

scatterplotMatrix(~px+x_t+vx | throws,data=all_pitch_train)
scatterplotMatrix(~pz+z_t+vz | throws,data=all_pitch_train)

plot(R_pitchers$x_t,R_pitchers$px,col="blue",xlab="Horizontal Position at 20sec",ylab="Final Horizontal Position")
points(L_pitchers$x_t,L_pitchers$px,col="red")
abline(a=0.8090564,b=1.1082148) #Represents the coefficents of a model for right handed pitchers
abline(a=-0.802637,b=1.05536) #Represents the coefficents of a linear model for left handed pitchers
legend(-2,2,c("RHP","LHP"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
title("Instantaneous vs Final Horizontal Position")

