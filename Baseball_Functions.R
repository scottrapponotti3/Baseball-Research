#Functions
#R code that determines the horizontal and vertical positions of the ball at a given time t or distance d 

#This function was created so that for any time t_d from the release of the pitch and for any data frame, outputs a dataframe that 
#for every pitch, calculates the position and velocities at that time

#x_t, z_t are the horizontal and vertical positions of the ball at a time t
#vx and vz are the horizontal and vertical velocities of the ball at a time t
tmound = function(data, t) {
	vy = -1 * sqrt(data$vy0 ^ 2 + 2 * data$ay * (-48.583))
	t_tot = (vy - data$vy0) / data$ay
	t_diff = t_tot - t
	x_t = signif(data$x0 + data$vx0 * t + 0.5 * data$ax * (t) ^ 2, 4) #X Position of the ball at a time t
	z_t = signif(data$z0 + data$vz0 * t + 0.5 * data$az * (t) ^ 2, 4) #Z Position of the ball at a time t
	vx = signif(data$vx0 + (data$ax) * t, 4) #X Velocity of the ball at a time t
	vz = signif(data$vz0 + (data$az) * t, 4) #Z Velocity of the ball at a time t
	new = data.frame(x_t, z_t, vx, vz, vy, t_tot, t_diff)
}

#Finds the positions and velocities at a time t_plate which is the time before reaching the plate
#vy is the velocity of the ball at the end of the plate (48.583 ft)
#t_tot is the time of the flight of the pitch
# t is the variable time from the ball to cross the plate
#x_t, z_t are the horizontal and vertical positions of the ball at a time t
#vx and vz are the horizontal and vertical velocities of the ball at a time 

t_plate = function(data, t_plate) {
	vy = -1 * sqrt(data$vy0 ^ 2 + 2 * data$ay * (-48.583))
	t_tot = (vy - data$vy0) / data$ay
	t = t_tot - t_plate
	x_t = signif(data$x0 + data$vx0 * t + 0.5 * data$ax * (t) ^ 2 , 4)
	z_t = signif(data$z0 + data$vz0 * t + 0.5 * data$az * (t) ^ 2, 4)
	vx = signif(data$vx0 + (data$ax) * t, 4)
	vz = signif(data$vz0 + (data$az) * t, 4)
	new = data.frame(x_t, z_t, vx, vz)
}

#Finds the positions and velocities at a time of the difference between the each pitch and the average Fastball time
#vy0FF average initial velcoity of the fastball
#vyFF average final velocity of the fastball
#tFB is the time of each fastball thrown 
t_pitcher = function(pitcher) {
	data = subset(baseball_data, pitcher_name == pitcher)
	dataFF = subset(data, pitch_type == "FF")
	vy0FF = -1 * sqrt(((mean(dataFF$start_speed) / 0.681818)^ 2 - mean(dataFF$vx0) ^ 2 - mean(dataFF$vz0) ^ 2))
	vyFF = - 1 * sqrt(mean(dataFF$vy0) ^ 2 + 2 * mean(dataFF$ay) * (-48.583))
	tFB = (vyFF - vy0FF) / mean(dataFF$ay)
	vy = -1 * sqrt(data$vy0 ^ 2 + 2 * data$ay * (-48.583))
	t_tot = (vy - data$vy0) / data$ay
	tdiff = abs(tFB - t_tot)
	x_t = signif(data$x0 + data$vx0 * tdiff + 0.5 * data$ax * tdiff ^ 2 , 4)
	z_t = signif(data$z0 + data$vz0 * tdiff + 0.5 * data$az * tdiff ^ 2, 4)
	vx = signif(data$vx0 + (data$ax) * tdiff, 4)
	vz = signif(data$vz0 + (data$az) * tdiff, 4)
	new = data.frame(x_t, z_t, vx, vz, t_tot, tdiff)
}

#Function that given a dataset and a distance d from the plate calculates the total time of the flight of the ball,
#the time it takes the ball to travel that distance and the time remaining for the batter to react along with the
#position and velocity of that ball in the x and z direction
#t_d is the time the ball travels at a distance d
#x_d is the horizontal position at a distance d
#z_d is the vertical position at a distance d
#vx is the horizontal velocities at a distance d
#vz is the vertical velocities at a distance d
distance_plate = function(data, d) {
	vy = -1 * sqrt(data$vy0 ^ 2 + 2 * data$ay * (-48.583))
	t_tot = (vy - data$vy0) / data$ay
	t_d = (-1 * data$vy0 - sqrt((data$vy0) ^ 2 - 2 * data$ay * (50 - d))) / data$ay
	t_rem = t_tot - t_d
	x_d = signif(data$x0 + data$vx0 * t_d + 0.5 * data$ax * (t_d) ^ 2, 4)
	z_d = signif(data$z0 + data$vz0 * t_d + 0.5 * data$az * (t_d) ^ 2, 4)
	vx = signif(data$vx0 + (data$ax) * t_d, 4)
	vz = signif(data$vz0 + (data$az) * t_d, 4)
	new = data.frame(x_d, z_d, vx, vz, t_d, t_tot, t_rem)
}

#Function that given a dataset, models for a pitcher and the year they pitched, finds the msex and msez values
#to quantofy the deception of that pitcher
t_ratio = function(pitcher, years) {
    data = subset(inplay_data_test, pitcher_name==pitcher & year==years)
    predict_x = mean((data$px - predict(model1x, data))^2, na.rm = TRUE)
    predict_z = mean((data$pz - predict(model1z, data))^2, na.rm = TRUE)
    out = data.frame(predict_x, predict_z)
}

#Function that for a given pitcher and year they pitched, finds the number of each pitch he threw that given year
#sorts the pitches by the number he threw (from greatest to least) and only includes pitches he threw at least 5% of the time
#The purpose is to create a pitcher description to track the usage of each pitch to help run case studies of deception
pitcher_des = function(pitcher, years) {
    data = subset(pitchers_play, pitcher_name == pitcher & years == year)
    data = with(data, data[order(count, decreasing = TRUE),])
    total = sum(data$count)
    data$percent_thrown = (data$count / total) * 100
    pitches = as.vector(data[data$percent_thrown > 5, "pitch_type"])
    des = ifelse(length(pitches) <= 3, paste(pitches,collapse=",",sep=""), paste(pitches[1],",", pitches[2],",", pitches[3],",", pitches[4]))
    des = gsub(" ","",des)
    return (des)
}