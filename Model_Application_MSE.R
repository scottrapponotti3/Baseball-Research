#Model Application 
#Develop the x and z positional models using smoothing splines to quantify deception at a distance of 40ft 
#from the plate

#inplay_data contains the orginal dataset of pitch fx data but only contains balls that were put in play
#d40 is the pitch fx data of balls put in a play but also contains the the variables of the position and velocity at 
#a distance of 40ft from the plate, calculated using the distance_plate function
library(dplyr)
d40 = distance_plate(inplay_data, 40)
d40 = cbind(inplay_data, d40)
inplay_data_train = subset(d40, year == 2008 | year == 2009 | year == 2010) 
inplay_data_test = subset(d40, year == 2011 | year == 2012 | year==2013 | year==2014)
train = sample(nrow(inplay_data_train), nrow(inplay_data_train)/2)

#We have a training set containing data from 2008 to 2010 and a testing set of data from 2011 to 2014
#The training set is also split into model training and a validation set with the degrees of freedom calculated
#using smoothing splines and generalized cross validation

#model1x - predict the final horizontal position of the ball from only the horizontal position at a distance d
#model1z - predict the final vertical position of the ball from only the vertical position at a distance d
#model2x - predict the final horizontal position of the ball from the horizontal velocity at a distance d
#model2z - predict the final vertical position of the ball from the vertical velocity at a distance d
#model3x - predict the final horizontal position of the ball from the horizontal position and velocity at a distance d
#model3z - predict the final vertical position of the ball from the vertical position and velocity at a distance d
with(d40, smooth.spline(x_d, px, cv = FALSE))$df #44.36754
with(d40, smooth.spline(z_d, pz, cv = FALSE))$df #55.94078
model1x = with(inplay_data_train,gam(px ~ s(x_d, 44.36754), data = inplay_data_train, subset = train))
model1z = with(inplay_data_train,gam(pz ~ s(z_d, 55.94078), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model1x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.3208195
mean((inplay_data_train$pz - predict(model1z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2872313
pitch_data_test = subset(pitch_data,  year == 2011 | year == 2012 | year==2013 | year==2014)
#mse is calculated by finding the average squared difference of the actual value and predicted value, the Mean Squared Error
#t_ratio just averages the mse for each pitcher and the  year they pitched. MSE is used to quantify deception
mse = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse = do.call(rbind, lapply(mse, head, 1))
pitch_data_test$msex = mse[c(TRUE, FALSE),]#msex is the horizontal deception of model1x
pitch_data_test$msez = mse[c(FALSE, TRUE),]#msez is the vertical deception of model1z

with(d40, smooth.spline(vx, px, cv = FALSE))$df #46.3862
with(d40, smooth.spline(vz, pz, cv = FALSE))$df #39.14204
model2x = with(inplay_data_train,gam(px ~ s(vx, 46.3862), data = inplay_data_train, subset = train))
model2z = with(inplay_data_train,gam(pz ~ s(vz, 39.14204), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model2x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2985681
mean((inplay_data_train$pz - predict(model2z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2610672
mse2 = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse2 = do.call(rbind, lapply(mse2, head, 1))
pitch_data_test$msevx = mse2[c(TRUE, FALSE),]#msevx is the horizontal deception of model2x
pitch_data_test$msevz = mse2[c(FALSE, TRUE),]#msevz is the vertical deception of model2z

model3x = with(inplay_data_train,gam(px ~ s(x_d, 44.36754) + s(vx, 46.3862), data = inplay_data_train, subset = train))
model3z = with(inplay_data_train,gam(pz ~ s(z_d, 55.94078) + s(vz, 39.14204), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model3x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.176168
mean((inplay_data_train$pz - predict(model3z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.22667
mse3 = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse3 = do.call(rbind, lapply(mse3, head, 1))
pitch_data_test$msexvx = mse3[c(TRUE, FALSE),]#msexvx is the horizontal deception of model3x
pitch_data_test$msexvz = mse3[c(FALSE, TRUE),]#msezvz is the horizontal deception of model3z

#This dataset contains the average position, times, velocities and spins of each pitcher, year and pitch they threw
pitchers_play = aggregate(d40[,c("px","pz","x0","z0","x_d","z_d","vx","vz","t_d","t_tot","t_rem", "spin_rate","spin_dir")], list(pitcher_name=d40$pitcher_name, year=d40$year, pitch_type=d40$pitch_type), FUN=mean, na.rm=TRUE)
pitchers_play = with(pitchers_play, pitchers_play[order(pitcher_name, year),])

#This is a way of quickly running test cases to compare pitcher year by year data. Can later be joined with pitch_data
pitchers_pitch = aggregate(count ~ pitcher_name + year + pitch_type, data = d40, FUN = length)
pitchers_pitch = with(pitchers_pitch, pitchers_pitch[order(pitcher_name, year),])
#finds the number of pitches thrown by a pitcher in a given year for a given pitch type
pitchers_play$count = pitchers_pitch$count
pitchers_play$des = mapply(pitcher_des,pitchers_play$pitcher_name,pitchers_play$year) #create a description of what kind of pitcher based on what pitches he throws
throws = pitch_data[,c("pitcher_name","throws")]#finds the handidiness of the pitcher
throws = throws[!duplicated(throws$pitcher_name),]
pitchers_play = pitchers_play %>% inner_join(throws, by = "pitcher_name")
#Joins pitcher handidness for each pitch
p = pitchers_play[pitchers_play$year>2010,c("pitcher_name","year","des")]
p = p[!duplicated(p),]
pitch_data_test = pitch_data_test %>% left_join(p, by = c("pitcher_name","year")) #Now we have pitcher description of pitch mix
#for example FF,SI,CU would be a pitcher mix of four-seam fastball, slider, and curveball
pitch_data_test=pitch_data_test[!is.na(pitch_data_test$throws),] #remove all missing values
