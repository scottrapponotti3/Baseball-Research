#Model Application 
#Apply the Linear x and z models along with the nonlinear x and z models 

#p_t20 variable is a data frame that combines pitch fx and season statistics from the Lahman database, and
#contains the position and velocity of the ball at 0.2 seconds after release

p_t20 = p_t20[!is.na(p_t20$throws),] #removes NA values of pitchers who don't have a value for the variable throws
pitch_data = p_t20[!duplicated(p_t20[c("pitcher_name","year")]),] %>% select(pitcher_name,year,age,BBper9,Kper9,BAOpp,ERA)

#Calculate the squared residual values of the predicted values of the model and the actual values of px and pz
#We can repeat this process for the non linear models as well and compare results
p_t20$predictx = (px - predict(modelx,pt20)) ^ 2
p_t20$predictz = (pz - predict(modelz,pt20)) ^ 2

#This function finds the mean of the squared residuals for each subset of the pitcher and the year they pitched
t_ratio = function(pitcher, years) {
	data = subset(p_t20, pitcher_name==pitcher & year==years)
	predict_x = mean(data$predictx, na.rm = TRUE)
	predict_z = mean(data$predictz, na.rm = TRUE)
	out = data.frame(predict_x, predict_z)
}

#Applies the t_ratio function to pitch_data as a list so calculates the mse values for every pitcher and the year they pitched
mse = mapply(t_ratio,pitch_data$pitcher_name,pitch_data$year) %>% do.call(rbind, lapply(a, head, 1))
msex = mse[c(TRUE, FALSE),] #gives the MSE of the the final horizontal position
msez = mse[c(FALSE, TRUE),] #gives the MSE of the final vertical position
pitch_data$msex = msex
pitch_data$msez = msez
