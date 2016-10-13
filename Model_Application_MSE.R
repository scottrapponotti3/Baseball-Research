#Model Application 
#Develop the x and z positional models using smoothing splines to quantify deception at a distance of 40ft 
#from the plate

#
d40 = distance_plate(inplay_data, 40)
d40 = cbind(inplay_data, d40)
inplay_data_train = subset(d40, year == 2008 | year == 2009 | year == 2010)
inplay_data_test = subset(d40, year == 2011 | year == 2012 | year==2013 | year==2014)
train = sample(nrow(inplay_data_train), nrow(inplay_data_train)/2)

with(d40, smooth.spline(x_d, px, cv = FALSE))$df #44.36754
with(d40, smooth.spline(z_d, pz, cv = FALSE))$df #55.94078
model1x = with(inplay_data_train,gam(px ~ s(x_d, 44.36754), data = inplay_data_train, subset = train))
model1z = with(inplay_data_train,gam(pz ~ s(z_d, 55.94078), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model1x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.3208195
mean((inplay_data_train$pz - predict(model1z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2872313
pitch_data_test = subset(pitch_data,  year == 2011 | year == 2012 | year==2013 | year==2014)
mse = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse = do.call(rbind, lapply(mse, head, 1))
pitch_data_test$msex = mse[c(TRUE, FALSE),]
pitch_data_test$msez = mse[c(FALSE, TRUE),]

with(d40, smooth.spline(vx, px, cv = FALSE))$df #46.3862
with(d40, smooth.spline(vz, pz, cv = FALSE))$df #39.14204
model2x = with(inplay_data_train,gam(px ~ s(vx, 46.3862), data = inplay_data_train, subset = train))
model2z = with(inplay_data_train,gam(pz ~ s(vz, 39.14204), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model2x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2985681
mean((inplay_data_train$pz - predict(model2z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.2610672
mse2 = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse2 = do.call(rbind, lapply(mse2, head, 1))
pitch_data_test$msevx = mse2[c(TRUE, FALSE),]
pitch_data_test$msevz = mse2[c(FALSE, TRUE),]

model3x = with(inplay_data_train,gam(px ~ s(x_d, 44.36754) + s(vx, 46.3862), data = inplay_data_train, subset = train))
model3z = with(inplay_data_train,gam(pz ~ s(z_d, 55.94078) + s(vz, 39.14204), data = inplay_data_train, subset = train))
mean((inplay_data_train$px - predict(model3x, inplay_data_train))[-train]^2, na.rm = TRUE) #0.176168
mean((inplay_data_train$pz - predict(model3z, inplay_data_train))[-train]^2, na.rm = TRUE) #0.22667
mse3 = mapply(t_ratio, pitch_data_test$pitcher_name, pitch_data_test$year)
mse3 = do.call(rbind, lapply(mse3, head, 1))
pitch_data_test$msexvx = mse3[c(TRUE, FALSE),]
pitch_data_test$msexvz = mse3[c(FALSE, TRUE),]
