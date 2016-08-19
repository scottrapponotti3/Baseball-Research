#Non Linear Models (pz)
#Regression Spline
#Plots the predictors vs the predictant values then use validation set approach to find the degrees of freedom
#that minimizes the MSE (test error rate)
library(splines)
attach(p_t20)
test = p_t20[sample(nrow(p_t20), 10000),]
plot(test$z_t,test$pz,col = "darkgray", xlab = "Vertical Position at Time t", ylab = "Final Vertical Position")
title("Natural Spline Regression Vertical Position")
train = sample(nrow(p_t20), nrow(p_t20) / 2)
ztlim = range(z_t)
zt.grid = seq(from = ztlim[1], to = ztlim[2])
colors = list("red","blue","green","black","purple")
vec = rep(0, 5)
for(i in 1:5) {
    fit = lm(pz ~ ns(z_t, df = i),data = p_t20, subset = train)
    pred = predict(fit, newdata = list(z_t = zt.grid),se = T)
    lines(zt.grid,pred$fit,col = as.character(colors[i]), lwd = 2)
    mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec[i] = mse
} # vec = 0.4380788 0.4242568 0.4238765 0.4241124 0.4240467
#The lowest degree of freedom for z_t is 3

vzlim = range(vz)
vz.grid = seq(from = vzlim[1], to = vzlim[2])
plot(test$vz,test$pz,col="darkgray",xlab = "Vertical Velocity at Time t", ylab = "Final Vertical Velocity")
title("Natural Spline Regression Vertical Velocity")
for(i in 1:5) {
    fit = lm(pz ~ ns(vz,df = i), data = p_t20, subset = train)
    pred = predict(fit, newdata = list(vz = vz.grid),se = T)
    lines(vz.grid,pred$fit, col = as.character(colors[i]), lwd = 2)
    mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec[i] = mse
}
legend("topleft",legend = c("1 DF","2 DF","3 DF","4 DF","5 DF"),col = c("red", "blue", "green", "black", "purple"),lty = 1,cex = 0.8)
# vec = 0.2989440 0.2887695 0.2865216 0.2851558 0.2846801
#The lowest degree of freedom for vz is 5

vec_spinrate = rep(0,8) #Calculate the test error rates for natural splines for spin rate
for(i in 1:8){
    fit = lm(pz~ns(spin_rate,df = i),data = p_t20, subset = train)
    mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec_spinrate[i] = mse
}
#vec_spinrate 0.7607898 0.7582779 0.7582017 0.7579613 0.7583471 0.7582630 0.7584552 0.7587944
#The lowest degree of freedom for spin_rate is 4

vec_spindir = rep(0,8) #Calculate the test error rates for natural splines for spin direction
> for(i in 1:8){
    fit = lm(pz~ns(spin_dir,df = i),data = p_t20, subset = train)
    mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec_spindir[i] = mse
}
#vec_spindir 0.7777585 0.7473159 0.7479655 0.7411796 0.7404396 0.7403044 0.7405680 0.7415335
#The lowest degree of freedom for spin_dir is 4

#Smoothing splines 
#Creates a graph of applying the smoothing spline to the different predictors through cross validation
plot(test$z_t,test$pz,col="darkgray",xlab="Vertical Position at Time t",ylab="Final Vertical Position")
title("Regression using Smoothing Spline Position")
fit.smooth = smooth.spline(z_t, pz, cv = TRUE)
lines(fit.smooth, col = "red", lwd = 2)
#Degrees of Freedom is 19.98483

plot(test$vz,test$pz,col = "darkgray",xlab = "Vertical Velocity at Time t",ylab = "Final Vertical Velocity")
title("Regression using Smoothing Spline Velocity")
fit.smooth = smooth.spline(vz, pz, cv = TRUE)
lines(fit.smooth, col = "red", lwd = 2)
#Degrees of Freedom is 39.49862

fit.smooth = smooth.spline(spin_rate, pz, cv = TRUE) #Degrees of Freedom is 32.14223
fit.smooth = smooth.spline(spin_dir, pz, cv = TRUE) #Degrees of Freedom is 25.25728

#Local Regression
#Find the span that minimizes the MSE (test error rate) through validation set
spans=seq(0.1, 0.9, 0.1)
mses=rep(0, length(spans))
for(i in 1:length(spans)){
	fit = loess(pz ~ z_t,span = spans[i], data = p_t20, subset = train)
	mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
plot(spans,mses,type="b")
title("Local Regression of Vertical Position")
#mses 0.5215574 0.5215808 0.5217292 0.5222077 0.5225496 0.5229818 0.5241778 0.5256699 0.5293405
#The span for z_t is 0.6

mses=rep(0, length(spans))
for(i in 1:length(spans)){
	fit = loess(pz ~ vz,span = spans[i], data = p_t20, subset = train)
	mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
plot(spans,mses,type="b")
title("Local Regression of Vertical Velocity")
#mses 0.2861305 0.2847892 0.2844764 0.2842541 0.2842405 0.2842558 0.2844586 0.2845490 0.2847221
#The span for vz is 0.4

mses=rep(0, length(spans))
for(i in 1:length(spans)){
	fit = loess(pz ~ spin_rate, span = spans[i], data = p_t20, subset = train)
	mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
plot(spans, mses, type = "b")
title("Local Regression of Vertical Final Position vs. Spin Rate")
#mses 0.7632202 0.7609701 0.7595190 0.7587334 0.7583511 0.7583164 0.7583095 0.7583014 0.7583228
#The span for spin_rate is 0.5

mses=rep(0, length(spans))
for(i in 1:length(spans)){
	fit = loess(pz ~ spin_dir, span = spans[i], data = p_t20, subset = train)
	mse = mean((pz - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
plot(spans, mses, type = "b")
title("Local Regression of Vertical Final Position vs. Spin Direction")
#mses 0.7433953 0.7420070 0.7412583 0.7408391 0.7406336 0.7406937 0.7415241 0.7425912 0.7443608
#The span for spin_dir is 0.5


#Generalized Additive Models (One subset of models include spin rate and direction and one that does not)
#The gam package allows generalized additive models in which s is smoothing splines (the numbers are the degrees of freedom obtained from cross validation)
#and lo is local regression with spans that where obtained that had the lowest test error rate using validation set
library(gam)
model1z = lm(pz ~ pitch_type + ns(z_t, 3) + ns(vz, 5), data = p_t20, subset = train)
model2z = gam(pz ~ pitch_type + s(z_t, 19.98483) + s(vz, 39.49862), data = p_t20, subset = train)
model3z = gam(pz ~ pitch_type + lo(z_t, span = 0.6) + lo(vz, span = 0.4), data = p_t20, subset = train)

mean((pz - predict(model1z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.0632457
mean((pz - predict(model2z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.0647913
mean((pz - predict(model3z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.0632818


model.1z = lm(pz ~ pitch_type + ns(z_t, 3) + ns(vz,5) + ns(spin_rate, 4) + ns(spin_dir, 4), data = p_t20, subset = train)
model.2z = gam(pz ~ pitch_type + s(z_t,19.98483) + s(vz,39.49862) + s(spin_rate,32.14223) + s(spin_dir,25.25728), data = p_t20, subset = train)
model.3z = gam(pz ~ pitch_type + lo(z_t,span = 0.6) + lo(vz,span = 0.4) + lo(spin_rate, span = 0.5) + lo(spin_dir,span = 0.5), data = p_t20, subset = train)

mean((pz - predict(model.1z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.0.0481321
mean((pz - predict(model.2z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.04869027
mean((pz - predict(model.3z, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.04805742

#Adding the spin rate and spin direction improves the model and the non parametric regression method with
#the lowest test error is local regression