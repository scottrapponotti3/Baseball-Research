#Non Linear Models (px)
#Regression Spline
#Plots the predictors vs the predictant values then use validation set approach to find the degrees of freedom
#that minimizes the MSE (test error rate)
library(splines)
library(gam)
attach(p_t20)
test = p_t20[sample(nrow(p_t20), 10000),]
plot(test$x_t, test$px, col="darkgray", xlab="Horizontal Position at Time t", ylab="Final Horizontal Position")
title("Natural Spline Regression at Various Degrees of Freedom")
train = sample(nrow(p_t20), nrow(p_t20) / 2)
xtlim = range(x_t)
xt.grid = seq(from = xtlim[1], to = xtlim[2])
colors = list("red","blue","green","black","purple")
vec = rep(0,5)
for (i in 1:5){
	fit = lm(px ~ ns(x_t,df = i), data = p_t20, subset = train)
	pred = predict(fit, newdata = list(x_t = xt.grid),se = T)
	lines(xt.grid,pred$fit,col = as.character(colors[i]), lwd = 2)
	mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	vec[i] = mse
}

legend("topright", legend = c("1 DF","2 DF","3 DF","4 DF","5 DF"), col = c("red", "blue", "green", "black", "purple"), lty = 1, cex = 0.8)
#vec values (test error rates through MSE for x_t)
#0.5850056 0.5578167 0.5504072 0.5286386 0.5289785
#The lowest degrees of freedom for x_t is 4

vxlim = range(vx)
vx.grid = seq(from = vxlim[1], to = vxlim[2])
plot(test$vx,test$px,col="darkgray",xlab = "Horizontal Velocity at Time t", ylab = "Final Horizontal Velocity")
title("Natural Spline Regression at Various Degrees of Freedom")
for (i in 1:5) {
    fit=lm(px ~ ns(vx,df = i), data = p_t20, subset = train)
    pred=predict(fit, newdata = list(vx = vx.grid),se = T)
    lines(vx.grid,pred$fit, col = as.character(colors[i]), lwd = 2)
    mse=mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec[i] = mse
}
legend("topright",legend=c("1 DF","2 DF","3 DF","4 DF","5 DF"),col=c("red", "blue", "green", "black", "purple"),lty=1,cex=0.8)
#vec values (test error rates through MSE for vx)
#0.6512843 0.6036914 0.5689947 0.5021986 0.5017303
#The lowest degrees of freedom for vx is 5

vec_spinrate = rep(0,8) #Calculate the test error rates for natural splines for spin rate
for (i in 1:8) {
    fit = lm(px ~ ns(spin_rate,df = i),data = p_t20, subset = train)
    mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec_spinrate[i] = mse
}
#vec_spinrate = 0.7785695 0.7736687 0.7738714 0.7728317 0.7727904 0.7727921 0.7729785 0.7730620
#The lowest degrees of freedom for spin rate is 4

vec_spindir = rep(0,8) #Calculate the test error rates for natural splines for spin direction
for(i in 1:8) {
    fit = lm(px ~ ns(spin_dir,df = i), data = p_t20, subset = train)
    mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
    vec_spindir[i] = mse
}
#vec_spindir = 0.7558058 0.7531907 0.7426455 0.7412160 0.7387536 0.7380679 0.7383263 0.7380049
#The lowest degrees of freedom for spin direction is 6

#Smoothing Spline
plot(test$x_t,test$px,col="darkgray",xlab="Horizontal Position at Time t",ylab="Final Horizontal Position")
title("Regression using Smoothing Spline")
fit.smooth = smooth.spline(x_t, px, cv = TRUE)
lines(fit.smooth, col = "red", lwd = 2)
#Degrees of Freedom is 21.54186

plot(test$vx,test$px,col="darkgray",xlab="Horizontal Velocity at Time t", ylab="Final Horizontal Velocity")
title("Regression using Smoothing Spline")
fit.smooth = smooth.spline(vx, px, cv = TRUE)
lines(fit.smooth, col = "red", lwd = 2)
#Degrees of Freedom is 30.76314

fit.smooth = smooth.spline(spin_rate, px, cv = TRUE) #Degrees of Freedom is 21.82538
fit.smooth = smooth.spline(spin_dir, px, cv = TRUE) #Degrees of Freedom is 27.70938


#Local Regression
spans = seq(0.1, 0.9, 0.1)
mses = rep(0, length(spans))
for (i in 1:length(spans)) {
	fit = loess(px ~ x_t,span = spans[i], data = p_t20, subset = train)
	mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
#mses 0.5215574 0.5215808 0.5217292 0.5222077 0.5225496 0.5229818 0.5241778 0.5256699 0.5293405
#The span for x_t is 0.1

spans = seq(0.1, 0.9, 0.1)
mses = rep(0, length(spans))
for (i in 1:length(spans)) {
	fit = loess(px ~ vx, span = spans[i], data = p_t20, subset = train)
	mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
#mses 0.4845038 0.4845644 0.4851247 0.4876573 0.4901821 0.4928623 0.4984521 0.5047280 0.5173516
#The span for vx is 0.1

spans = seq(0.1, 0.9, 0.1)
mses = rep(0, length(spans))
for (i in 1:length(spans)) {
	fit = loess(px ~ spin_rate, span = spans[i], data = p_t20, subset = train)
	mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
#mses 0.7754118 0.7734154 0.7731133 0.7728861 0.7728976 0.7728320 0.7729487 0.7731467 0.7732996
#The span for spin rate is 0.4

spans=seq(0.1, 0.9, 0.1)
mses=rep(0, length(spans))
for (i in 1:length(spans)) {
	fit = loess(px ~ spin_dir, span = spans[i], data = p_t20, subset = train)
	mse = mean((px - predict(fit, p_t20))[-train] ^ 2, na.rm = TRUE)
	mses[i] = mse
}
#mses 0.7396586 0.7376261 0.7375083 0.7377340 0.7379434 0.7382783 0.7390705 0.7401375 0.7422783
#The span for spin direction is 0.3

#Generalized Additive Model (One subset of models include spin rate and direction and one that does not)
#The gam package allows generalized additive models in which s is smoothing splines (the numbers are the degrees of freedom obtained from cross validation)
#and lo is local regression with spans that where obtained that had the lowest test error rate using validation set

model1 = lm(px ~ pitch_type + ns(x_t, 4) + ns(vx, 5), data = p_t20, subset = train)
model2 = gam(px ~ pitch_type + s(x_t, 21.54186) + s(vx, 30.76314), data = p_t20, subset = train)
model3 = gam(px ~ pitch_type + lo(x_t,span = 0.1) + lo(vx,span = 0.1), data = p_t20, subset = train)

mean((px - predict(model1, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.06287
mean((px - predict(model2, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.06310
mean((px - predict(model3, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.06264


model.1 = lm(px ~ pitch_type + ns(x_t,4) + ns(vx,5) + ns(spin_rate,4) + ns(spin_dir,6), data = p_t20, subset = train)
model.2 = gam(px ~ pitch_type + s(x_t,21.54186) + s(vx,30.76314) + s(spin_rate,21.82538) + s(spin_dir,27.70938), data = p_t20, subset = train)
model.3 = gam(px ~ pitch_type + lo(x_t,span = 0.1) + lo(vx,span = 0.1) + lo(spin_rate, span = 0.4) + lo(spin_dir,span = 0.3), data = p_t20, subset = train)

mean((px - predict(model.1, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.01663
mean((px - predict(model.2, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.01635
mean((px - predict(model.3, p_t20))[-train] ^ 2, na.rm = TRUE) #mse is 0.01667
#The statistical method with the lowest test error rate is a generalized additive model with smoothing splines
#Adding spin rates and direction improves the model as well
