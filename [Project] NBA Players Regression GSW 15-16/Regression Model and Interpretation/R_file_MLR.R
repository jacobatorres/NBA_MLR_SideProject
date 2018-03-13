library(MASS)

library(lmtest)
library(ggplot2)
NBA_pbp_data = read.csv("Pbp Positions for Regression.csv")
model = lm(sO.sG ~ SFnum_G + PFnum_G + Cnum_G + SGnum_G + PGnum_G + 
             SFnum_O + PFnum_O + Cnum_O + SGnum_O + PGnum_O + secs,data = NBA_pbp_data)
summary(model)

#USE STEP
step_NBA = stepAIC(model, direction="both")
summary(step_NBA)

#plot if there is influential data
std_resid = rstandard(step_NBA)
cooks_D = cooks.distance(step_NBA)
hat_vals = hatvalues(step_NBA)
plot(hat_vals, std_resid, cex = 10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)


# check for normal distribution of errors
resids_and_pred = data.frame(residual = resid(step_NBA), residual_z = rstandard(step_NBA), fitted.values = predict(step_NBA))
hist(resids_and_pred$residual)
# Analysis: it looks normally distributed

# check for homoscedasticity
plot(resids_and_pred$fitted.values, resids_and_pred$residual)
# Analysis: It looks kind of the same variance across 

# check for independence of errors (no need since this is not time-series data)

# check for linearity per variable
 
terms = predict(step_NBA, type='terms')
partial_resid = resid(step_NBA) + terms
variable <- c(rep("y1", 20), rep("y2", 20) )
for (i in colnames(partial_resid)) {
  
  x = NBA_pbp_data[,i]
  y = partial_resid[,i]
  z = terms[,i]
  
  
  df_temp = data.frame(observed_data = x, resids = y,Terms = z)
  titl = paste(i, " vs error",sep='')
  #plot(title=titl,x,y)
  
  print (ggplot(df_temp, aes(observed_data,resids))  + geom_point(shape=1) + geom_line(aes(observed_data, Terms)) + scale_color_manual(values=c("#CC6666", "#9999CC")))
  
}


