#Load in required libraries
library(lme4) 
install.packages('lmerTest') 
library(lmerTest) 
install.packages('MuMIn') 
library(MuMIn) 

#Make up some fake data
g <- rbinom(100,1,0.5) 
x <- rnorm(100) 
y <- x + 2 * g + rnorm(100) 
plot(x,y,col=hsv(h=g / 1.5)) 
# 1 | g means apply the offset everywhere 
m <- lmer(y ~ x + (1 | g)) 
summary(m) 
#Output explaination:
#Scaled residuals can kind of be ignored
#The Fixed Effects report dislays the p value for the X factor. We have a 
#very significant effect of X, as evidenced by <2e-16. This is reflected by
#the obvious trend in the plot

ranef(m) 


rand(m) 
#Does a significance test for the grouping effect. ARe the groupsings, e.g.
#Red and Blue, significantly different? p = 5.158e-13, so very significant.
#Again, this is obviously reflected in the plot

r.squaredGLMM(m)
# R2m (marginal r2) is for fixed effects # R2c (conditional r2) #is
#everything combined (marginal + random effects)
#These are "pseudo-Rsquared" values. They are treated similarly to the
#traditional R^2 value, but because it's calculated differently we have to
#call it a "pseudo-Rsquared".
#The values show that x explains about 1/3 of the data, and random effects 
#explain the next approx. 3/4. This is expected based on the setup of the
#equation.

#Now, let's try a multiple regression
g <- 4 * rbinom(1000,1,0.5) 
x1 <- rnorm(1000) 
x2 <- rnorm(1000) 
y <- x1 + x1 * g + x2 + rnorm(1000)/10 
plot(x1,y,col=hsv(h=g / 6)) 
# x1 | g means ONLY apply the offset to x1 
m <- lmer(y ~ x1 + x2 + (x1 | g)) 
#Ignore the warning message. This is fine for this example
summary(m) 

#The rnorm(1000)/10 is our term to describe the random error. By dividing this
#term by 10, we significantly reduce the error rate. This results in high
#correlation of the data


#In the Correlarion of Fixed Effects, the correlation between x1 and x2 is 0
#(or bear 0). This si good to ensure that each variable is discrete. If we
#got a value of something like >0.4, this indicates an interaction between
#your factors that may impact on your data. Thus, treating them as discrete
#is not statistically accurate. For exmaple, temperature and pressure will
#likely have a >0 correlation, as we know practically that temperature will
#have an effect on pressure. 

ranef(m) 
#In the equation, the group effect, g, essentially increases x1 by 4. This
#results in a measurement error of -2 for the red group, and +2 for the blue
#group. This sums to a difference of 4, which correlates to the factor of 4

rand(m) 


r.squaredGLMM(m)
#The R2c is basically 1. It is so high because of 2 reasons: i) The correlation
#is increased, due to the factor of 4 in the group term, and ii) we have reduced
#the random effects by diving this term by 10

