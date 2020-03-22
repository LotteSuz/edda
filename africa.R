# load data (file was edited)
library(readr)
africa <- read_delim("C:/Users/samve/Desktop/MScCS/EDDA/assignment3/edda/data/africa.txt", " ", escape_double = FALSE, trim_ws = TRUE)

africa$pollib = as.factor(africa$pollib)


# Question 3.a) 
# Perform Poisson regression on the full data set africa, taking miltcoup as response variable.
# -------------------------------------------------------------------

africaglm = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim,family=poisson,data=africa)
summary(africaglm)
coef(africaglm)

par(mfrow=c(1,1))
plot(fitted(africaglm),residuals(africaglm))
plot(log(fitted(africaglm)),residuals(africaglm))
plot(log(fitted(africaglm)),residuals(africaglm,type="response"))

# Question 3.b) 
# Use the step down approach (using output of the function summary) to reduce the number of explanatory variables.
# -------------------------------------------------------------------

# step 1: remove numelec
summary(africaglm)

# step 2: remove numregim
africaglm2 = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim,family=poisson,data=africa)
summary(africaglm2)

# step 3: remove size
africaglm3 = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size,family=poisson,data=africa)
summary(africaglm3)

# step 4: remove popn
africaglm4 = glm(miltcoup~oligarchy+pollib+parties+pctvote+popn,family=poisson,data=africa)
summary(africaglm4)

# step 5: remove pctvote
africaglm5 = glm(miltcoup~oligarchy+pollib+parties+pctvote,family=poisson,data=africa)
summary(africaglm5)

# step 6: final model
africaglm6 = glm(miltcoup~oligarchy+pollib+parties,family=poisson,data=africa)
summary(africaglm6)
