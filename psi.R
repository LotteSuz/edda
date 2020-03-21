# load data
library(readr)
psi_data <- read_delim("C:/Users/samve/Desktop/MScCS/EDDA/assignment3/edda/data/psi.txt", 
                  " ", escape_double = FALSE, trim_ws = TRUE)

# Question 2.a) 
# Study the data and give a few (> 1) summaries (graphics or tables).
# -------------------------------------------------------------------


# add columns for rounded gpa's
psi_data$gpa_50 = round(psi_data$gpa/.50)*.50
psi_data$gpa_25 = round(psi_data$gpa/.25)*.25
psi_data$gpa_1 = round(psi_data$gpa/1)*1

# normality of gpa
par(mfrow=c(1,2))
hist(psi_data$gpa, main="Average grades (GPA)",xlab="GPA score")
hist(psi_data$gpa_50, main="Rounded average grades (GPA) of students",xlab="GPA score")
hist(psi_data$gpa_25, main="GPA rounded to .25")
shapiro.test(psi_data$gpa) # W = 0.96974, p-value = 0.4921
qqnorm(psi_data$gpa, main="QQ-plot of GPA")
qqnorm(psi_data$gpa_50, main="QQ-plot of rounded GPA")

# for passed (not useful)
tot=xtabs(~gpa+psi,data=psi)
tot 
tot.c=xtabs(passed~gpa+psi,data=psi)
round (tot.c/tot,2)

# for rounded gpa (dit geeft nu mooi 4 rijen zoals in de slides)
tot=xtabs(~gpa_50+psi,data=psi)
tot 
tot.c=xtabs(passed~gpa_50+psi,data=psi)
round (tot.c/tot,2)

# barplot (looks linear? no reason to include gpa*2?)
par(mfrow=c(1,1))
totage=xtabs(~gpa_50,data=psi)
barplot(xtabs(passed~gpa_50,data=psi)/totage, main="Ratio of passing per GPA group",xlab="GPA",ylab="Ratio")

# normal
psiglm=glm(passed~gpa+psi,data=psi,family=binomial)
summary(psiglm)

# with gpa2
psi_data$gpa2 = psi_data$gpa_50^2
psiglm2=glm(passed~gpa+gpa2+psi,data=psi,family=binomial)
summary(psiglm2)

# with rounded gpa to .5
psiglm=glm(passed~gpa_50+psi,data=psi,family=binomial)
summary(psiglm)

# with rounded gpa to .25
psiglm=glm(passed~gpa_25+psi,data=psi,family=binomial)
summary(psiglm)

# factorised
psi_data$passed = factor(psi_data$passed); psi_data$psi = factor(psi_data$psi); psi_data$gpa = factor(psi_data$gpa)
psi_data$passed = as.factor(psi_data$passed); psi_data$psi = as.factor(psi_data$psi)
psi_data$gpa_50 = as.factor(psi_data$gpa_50); psi_data$gpa_1 = as.factor(psi_data$gpa_1)

psiglm3=glm(psi_data$passed~psi_data$gpa_50+psi_data$psi,family=binomial)
summary(psiglm3)

psiglm4=glm(psi_data$passed~psi_data$gpa_1+psi_data$psi,family=binomial)
summary(psiglm4)

# A graph of the coe???cients for the di???erent age categories:
# By inserting the variables as factors each level gets its own parameter, 
# and we can look at the dependence. Disadvantage: (too) many parameters.
plot(c(0,coef(psiglm3)[2:5]),type="l")
plot(c(0,coef(psiglm4)[2:8]),type="l")

# The drop1 command reduces the list of the p-values to one p-value per variable, 
# for testing the null hypothesis that all parameters attached to that variable are 0. 
# The coe???cients of all three variables di???er statistically signi???cantly from 0. 
# The anova command works too, but gives "sequential" tests, which are hard to interpret.
drop1(psiglm3,test="Chisq")
drop1(psiglm4,test="Chisq")



# perform anova
psi_data$passed = as.numeric(psi_data$passed); psi_data$psi = as.numeric(psi_data$psi)
psi_data$gpa_50 = as.numeric(psi_data$gpa_50); psi_data$gpa_25 = as.numeric(psi_data$gpa_25)
anova(lm(psi_data$passed ~ psi_data$psi + psi_data$gpa))
anova(lm(psi_data$passed ~ psi_data$psi + psi_data$gpa_50))
anova(lm(psi_data$passed ~ psi_data$psi + psi_data$gpa_25))

# factorise binary variables
psi_data$passed = as.factor(psi_data$passed); psi_data$psi = as.factor(psi_data$psi)

psi_data=data.frame(psi_data)
wo_psi = data.frame(psi=0,gpa=psi_data$gpa)
w_psi = data.frame(psi=1,gpa=3)
predict(psiglm,wo_psi,type="response") # 0.08230274
predict(psiglm,w_psi,type="response") # 0.4815864 

# e)
tab=matrix(c(3,15,8,6),2,2)
fisher.test(tab)

# calculate by hand
(tab[1,1]/ tab[2,1])/ (tab[1,2]/ tab[2,2])

