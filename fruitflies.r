# include libraries
# library(readr)

# read fruitflies.txt
# ff = read.table("data/fruitflies.txt", header=TRUE, sep="")

# add a column loglongevity
# holder = c()
# for (value in ff$longevity){
#   lg = log10(value)
#   holder = c(holder,lg)
# }
# ff$loglongevity = holder

# a) one-way ANOVA
# investigate normality of all groups
# hist_iso = hist(subset(ff, activity == 'isolated')$loglongevity, main="Loglongevity data for isolated group", xlab='Loglongevity')
# hist_low = hist(subset(ff, activity == 'low')$loglongevity, main="Loglongevity data for low activity group", xlab='Loglongevity')
# hist_high = hist(subset(ff, activity == 'high')$loglongevity, main="Loglongevity data for high activity group", xlab='Loglongevity')
# boxplot((subset(ff, activity == 'isolated')$loglongevity), (subset(ff, activity == 'low')$loglongevity),
#        (subset(ff, activity == 'high')$loglongevity), names=c('isolated', 'low', 'high'), main='Boxplots of loglongevity for each activity level')
# qqnorm(subset(ff, activity == 'high')$loglongevity, main="Loglongevity data for high activity group")
# shapiro.test(subset(ff, activity == 'high')$loglongevity)
# ANOVA on loglongevity based on 'activity'

# NOTES
# print(summary(ff))
# hist(ff$thorax)
#a = subset(ff, activity == 'high')
# hist(subset(ff, activity == 'high')$thorax)

melon = data.frame(num=ff$loglongevity, cat = ff$activity)
anova(lm(num~cat,data=melon))
attach(ff); kruskal.test(loglongevity,activity)
t.test(d$longevity)
