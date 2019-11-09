setwd("~/Desktop/Mathemagic/Fall2019/CS614_Interactive_Data_Analysis/Homework_614/HW2")
load('accel.RData')
d = df
summary(d)

# Problem 1
summary(d$walk)
summary(d$income)
str(d$income)
str(d$walk)

wlkinc=d$walk
wlkinc[intersect(which(d$income =="high"),which(d$walk == "high"))] = "hi/hi"
wlkinc[intersect(which(d$income =="high"),which(d$walk == "low"))]  = "hi/lo"
wlkinc[intersect(which(d$income =="low"),which(d$walk == "high"))]  = "lo/hi"
wlkinc[intersect(which(d$income =="low"),which(d$walk == "low"))]   = "lo/lo"

idx1 = which(d$walk == "high" & d$income =="low")

paste("i_", d$income, " w_", d$walk,sep="")

check = cbind(wlkinc,d$income)
check = cbind(check,d$walk)
check[1:60,]

d = cbind(d,wlkinc)
summary(d$wlkinc)

tt1 = table(d$wlkinc)
barplot(tt1)


# Problem 2
summary(d$bout.min)
summary(d$wear)
hist(d$bout.min)
hist(d$wear)
# wear is more skewed, looks like chisqr
hist(log(d$bout.min))

# Problem 3
summary(d$goal_minutes)
hist(d$goal_minutes)
#boxplot(d$goal_minutes)
mean(d$goal_minutes)

# Problem 4
str(d$Sex)
bout_male = d$bout.min[which(d$Sex == "Male")]
bout_female = d$bout.min[which(d$Sex == "Female")]
length(bout_female)
length(bout_male)
length(d$Sex)

boxplot(bout_female,bout_male)

summary(bout_female)
summary(bout_male)
t.test(bout_female,bout_male)
# Problem 5
summary(d)
dpl = d[,c(-1,-8,-9,-10,-11,-12)]
summary(dpl)
plot(dpl)

plot(dpl$bout.min,dpl$goal_minutes)
summary(dpl$goal_minutes)

boxplot(dpl$goal_minutes)
m=quantile(dpl$goal_minutes, c(.25,.50,.75,.90), na.rm=TRUE)
m

# Problem 6
#using norm
setwd("~/Desktop/Mathemagic/Fall2019/CS614_Interactive_Data_Analysis/Classwork/Classwork_2")
d=read.csv('tao.csv')
summary(d)

install.packages('norm')
library('norm')
prelim <- prelim.norm(as.matrix(d))
thetaHat <- em.norm(prelim)
rngseed(1234)
d[is.na(d)]<-imp.norm(prelim, thetaHat, d)[is.na(d)]
