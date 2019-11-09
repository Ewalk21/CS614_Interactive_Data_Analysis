#Homework 3
#---------Prerec------------
library(devtools)
install_github("vancebee/MarkovSCD")
library(MarkovSCD)
library(ggplot2)
#Load Baseline and Treatment Phase data for one home
BL = HM2$MassAve[HM2$Phase == "BL"]
TX = HM2$MassAve[HM2$Phase == "TX"]
#Define state boundaries
sb = seq(30,90,10)

#-------------Problem 1 ------------
cv = dynamicsconv(tseries1 = BL, tseries2 = TX, nitvl = 10,statebounds = sb,lag = 6)
il1 = cv$ilength1[7]
il2 = cv$ilength2[7]
vv = validitycheck(tseries1 = BL, tseries2 = TX, ilength1 = il1,ilength2 = il2,
                   statebounds = sb,lag = 6)
summary(vv)
summary(il1)
summary(il2)
summary(cv)

d=data.frame("norm"=vv$norm, "iter"=c(-4:2))
d

ggplot(d, aes(x=iter, y = norm)) + geom_line()+
  geom_point(size=4)+
  scale_y_continuous(breaks = c(.7,.8,.9))+
  labs(title="Home 209",
        x ="Iteration Offset from A", y = "Norm")+
  theme(plot.title = element_text(size=25, hjust = 0.5))+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.title.x = element_text(size=20))


#-------------Problem 2 ------------
dd=vv$diagconfig
colnames(dd)=c(-4:2)
rownames(dd)=c(1,0,-1)
#carsLong = melt(d, id.vars= "cyl", variable.name= "motorvar", measure.vars = c("qsec","wt","drat"), value.name = "meas")
meltedd=melt(dd, id.vars = "Pos", variable.name = "label",measure.vars=c(1,0,-1) , value.name = "meas")
meltedd$Var1=as.factor(meltedd$Var1)
colnames(meltedd)=c("Pos","Off","meas")
#names(meltedd)=c("label","Iteration Offset from A","Mean Value")
ggplot(meltedd, aes(x = Off, y = meas))+
  geom_line(aes(group=Pos, col=Pos))+
  geom_point(aes(col=Pos), size = 4)+
  labs(x="Iteration Offset from A",y="Mean Value")+
  theme(axis.title.y = element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(legend.position = c(0.15, 0.85))

#-------------Problem 3 ------------




#-------------Problem 4 ------------
