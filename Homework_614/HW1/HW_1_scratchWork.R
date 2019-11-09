# --------------------------Problem_1--------------------------------
mat_primes = rbind(c(2,3,5),c(7,11,13),c(17,19,23),c(29,31,37))

standardizer <- function(vect)
{
  standard_vect = (vect - mean(vect))/sd(vect)
  return(standard_vect)
}

standard_primes = mat_primes
for (i in 1:dim(mat_primes)[2]){
  standard_primes[ ,i] = standardizer(mat_primes[ ,i])
}

standard_primes_v2 = apply(mat_primes,MARGIN=2,function(x) (x - mean(x))/sd(x))

# ---------------------------Problem_2-------------------------------
d=CO2
summary(d)

plot(d$conc,d$uptake)

percabs=(d$uptake/d$conc)*100
d=cbind(d,percabs)

que = d[grep("Quebec",d$Type), ]
missi = d[grep("Mississippi",d$Type), ]

mean_que = mean(que$percabs)
std_que = sd(que$percabs)

mean_missi = mean(missi$percabs)
std_missi = sd(missi$percabs)

hist(d$percabs)

#http://varianceexplained.org/RData/lessons/lesson1/
#https://rmarkdown.rstudio.com/authoring_basics.html