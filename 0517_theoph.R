# system("./mcsim.cpt.v1.model theoph.1cpt.in")
sim <- read.delim("theoph.1cpt.out", skip = 2)
names(sim)

# Validate the httk and mcsim result
par(mfrow = c(2,2), mar = c(4, 5, 2, 1))
for (i in 2:4) {
  plot(sim$Time, sim[,i], xlab = "Time (hour)", ylab = "",
       main = names(sim)[i], las = 1, col = "red", lwd = 2,
       type = "l")
}

source("theoph.post.R")

# trace M_Vd
plot(theoph.c1.1[,1], theoph.c1.1[,2], type = "l")
lines(theoph.c1.2[,1], theoph.c1.2[,2], type = "l", col="red")

# trace M_Ke
plot(theoph.c1.1[,1], theoph.c1.1[,3], type = "l")
lines(theoph.c1.2[,1], theoph.c1.2[,3], type = "l", col="red")

library(boa)
boa.menu()

# Validation
df<-read.delim("1cpt.MCMC1.validate.out", header = T, sep="")
plot(df$Prediction~df$Data, log="xy")
abline(0,1)
