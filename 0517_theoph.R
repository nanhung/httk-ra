# system("./mcsim.cpt.v1.model theoph.1cpt.in")
sim <- read.delim("theoph.1cpt.out", skip = 2)
names(sim)

par(mfrow = c(2,2), mar = c(4, 5, 2, 1))
for (i in 2:4) {
  plot(sim$Time, sim[,i], xlab = "Time (hour)", ylab = "",
       main = names(sim)[i], las = 1, col = "red", lwd = 2,
       type = "l")
}
dev.off()

