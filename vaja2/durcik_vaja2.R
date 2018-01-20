#Primoz Durcik, Financni praktikum, 2. naloga
#install.packages("actuar")
library(actuar)

# 1. naloga
# 1. naloga a) 
vzorec <- scan("vzorec3.txt")
histogram1 <- hist(vzorec, xlab = "Visina odskodnine", 
                  main = "Histogram odskodnin", freq = NULL, col = "green", xlim = range(0:50))

# 1. naloga b) 

lambda <- mde(vzorec, pexp, start = list(rate = 1), measure = "CvM")

# 1. naloga c)

histogram2 <- hist(vzorec, xlab = "Visina odskodnine", probability = "True",
               main = "Histogram odskodnin", freq = NULL, col = "green", xlim = range(0:50))
curve(dexp, col = "blue", add = TRUE)
legend("topright", "eksponentna porazdelitev", col = "blue", lwd = 1.5)

#primerjava vzorčne in teoretične porazdelitve

graf1 <- plot(ecdf(vzorec), main = "Porazdelitvena funkcija odskodnin", ylab = "Porazdelitvena funkcija", 
     xlim = range(0:50), ylim = range(0:1))
curve(pexp(x, as.numeric(lambda[1])), col = "blue", add = TRUE)
legend("bottomright", c("eksponentna porazdelitev", "empiricna porazdelitev"), 
       col = c("blue", "black"), lwd = 1.5, pch=c(NA,16))

# 1. naloga d) 

#Waldove identitete
#
#E(S) = E(E(S|N)) = E(N*E(Y)) = E(Y)*E(N), N stevilo odskodninskih zneskov
#E(S|N=k) = E(sum|N=k) = E(sum) = sum(E(Yi)) = kE(Y)
#Var(S) = Var(Y)*E(N) + E(Y)^2*Var(N)

#N porazdeljen binomsko
#E(S) = E(Y)*E(N)
n <- 20
p <- 3/4
#upanje <- mean(binom)
lambda <- lambda$estimate[1]
upanjeS1 <- (1/(lambda)*n*p) # = 80.79958
disperzijaS1 <- (1/(lambda^2)*n*p) + (1/lambda)^2*n*p*(1-p) # = 544.047

###

# 2. naloga
# 2. naloga a) 
library(actuar)
h <- 0.25
n <- 100
diskretizacija <- discretize(pexp(x, lambda), step = h, from = 0, to = n*h, method = "rounding")

# 2. naloga b) 

graf2 <- plot(stepfun(x = seq(from = 0, to = 24.75, by = h), y = diffinv(diskretizacija)), pch = NA, 
               col = "Orange", ylab = "Porazdelitvena funkcija", main = "Eksponentna porazdelitev")
curve(pexp(x, lambda), main = "Eksponentna porazdelitev", ylab = "Porazdelitvena funkcija", 
              xlim = range(0:25), ylim = range(0:1), add = TRUE)

# 2. naloga c) 
Fs <- aggregateDist("recursive", model.freq = "binomial",
                    model.sev = diskretizacija, size=20, prob=3/4, x.scale = h)
#plot(Fs)

# 2. naloga d) 
tocke <- knots(Fs) #tocke, kjer so skoki
skoki <- diff(Fs) #kako veliki so
upanjeS2 <- sum(tocke * skoki) # = 64.19297

upanjeS2_kvadrat <- sum(tocke^2 * skoki)
disperzijaS2 <- upanjeS2_kvadrat - upanjeS2^2 # = 1073.28

# 2. naloga e) 

tvegana1 <- VaR(Fs, 0.995) # = 125
izpad1 <- CTE(Fs, 0.005) # = 76.12891

####

# 3 naloga
# 3. naloga a)

vektorS <- numeric(10000)
pojavitve <- numeric(199)
simulacijaN <- rbinom(10000, 20, 3/4)
i=1
while (i<10001) {
  vektorS[i] <- round(sum(rexp(simulacijaN[i], lambda)))
  pojavitve[vektorS[i]] <- pojavitve[vektorS[i]]+1
  i <- i+1
}


# 3. naloga b) 

upanjeS3 <- mean(vektorS) # = 81.06948 ... se spreminja :)
disperzijaS3 <- var(vektorS) # = 555.4152 ... se spreminja :)

abs_razlika_upanj <- abs(upanjeS3 - upanjeS2) # = 16.87652 ... se spreminja :)
abs_razlika_disprezij <- abs(disperzijaS3 - disperzijaS2) # = 517.8648 ... se spreminja :)

# 3. naloga c)

vektorS_crtica <- stepfun(seq(0,(1-(2/length(vektorS))),(1/length(vektorS))),sort(vektorS))
tvegana2 <- vektorS_crtica(0.995) 

primerjava_tveganj <- abs(tvegana2 - tvegana1) 


#d) 

graf4 <- plot(Fs, xlim = c(0,100))
graf4 <- plot(stepfun(sort(vektorS), seq(0, 1, (1/length(vektorS)))), col = "Green", add = TRUE)
legend("topleft", legend = c("Panjerjev algoritem", "Monte Carlo simulacija"),
       col = c("black", "green"), lwd = 2:1)