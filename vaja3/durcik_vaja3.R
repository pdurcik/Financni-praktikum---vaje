#install.packages("combinat")
#install.packages("Rlab")
library(Rlab)
library(combinat)

# 1. naloga

S0=50
u=1.05
d=0.95
T=5
R=0.03
W=c(1:6)


# a)

izplacilo <- function(vrsta, W, type){
  K=0
  i=1
  while (i<=length(W)){
    K=K+(vrsta[i]*W[i])/sum(W)
    i=i+1
  }
  if (type=="call") {
    return(max(0,vrsta[length(vrsta)]-K))
  }
  if (type=="put") {
    return(max(0,K-vrsta[length(vrsta)]))
  }
}

# b)

c1<-c(50.00, 52.50, 49.88, 47.38, 45.01, 47.26,
      50.00, 52.50, 55.12, 57.88, 60.78, 63.81,
      50.00, 47.50, 49.88, 47.38, 45.01, 42.76,
      50.00, 47.50, 45.12, 47.38, 45.01, 47.26,
      50.00, 52.50, 49.88, 52.37, 54.99, 52.24)
matrika_cen<-matrix(c1,nrow=6, ncol=5)
matrika_cen<-t(matrika_cen)

matrika_cen_razsirjena <- cbind(matrika_cen,c(1,1,1,1,1))
matrika_cen_razsirjena <- cbind(matrika_cen_razsirjena,c(1,1,1,1,1))

#matrika_cen_razsirjena <- apply(matrika_cen, 1, izplacilo, W=W, type=type)

j=1
while (j<=5) {
  matrika_cen_razsirjena[j,7]<-izplacilo(matrika_cen[j,],W,"call")
  matrika_cen_razsirjena[j,8]<-izplacilo(matrika_cen[j,],W,"put")
  j=j+1
}

# 2. naloga

# a)

binomski <- function(S0, u, d, R, Times, W, type) {
  poti <- hcube(rep(2,Times), translation = -1)
  q=(1+R-d)/(u-d)
  r <- rowSums(poti)
  vektor_verjetnosti <- q^r*(1-q)^(Times-r)
  smeri <- u^(poti)*d^(1-poti)
  smeri <- cbind(rep(S0, nrow(poti)), smeri)
  matrika_vrednosti <- t(apply(smeri, 1, cumprod))
  vektor_izplacil <- apply(matrika_vrednosti, 1, izplacilo, W=W, type=type)
  EQodX <- sum(vektor_verjetnosti*vektor_izplacil)
  cena <- EQodX/((1+R)^Times)
  return(cena)
}

# b)

monte <- function(S0, u, d, R, Times, W, type, N) {
  poti <- matrix( nrow = N, ncol = Times)
  q=(1+R-d)/(u-d)
  i=1
  while(i<=N) {
    vektor_0in1 <- rbinom(Times,1,q)
    poti[i,] <- vektor_0in1
    i=i+1
  }
  r <- rowSums(poti)
  vektor_verjetnosti <- q^r*(1-q)^(Times-r)
  smeri <- u^(poti)*d^(1-poti)
  smeri <- cbind(rep(S0, nrow(poti)), smeri)
  matrika_vrednosti <- t(apply(smeri, 1, cumprod))
  vektor_izplacil <- apply(matrika_vrednosti, 1, izplacilo, W=W, type=type)
  EQodX <- sum(vektor_izplacil)/N
  cena <- EQodX/((1+R)^Times)
  return(cena)
}

m1 <- monte(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put", N=10)
m2 <- monte(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put", N=100)
m3 <- monte(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put", N=1000)

# 3. naloga

# a)

M <- 100
S0 = 60
u = 1.05
d = 0.95
R = 0.01
Times = 15
W = rep(1, 16)
type = "put"

#N=10

N=10
cene10 <- c(1:M)
j=1
while (j<=M) {
  cene10[j] <- monte(S0, u, d, R, Times, W, type, N)
  j=j+1
}

h1 <- hist(cene10, xlab = "Premija", 
       main = "Monte Carlo: N = 10",  col = "yellow", xlim= c(0,5))

#N=100

N=100
cene100 <- c(1:M)
j=1
while (j<=M) {
  cene100[j] <- monte(S0, u, d, R, Times, W, type, N)
  j=j+1
}

h2 <- hist(cene100, xlab = "Premija", 
           main = "Monte Carlo: N = 100",  col = "yellow", xlim= c(0,5))

#N=1000

N=1000
cene1000 <- c(1:M)
j=1
while (j<=M) {
  cene1000[j] <- monte(S0, u, d, R, Times, W, type, N)
  j=j+1
}

h3 <- hist(cene1000, xlab = "Premija", 
           main = "Monte Carlo: N = 1000",  col = "yellow", xlim= c(0,5))

# b)

# N=10

povpr <- sum(cene10/length(cene10)) # zelena črta
bin <- binomski(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put") # rdeča črtkana črta
# za standardni odklon:
standardni_odklon <- (sum(cene10^2)/length(cene10)-(sum(cene10/length(cene10)))^2)^(1/2)

h11 <- hist(cene10, xlab = "Premija", 
           main = "Monte Carlo: N = 10",  col = "yellow", xlim= c(0,5))
abline(v=povpr, untf = FALSE, col="green", lwd=2) # zelena črta
abline(v=bin, untf = FALSE, col="red", lwd=2, lty=3) # rdeča črtkana črta
arrows(x0=povpr, y0=0 , x1 = povpr+standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2) 
arrows(x0=povpr, y0=0 , x1 = povpr-standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2)
legend("topright", c("Monte Carlo", "analiza modela"), 
       col = c("green", "red"), lwd = 1.5, lty=c(1,3))

# N=100

povpr <- sum(cene100/length(cene100)) # zelena črta
bin <- binomski(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put") # rdeča črtkana črta
# za standardni odklon:
standardni_odklon <- (sum(cene100^2)/length(cene100)-(sum(cene100/length(cene100)))^2)^(1/2)

h21 <- hist(cene100, xlab = "Premija", 
            main = "Monte Carlo: N = 100",  col = "yellow", xlim= c(0,5))
abline(v=povpr, untf = FALSE, col="green", lwd=2) # zelena črta
abline(v=bin, untf = FALSE, col="red", lwd=2, lty=3) # rdeča črtkana črta
arrows(x0=povpr, y0=0 , x1 = povpr+standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2) 
arrows(x0=povpr, y0=0 , x1 = povpr-standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2)
legend("topright", c("Monte Carlo", "analiza modela"), 
       col = c("green", "red"), lwd = 1.5, lty=c(1,3))

# N=1000

povpr <- sum(cene1000/length(cene1000)) # zelena črta
bin <- binomski(S0 = 60, u =1.05, d = 0.95,R = 0.01, T = 15,W = rep(1, 16), type = "put") # rdeča črtkana črta
# za standardni odklon:
standardni_odklon <- (sum(cene1000^2)/length(cene1000)-(sum(cene1000/length(cene1000)))^2)^(1/2)

h31 <- hist(cene1000, xlab = "Premija", 
            main = "Monte Carlo: N = 1000",  col = "yellow", xlim= c(0,5))
abline(v=povpr, untf = FALSE, col="green", lwd=2) # zelena črta
abline(v=bin, untf = FALSE, col="red", lwd=2, lty=3) # rdeča črtkana črta
arrows(x0=povpr, y0=0 , x1 = povpr+standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2) 
arrows(x0=povpr, y0=0 , x1 = povpr-standardni_odklon, length = 0.1, angle = 30,
       col = "green", lwd=2)
legend("topright", c("Monte Carlo", "analiza modela"), 
       col = c("green", "red"), lwd = 1.5, lty=c(1,3))

