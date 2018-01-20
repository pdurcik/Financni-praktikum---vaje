library(readr)

# 1. naloga
cene_srebra <- read_csv("LBMA-SILVER.csv", locale = locale(encoding = "cp1252"))
casovna_vrsta <- ts(cene_srebra[c(150:1), 4])
graf_srebra <- plot(casovna_vrsta, main = "Srebro", ylab="EUR", xlim = c(0, 150))
points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)

# 2. naloga

# a) Funkcija, ki priredi zglajene vrednosti

G <- function(vrsta, k){
  glajene_vrednosti <- c()
  T = length(vrsta)
  for (i in (k):(T)){
    glajene_vrednosti[i] <- sum(vrsta[(i - k + 1):(i)])/k
  }
  
  return(glajene_vrednosti)
}

# b)

zglajena_vrsta_k <- function(k) {
  zglajena_vrsta <- G(casovna_vrsta, k)
  napoved <- tail(zglajena_vrsta, 1) #napoved za naslednji dan
  zglajena_vrsta <- c(zglajena_vrsta, rep(napoved, 10))
  zglajena_vrsta <- ts(zglajena_vrsta)
  return(zglajena_vrsta)
}


# c) 
graf2 <- ts.plot(casovna_vrsta, zglajena_vrsta_k(7), main = "Drseče povprečje", ylab="EUR", xlim = c(0, 150), col = c("black", "red"), lwd=c(1,2))
points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)

# d) Srednja kvadratna napaka

kvadratna_napaka_k <- function(k) {
  skrajsana_casovna_vrsta <- casovna_vrsta[(k + 1) : length(casovna_vrsta)]
  skrajsana_zglajena_vrsta <- zglajena_vrsta_k(7)[(k + 1): (length(zglajena_vrsta_k(7))-10)]
  razlika_na_2 <- (skrajsana_zglajena_vrsta - skrajsana_casovna_vrsta)^2
  MSE <- mean(razlika_na_2)
  return(MSE)
}

######

par(mfrow=c(2,2))
for (i in (c(7,14,30))){
  if (i==7){
    g1 <- ts.plot(casovna_vrsta, zglajena_vrsta_k(7), main = "Drseče povprečje reda 7", ylab="EUR", xlim = c(0, 150), col = c("black", "red"), lwd=c(1,2))
    points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)
  }
  else if (i==14){
    g2 <- ts.plot(casovna_vrsta, zglajena_vrsta_k(14), main = "Drseče povprečje reda 14", ylab="EUR", xlim = c(0, 150), col = c("black", "red"), lwd=c(1,2))
    points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)
  }
  else if (i==30){
    g3 <- ts.plot(casovna_vrsta, zglajena_vrsta_k(30), main = "Drseče povprečje reda 30", ylab="EUR", xlim = c(0, 150), col = c("black", "red"), lwd=c(1,2))
    points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)
  }
}
par(mfrow=c(1,1))
# manjkajo ti trije grafi na eni sliki!!!!

kvadratna_napaka_k(7)
kvadratna_napaka_k(14)
kvadratna_napaka_k(30)


# 3. naloga

# a)

EG <- function(vrsta,alpha){
  l <- c()
  l[1] <- vrsta[1]
  for (i in (2:length(vrsta))){
    l[i] <- alpha*vrsta[i] + (1-alpha)*l[i-1]
  }
  return(l)
}

# b)

e_glajenje <- function(alpha){
  l_zglajena_vrsta <- EG(casovna_vrsta,alpha)
  e_glajene_vrednosti <- c()
  # for zanka da naredi y-streha_t+1 = l_t
  for (i in 1:length(l_zglajena_vrsta)){
    e_glajene_vrednosti[i+1] <- l_zglajena_vrsta[i]
  }
  napoved <- tail(e_glajene_vrednosti, 1) #napoved za naslednji dan
  e_glajene_vrednosti <- c(e_glajene_vrednosti, rep(napoved, 10))
  e_glajene_vrednosti <- ts(e_glajene_vrednosti)
  
  
  return(e_glajene_vrednosti)
}


graf3 <- ts.plot(casovna_vrsta, e_glajenje(0.25), main = "Eksponentno glajenje", ylab="EUR", xlim = c(0, 150), col = c("black", "red"), lwd=c(1,2))
points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)

# c)
f_za_alpha <- function(a){
  mean((EG(casovna_vrsta,a)-casovna_vrsta)^2)
}
alpha_zvezdica <- optimize(f_za_alpha,c(0,1))

# d)

graf3 <- ts.plot(casovna_vrsta, e_glajenje(as.numeric(alpha_zvezdica[1])), main = "Eksponentno glajenje, minimalen MSE", 
                 ylab="EUR", xlim = c(0, 150), col = c("black", "green"), lwd=c(1,2))
points(x = time(casovna_vrsta), y = casovna_vrsta, type = "p", col= "black", pch = 20)