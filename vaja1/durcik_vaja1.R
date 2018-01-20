#Primoz Durcik, Financni praktikum, 1. naloga

#1. Uvoz podatkov in dinamika obrestnih mer v času
leto_2008 <- read.table("hist_EURIBOR_2008.txt")
leto_2009 <- read.table("hist_EURIBOR_2009.txt")
leto_2010 <- read.table("hist_EURIBOR_2010.txt")

#stolpce (prve delovne dni v mesecu) sem izbral s pomočjo excela
datumi_2008<-colnames(leto_2008[c(1,23,44,63,85,106,127,150,171,193,216,236)])
datumi_2009<-colnames(leto_2009[c(1,22,42,64,84,104,126,149,170,192,214,235)])
datumi_2010<-colnames(leto_2010[c(1,21,41,64,84,105,127,149,171,193,214,236)])

tabela <- t(cbind(leto_2008[datumi_2008], leto_2009[datumi_2009], leto_2010[datumi_2010]))

#pri nalogi (1c) sem si izbral 3-mesečno in 9-mesečno obrestno mero
#stolpca 6 in 12

stolpca2 <- tabela[,c(6,12)]

m3 <- ts(data=stolpca2[,1], start=c(2008,1), frequency = 12)
m9 <- ts(data=stolpca2[,2], start=c(2008,1), frequency = 12)

graf1 <- ts.plot(m3,m9, gpars = list(ylim=c(0,5.5), xlab="Čas", ylab="%", col=c("green","red"), main="Euribor"))
legend("topright", c("3m","9m"),col=c("green","red"),lwd=1.5)

#2. Oblika obrestne krivulje
#izbral sem si datume 3.3.2008, 3.11.2008, 3.8.2009

stevilka <- c(0.25,0.5,0.75,1:12)
datumi3 <- t(tabela[c(11,3,20),])
datumi3 <- cbind(datumi3,stevilka)

graf21 <- plot(y=datumi3[,c(1)], x=stevilka, ylim=c(min(0),max(5.5)),xlab="Dospetje [mesec]", ylab="%", col="red", main="Casovna struktura Euribor")
lines(datumi3[,c(2)], x=stevilka,col="blue", type="o",pch = 16, text(10,4,"3.3.2008", col="blue"))
#text(x koordinata, y koordinata, "napis", ...)
lines(datumi3[,c(1)], x=stevilka,col="red", type="o",pch = 16, text(10,5.1,"3.11.2008", col="red"))
lines(datumi3[,c(3)], x=stevilka,col="green", type="o",pch = 16, text(10,1.6,"3.8.2009", col="green"))

# Pri datumih 3.11.2008 in 3.8.2009 imamo (rahlo) naraščajočo kirvuljo; na dolgi rok imamo višjo 
# donosnost kot na kratek rok. Rečemo ji tudi normalna krivulja donosnosti, ker se oblikuje 
# v normalnih tržnih pogojih (to je, ko trg pričakuje večjo donosnost za večje tveganje). 
#
# Pri datumu 3.3.2008 pa je skoraj ravna krivulja, torej je malo razlik med kratkoročnimi 
# in dolgoročnimi obrestnimi merami. Ravna krivulja je tipičen pokazatelj investitorjeve 
# zaskrbljenosti glede gospodarskega stanja (na primer pričakovanje zmanjšanja inflacije 
# na trgu). 

#3. Hipoteza pričakovanj trga

# a) zadnji stolpec v tabeli "tabela z napovedjo" je terminska obrestna mera tipa 3x9
napoved6m <- c(0) # da bom v ta stolpec shranil izracunano terminsko obrestno mero
tabela_z_napodvedjo <- tabela
tabela_z_napodvedjo <- cbind(tabela_z_napodvedjo,napoved6m)

i=1
while (i<37) {
  if (i<4) {
    tabela_z_napodvedjo[i,16] <- NA # prve stiri so pri meni NA
    } else{
      tabela_z_napodvedjo[i,16] <- (1/(6/12))*(((1+(9/12)*tabela[i-3,12])/(1+(3/12)*tabela[i-3,6]))-1) 
    }
  i<-i+1
  }
  
# b)
skrcena_tabela <- tabela_z_napodvedjo[,c(0,9,16)] 

#c)
vsa_leta <- as.data.frame(skrcena_tabela)
colnames(vsa_leta)[colnames(vsa_leta)=="6m"] <- "opazovano6m"

l2008 <- vsa_leta[c(1:12),] #leto 2008
l2009 <- vsa_leta[c(13:24),] #leto 2009
l2010 <- vsa_leta[c(25:36),] #leto 2010

linerana_regresija <- plot(vsa_leta,type = "n",xlab="Napoved", ylab="Opazovano",
         ylim=c(0,5.5), xlim=c(0,5.5), main="6m Euribor 2008-2010")
points(x=l2008[,2], y = l2008[,1], type = "p", col="red",pch = 16)
points(x=l2009[,2], y = l2009[,1], type = "p", col="blue",pch = 16)
points(x=l2010[,2], y = l2010[,1], type = "p", col="green",pch = 16)
fit <- lm(opazovano6m~napoved6m, data=vsa_leta) # to uporabim v naslednji vrstici
abline(fit) # to nam da regresijsko premico
abline(a=0,b=1, lty=2)  # to nam da simetralo kvadranta
legend("topleft", c("2008","2009","2010"),col=c("red","blue","green"),lwd=1.5, 
       pch=c(16,16,16), lty=c(NA,NA,NA))


# d)
#regresija za leto 2008
linerana_regresija_2008 <- plot(l2008,type = "n",xlab="Napoved", ylab="Opazovano",
                           ylim=c(2,5.5), xlim=c(2,5.5), main="6m Euribor 2008")
points(x=l2008[,2], y = l2008[,1], type = "p", col="red",pch = 16)
fit <- lm(opazovano6m~napoved6m, data=l2008)
abline(fit)
abline(a=0,b=1, lty=2) 

#regresija za leto 2009
linerana_regresija_2009 <- plot(l2009,type = "n",xlab="Napoved", ylab="Opazovano",
                                ylim=c(0.8,3.3), xlim=c(0.8,3.3), main="6m Euribor 2009")
points(x=l2009[,2], y = l2009[,1], type = "p", col="blue",pch = 16)
fit <- lm(opazovano6m~napoved6m, data=l2009)
abline(fit)
abline(a=0,b=1, lty=2) 

#regresija za leto 2010
linerana_regresija_2010 <- plot(l2010,type = "n",xlab="Napoved", ylab="Opazovano",
                                ylim=c(0.9,1.4), xlim=c(0.9,1.4), main="6m Euribor 2010")
points(x=l2010[,2], y = l2010[,1], type = "p", col="green",pch = 16)
fit <- lm(opazovano6m~napoved6m, data=l2010)
abline(fit)
abline(a=0,b=1, lty=2) 

#e)
# Da bi hipoteza pričakovanj trga veljala, bi morale regresijske premice v teh grafih biti 
# blizu simetrale lihih kvadrantov. Z mojimi podatki se je to skoraj zgodilo le v letu 2009, 
# v ostalih letih pa je prišlo do velikih odstopanj. Tako da empirični podatki ne potrjujejo 
# hipoteze. Na tej točki pa velja omeniti tudi, da je moje obdobje (2008-2010) ravno obdobje,
# ko je prišlo do svetovne gospodarske krize.