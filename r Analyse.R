library(readxl)
DatensatzFragebogenLJJY <- read_excel("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Fragebogen Projekt/DatensatzFragebogenLJJY.xlsx")
data <- DatensatzFragebogenLJJY

library("tikzDevice")

# 3: Stichprobe und Datensatz
# Erhebung: Aushaendigen von Boegen und online Ausfuellen 
# Ort: Campus, Lernorte der TU (Fokus: Galerie, Mathetower)
# Zeitraum: 13.11. bis 26.11.2023
# Stichprobe: 141 eingereichte Frageboegen, aber
nrow(data) 
drop = c(41, 69) 
data = data[-drop, ]
rm(drop)
nrow(data) # 139 verwertbare Daten
ncol(data)
# davon Nicht-TU-Studenten und Erstis ebenfalls loeschen
data = data[-which(data[ ,64] == 0), ]
nrow(data) # also nur noch 138 verwertbare
# Variablen mit Skalenniveau: gleiche Typen gruppieren
# 68 Variablen 
# --- 

#table(data$Geschlecht)
#hist(table(data$Ruhe...33))
#hist(data$Ruhe...33)

#data$Ruhe...53 <-factor(data$Ruhe...53,levels = c("1","2","3","4","5","6"), 
       #labels = c("sehr gut","gut","befriedigend",
                  #"ausreichend", "mangelhaft","ungenügend"))
#data$Ruhe...43 <-factor(data$Ruhe...43,levels = c("1","2","3","4","5","6"), 
                        #labels = c("sehr gut","gut","befried.",
                                  # "ausr.", "mangel.","ungenüg."))
#par(mfrow=c(1,2))
#barplot(table(data$Ruhe...43)/length(data$Ruhe...43), main="Vorher",
#        xlab = "Aspekt Ruhe", ylim = c(0.0,0.4))
#barplot(table(data$Ruhe...53)/length(data$Ruhe...53), main="Nachher",
#        xlab = "Aspekt Ruhe", ylim = c(0,0.4))
#par(mfrow=c(1,1))

sd(table(as.numeric(data$Fakultät)))



## Nas Raus
data[,28:57][is.na(data[,28:57])] <- 0

## Berechnung Score Vorher

ScoreVorher <- numeric(nrow(data))
gesum <- numeric(nrow(data))
for(j in 1:nrow(data)) {
  for (i in 0:9) {
    ScoreVorher[j] <- ScoreVorher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 38 + i]))
    gesum[j] <- gesum[j] + as.numeric(data[j, 28 + i])
  }
  if(gesum[j] != 0){
    ScoreVorher[j] <- ScoreVorher[j]/gesum[j]
  }
  else{
    ScoreVorher[j] <- ScoreVorher[j]
  }
}

## Berechnung Score Nachher

ScoreNachher <- numeric(nrow(data))
for(j in 1:nrow(data)) {
  for (i in 0:9) {
    ScoreNachher[j] <- ScoreNachher[j] + (as.numeric(data[j, 28 + i]) * as.numeric(data[j, 48 + i]))
  }
  if(gesum[j] != 0){
  ScoreNachher[j] <- ScoreNachher[j]/gesum[j]
  }
  else{
    ScoreNachher[j] <- ScoreNachher[j]
  }
}
ScoreNachher[which(ScoreNachher < 1)] <- NA
ScoreVorher[which(ScoreVorher < 1)] <- NA
meanV <- mean(ScoreVorher, na.rm = T)  #2.19936
meanN <- mean(ScoreNachher, na.rm = T) #2.727848
varV <- var(ScoreVorher, na.rm = T)  #0.3812979
varN <- var(ScoreNachher, na.rm = T) #0.5709283
ScoreNachher[which(ScoreNachher < 1)] <- NA
ScoreVorher[which(ScoreVorher < 1)] <- NA
ScoreVorher
Scorediff <- ScoreVorher - ScoreNachher
data$ScoreV <- ScoreVorher
data$ScoreN <- ScoreNachher
data$ScoreDiff <- Scorediff

## Plots
#Bachelor <- subset(data,data$Studium==1)
#Master <- subset(data,data$Studium==2)

#par(mfrow=c(1,2))
#plot(data$ScoreV, col=0, ylim = c(6,1), main="Score Vorher", ylab="Score Vorher")
#points(1:19,Master$ScoreV, ylim = c(6,1), col="blue", pch=16)
#points(20:134,Bachelor$ScoreV, ylim = c(6,1), col="orange",pch=16)
#abline(h=meanV, col="red", lwd=2)
#plot(data$ScoreN, col=0,ylim = c(6,1),main="Score Nachher", ylab="Score Nachher")
#points(1:19,Master$ScoreN, ylim = c(6,1), col="blue", pch=16)
#points(20:137,Bachelor$ScoreN, ylim = c(6,1), col="orange",pch=16)
#abline(h=meanN, col="red", lwd=2)
#par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(data$ScoreV, col=1, ylim = c(1,6), main="Score Vorher", ylab="Score Vorher")
abline(h=meanV, col="red", lwd=2)
plot(data$ScoreN, col=1,ylim = c(1,6),main="Score Nachher", ylab="Score Nachher")
abline(h=meanN, col="red", lwd=2)
par(mfrow=c(1,1))


#Vorher und Nacher
par(mfrow=c(1,1)) 
plot(ScoreVorher, col="orange", pch=19, cex=1, ylab="Score")
points(1:nrow(data),ScoreNachher, col="darkblue",pch=19 )
#abline(h=meanN, col="darkblue")
#abline(h=meanV, col="orange")

table(as.numeric(data$`Ersatzbew. (10)`))

#Scorediff Allg
Scorediff <- ScoreVorher-ScoreNachher
plot(Scorediff, ylim = c(-3,3), xaxt="n", pch=1)
abline(h=0)


#Scorediff in Abhängigkeit von Fak.
plot(data$ScoreDiff ~ data$Fakultät, ylim = c(-3,3), xaxt="n", pch=1)
abline(h=0)
#Scorediff in Abhängigkeit von Fahrzeit
plot(data$ScoreDiff ~ data$Fahrzeit, ylim = c(-3,3), pch=1, col="darkblue", xlim = c(0,110))
abline(h=0, lwd=2)

#Scorediff in Abhängigkeit von Lernzeit Jetzt
plot(data$ScoreDiff ~ data$`Frage 2 (Jetzt)`, ylim = c(-3,3), pch=1)
abline
#Scorediff in Abhängigkeit der Geschlechter
boxplot(data$ScoreDiff ~ data$Geschlecht, ylim = c(-3,3), pch=1)
#abline(h=0)

#Scorediff nach Master/Bachelor
plot(data$ScoreDiff ~ data$Studium, ylim = c(-3,3), pch=1)
abline(h=0)
boxplot(data$ScoreDiff ~ data$Studium, ylim = c(-3,3), pch=1)


plot(ScoreVorher~ data$Fakultät, col="orange", pch=19, cex=1, ylab="Score")
points(data$Fakultät,ScoreNachher, col="darkblue",pch=19 )

mean(as.numeric(data$`Frage 2 (Vorher)`), na.rm = T)
mean(as.numeric(data$`Frage 2 (Jetzt)`), na.rm = T)



#Lernorte vorher
seq1 <- seq(from=5, to=20, by=2)
vorherOrt <- numeric(8)
w <- 0
for(i in seq1){
  w <- w+1
  vorherOrt[w] <- sum(data[i], na.rm=T)
}
vorherOrtdf <- rbind(vorherOrt,names=colnames(data[seq1]))
barplot(as.numeric(vorherOrtdf[1,])~vorherOrtdf[2,], 
        xlab="Lernorte", ylab = "absolute Häufigkeit")

#Lernorte jetzt
seq2 <- seq(from=6, to=20, by=2)
jetztOrt <- numeric(8)
w <- 0
for(i in seq2){
  w <- w+1
  jetztOrt[w] <- sum(data[i], na.rm=T)
}
jetztOrtdf <- rbind(jetztOrt,names=colnames(data[seq2]))
barplot(as.numeric(jetztOrtdf[1,])~jetztOrtdf[2,], xlab="Lernorte",
        ylab = "absolute Häufigkeit")

par(mfrow=c(1,2))
barplot(as.numeric(vorherOrtdf[1,])~vorherOrtdf[2,],
        xlab="Lernorte", ylab = "absolute Häufigkeit",
        main="Vorher", ylim=c(0,100))
barplot(as.numeric(jetztOrtdf[1,])~jetztOrtdf[2,],
        xlab="Lernorte", ylab = "absolute Häufigkeit",
        main="Jetzt", ylim=c(0,100))
par(mfrow=c(1,1))

#Differenz
indi <- c(2,3,4,5,6,7,8)
diffOrtc <- jetztOrt[indi]-vorherOrt[indi]
diffOrt <- rbind(diffOrtc,names=c("EFB","CLS","Galerie","Fak","BCI","Süd","SRG"))
col1 <- c("red",rep("green3",5),"red")
barplot(as.numeric(diffOrt[1,])~diffOrt[2,], xlab="Lernorte",
        ylab = "absolute Häufigkeit", ylim=c(-10,40), col=col1)
abline(h=0)

#Nutzungsgrund

seq3 <- seq(from=21, to=26)
w <- 0
grundc <- numeric(6)
for(i in seq3){
  w = w + 1
  grundc[w] <- sum(data[i], na.rm = T)
}
Grund <- rbind(grundc,names=colnames(data[seq3]))
Grund[2,1] <- "Zeitüberbrückung"
barplot(as.numeric(Grund[1,])~Grund[2,], xlab="Grund",
        ylab="Absolute Häufigkeiten")

#Boxplot Ersatzbewertung-Fahrzeit
boxplot(as.numeric(data$Fahrzeit)~as.numeric(data$`Ersatzbew. (10)`),  na.rm=T)

#Erhebungsorte
barplot(table(data$Erhebungsort))
table(data$Erhebungsort)

#mittelwerte wichtigkeit
Mittelwerte_ <- numeric(10)
seq4 <- seq(from=28, to=37)
w <- 0
for(f in seq4){
  w <- w+1
  Mittelwerte_[w] <- mean(as.numeric(data[[f]]), na.rm = T)
}

Namen <-c("Erreichbarkeit", "Barrierfreiheit", "Öffnungszeiten",
          "Platzgarantie", "Sicherheit", "Ruhe", "Stromversorgung",
          "Gruppenräume", "Pausenbereiche", "Computer")

#Mittelwerte <- rbind(Mittelwerte_,names=colnames(data[seq4]))
Mittelwerte <- rbind(Mittelwerte_,Namen)
barplot((as.numeric(Mittelwerte[1,])-1)~(Mittelwerte[2,]), ylim=c(0,4),las=2, cex.names=0.6, xlab="", ylab = "Mittelwerte der Wichtigkeit")
max(Mittelwerte)

cor(data$ScoreN,data$`Bewertung (9)`,use="pairwise.complete.obs" )
cor(data$`Bewertung (9)`,as.numeric(data$`Frage 2 (Jetzt)`),use="pairwise.complete.obs")
cor(data$`Bewertung (9)`, data$`SB(J)`,use="pairwise.complete.obs")

## Plots

tikz("ScoreVN.tex", width=7, height = 3.5)
setwd("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Bericht-ET")
indizes <- which(data$`UB(V)` == T)
Farben <- c(rep("blue", 138))
Farben[indizes] <- "orange"
Farben
par(mfrow=c(1,2))
plot(data$ScoreV, ylim = c(6,1), main="Score Vorher", ylab="Score Vorher", col=Farben,pch=16, xaxt="n", xlab="")
abline(h=meanV, col="red", lwd=3)
legend(3.5,5,
       legend = c("arith. Mittel", "UB genutzt", "UB nicht genutzt"),
       bty = "n", 
       lty = c(1, NA, NA),
       pch = c(NA, 16, 16),
       col = c("red", "orange", "blue"),
       lwd = 1,
       cex = 0.6)
plot(data$ScoreN,ylim = c(6,1),main="Score Nachher", ylab="Score Nachher", col=Farben, pch=16, xaxt="n", xlab="")
abline(h=meanN, col="red", lwd=3)
legend(3.5,5,
       legend = c("arith. Mittel", "UB genutzt", "UB nicht genutzt"),
       bty = "n", 
       lty = c(1, NA, NA),
       pch = c(NA, 16, 16),
       col = c("red", "orange", "blue"),
       lwd = 1,
       cex = 0.6)
par(mfrow=c(1,1))
dev.off()
getwd()

plot(data$ScoreN~data$ScoreV)
abline(0,1)
abline(reg$coefficients)
cor(data$ScoreN,data$ScoreV, use="pairwise.complete.obs")
reg <- lm(data$ScoreN~data$ScoreV)
summary(reg)

mean(data$ScoreN, na.rm=T)
mean(data$ScoreV, na.rm = T)
median(data$ScoreN, na.rm=T)
median(data$ScoreV, na.rm=T)
var(data$ScoreN, na.rm = T)
var(data$ScoreV, na.rm = T)
sd(data$ScoreN, na.rm = T)
sd(data$ScoreV, na.rm = T)
min(data$ScoreN, na.rm=T)
min(data$ScoreN, na.rm=T)

summary(data$ScoreN, na.rm=T)
summary(data$ScoreV, na.rm=T)
length(data$ScoreV)
