library("readxl")
data <- read_excel("C:/Users/Jacqu/Documents/Erhebungstechniken/Bericht/DatensatzFragebogenLJJY.xlsx")
data <- read_excel("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Fragebogen Projekt/DatensatzFragebogenLJJY.xlsx")
data <- DatensatzFragebogenLJJY
data$id <- c(seq(1, length(data$`[id]`)))
setwd("C:/Users/Jacqu/Documents/GitHub/Bericht-ET")


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


## Anteile Bib-Nutzung und Ersatzbewertung ##

#barplot(table(data$`Ersatzbew. (10)`))
#data$`Ersatzbew. (10)` <- factor(data$`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein"))
#barplot(table(data$`Ersatzbew. (10)`))

mosaicplot(~ factor(`UB(V)`, levels = c(1,0), labels = c("Ja", "Nein")) +
             factor(`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein")),
           data = data, ylab = "Ersatz ausreichend", xlab= "Bibliothek benutzt",
           main = "Ersatzbewertung")

## Wofür werden die Lernorte genutzt? ##

barplot(colSums(na.omit(data[,21:26])))

## Am Meisten für Abgaben, am wenigsten für Abschlussarbeiten

## Bewertung der allgemeinen Lernsituation ##

barplot(table(na.omit(data$`Bewertung (9)`)))

## 0 im Datensatz?

index <- which(data$`Bewertung (9)` == 0)
data$`Bewertung (9)`[index] <- NA

barplot(table(na.omit(data$`Bewertung (9)`)), names.arg = c("Sehr schlecht", "Schlecht", "Gut", "Sehr gut"))
## Am meisten gut

mean(na.omit(data$`Bewertung (9)`))
## [1] 2.602941

## Geschlechter ##

data$Geschlecht <- factor(data$Geschlecht, labels=c("Weiblich", "Männlich", "Divers"))
barplot(table(data$Geschlecht), main = "Verteilung der Geschlechter",
        col = c("red", "orange", "yellow"), xlab = "Geschlecht", ylab="Anzahl")

## Häufigkeit Lernortnutzung Vorher/Jetzt ##

boxplot(as.numeric(na.omit(data$`Frage 2 (Vorher)`)), as.numeric(na.omit(data$`Frage 2 (Jetzt)`)), names = c("Group1", "Group2"), main = "Boxplots nebeneinander")


## Sankey Plot

# install.packages("networkD3")
# install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("virdis")
#install.packages("patchwork")
#install.packages("hrbrthemes")
#install.packages("circlize")

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(dplyr)

sankeyNetwork(data[seq(5,20,2)], data[seq(6,)])

## Haeufigkeiten der Lernortnutzung (Vorher und Jetzt)
freq <- colSums(na.omit(data[,5:20]))

## Hauefigkeiten der Lernorte (Vorher)
sum_before <- colSums(na.omit(data[,seq(5,20, by=2)]))
# UB(V)     EFB(V)     CLS(V) Galerie(V)     Fak(V)     BCI(V)     Süd(V)    SRG (V) 
# 108   27          9         23         85          7          8         38 

## Hauefigkeiten der Lernorte (Jetzt)
sum_now <- colSums(na.omit(data[,seq(6,20, by=2)]))
# SB(J)     EFB(J)     CLS(J) Galerie(J)     Fak(J)     BCI(J)     Süd(J)     SRG(J) 
# 16         34         24         63         87          6          7         41 


## Spalten- und Zeilennamen definieren (alle möglichen Lernorte)
## Spalten = Jetzt, Zeilen = Vorher
spaltennamen <- c("SB", "EFB", "CLS", "Galerie", "Fak", "BCI", "Süd", "SRG")
zeilennamen <- c("UB", "EFB", "CLS", "Galerie", "Fak", "BCI", "Süd", "SRG")

## Test-Matrix = Änderung der Lernortnutzung
## Zählt also, wie viele Leute immer noch einen Lernort benutzten bzw. wie viele
## Leute von einem Lernort zu einem anderen gewechselt sind
test <- matrix(0, nrow = length(zeilennamen), ncol = length(spaltennamen))
rownames(test) <- zeilennamen
colnames(test) <- spaltennamen
test <- as.data.frame(test)

names <- names(freq)
orte_vorher <- names[seq(1,length(names), by = 2)]
orte_nachher <- names[seq(2,length(names), by = 2)]

## NAs entfernen
data[,5:20][is.na(data[,5:20])] <- 0

## Mithilfe der Schleifen wird (hoffentlich) gezählt, wie viele Leute einen
## Lernort vorher und jetzt angekreuzt haben bzw. wie viele Leute einen Lernort
## vorher angekreuzt haben und jetzt einen anderen (Doppelungen können vorkommen)

for(i in 1:length(zeilennamen)){
  for(j in 1:length(spaltennamen)){
    for(k in 1:length(data$`Frage 1`)){
      if((i == j)|(i == 1)){
        if((data[k,3+2*i][[1]] == 1) & (data[k,4+2*j][[1]] == 1)){
          test[i,j] <- test[i,j] + 1
        }
      }
      else if((data[k,3+2*i][[1]] == 1) & (data[k,4+2*j][[1]] == 1) & (data[k,3+2*j][[1]] == 0)){
        test[i,j] <- test[i,j] + 1
      }
    }
  }
}


## Die Lernorte die generell sehr wenig genutzt werden, werden entfernt
## -> Übersichtlichkeit
test <- subset(test, select = -BCI)
test <- subset(test, select = -Süd)
test <- test[c(T,T,T,T,T,F,F,T),]

## Sankey Diagramm wird erstellt

library(networkD3)

## "Langes" Format wird erstellt
data_long <- test %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)

colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

farben_blau_gruen = c(rgb(.5, .7, .4), rgb(.4, .6, .8), rgb(.8, .7, .4),
                      rgb(.8, .4, .4), rgb(.6, .4, .8) , rgb(.9, .6, .2) )
library(RColorBrewer)
farben_blau_gruen <- brewer.pal(6, "Set2") 
farben <- paste0('d3.scaleOrdinal().range(["', paste(coul3, collapse = '","'), '"])')

sankey_diagramm <- sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", nodePadding=17,
              sinksRight=FALSE, colourScale=farben, nodeWidth=45, fontSize=12.5)

sankey_diagramm
library(networkD3)
library(plotly)
library(webshot)
library(htmlwidgets)
htmlwidgets::saveWidget("dein_sankey_plot.html", last_plot())

dein_sankey_plot <- sankey_diagramm  # Hier deine Sankey-Daten einfügen

# Benutze onRender, um das Widget zu rendern, bevor es gespeichert wird
htmlwidgets::onRender(
  dein_sankey_plot,
  'function(el, x) { Plotly.plot(el, x); }'
)

saveWidget(sankey_diagramm, "dein_sankey_plot.html")


## Barplots der Lernortnutzung (Vorher und Jetzt)
par(mfrow = c(1, 2))
barplot(sum_before, las = 2, ylim = c(0,100))
barplot(sum_now, las = 2, ylim = c(0,100))

dev.off()


table(as.numeric(data$`Ersatzbew. (10)`))


### Tabellen ###

for(i in 28:57){
  print(table(data[,i]))
}
 
# Erreichbarkeit...28
# 1  2  3  4  5 
# 1  2  5 49 79 
# Barrierefreiheit...29
# 0  1  2  3  4  5 
# 5 27 12 61 18 13 
# Öffnungszeiten...30
# 0  1  2  3  4  5 
# 1  2  2 11 46 74 
# Platzgarantie...31
# 0  2  3  4  5 
# 1  1  7 34 93 
# Sicherheit...32
# 0  1  2  3  4  5 
# 2  2  4 47 40 41 
# Ruhe...33
# 1  2  3  4  5 
# 3  2 22 55 54 
# Stromversorgung...34
# 1  2  3  4  5 
# 4  2 14 33 83 
# Gruppenräume...35
# 0  1  2  3  4  5 
# 1 13 11 34 47 30 
# Pausebreiche...36
# 1  2  3  4  5 
# 20 34 47 27  8 
# Computer...37
# 1  2  3  4  5 
# 53 36 28 11  8 
# Erreichbarkeit...38
# 0  1  2  3  4  5 
# 1 64 59 10  1  1 
# Barrierefreiheit...39
# 0  1  2  3  4  5 
# 16 22 58 32  7  1 
# Öffnungszeiten...40
# 0  1  2  3  4  5  6 
# 1 55 48 23  6  2  1 
# Platzgarantie...41
# 0  1  2  3  4  5  6 
# 1 21 40 35 26 11  2 
# Sicherheit...42
# 0  1  2  3  4  5 
# 5 44 64 21  1  1 
# Ruhe...43
# 0  1  2  3  4  5 
# 1 32 62 33  7  1 
# Stromversorgung...44
# 0  1  2  3  4  5  6 
# 3 32 47 38  7  8  1 
# Gruppenräume...45
# 0  1  2  3  4  5  6 
# 3 17 46 52 12  4  2 
# Pausebreiche...46
# 0  1  2  3  4  5  6 
# 4 15 43 45 22  5  2 
# Computer...47
# 0  1  2  3  4  5  6 
# 9 24 48 34 14  3  4 
# Erreichbarkeit...48
# 0  1  2  3  4  5  6 
# 1 24 51 38 14  4  4 
# Barrierefreiheit...49
# 0  1  2  3  4  5  6 
# 16  9 54 40  9  7  1 
# Öffnungszeiten...50
# 0  1  2  3  4  5  6 
# 1 16 45 41 20 10  3 
# Platzgarantie...51
# 0  1  2  3  4  5  6 
# 2 11 19 31 33 30 10 
# Sicherheit...52
# 0  1  2  3  4  5 
# 5 35 61 29  5  1 
# Ruhe...53
# 0  1  2  3  4  5  6 
# 1 15 46 47 16  7  4 
# Stromversorgung...54
# 0  1  2  3  4  5  6 
# 2 21 42 37 17 14  3 
# Gruppenräume...55
# 0  1  2  3  4  5  6 
# 4 12 38 52 17 10  3 
# Pausenbereiche
# 0  1  2  3  4  5 
# 5 13 42 45 21 10 
# Computer...57
# 0  1  2  3  4  5  6 
# 8 16 26 43 24 13  6 

### Score ###

data[,28:57][is.na(data[,28:57])] <- 0

## Berechnung Score Vorher

ScoreVorher <- numeric(141)
gesum <- numeric(141)
for(j in 1:141) {
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

ScoreNachher <- numeric(141)
for(j in 1:141) {
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

par(mfrow = c(1, 2))
boxplot(ScoreVorher, ylim = c(0,5))
boxplot(ScoreNachher, ylim = c(0,5))
dev.off()


setwd("C:/Users/Jacqu/Documents/GitHub/Bericht-ET")

### Boxplot Frage 2 ###


#library(tikzDevice)
#tikz('Frage2.tex', width=6,height=3.5)
boxplot(as.numeric(na.omit(data$`Frage 2 (Vorher)`)), as.numeric(na.omit(data$`Frage 2 (Jetzt)`)), names = c("Vorher", "Nachher"), main = "Durschnittliche Lernzeit in Stunden pro Woche")
dev.off()

### Sankey Diagramm ###
#setwd("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Bericht-ET")

library(tikzDevice)
tikz('Sankey.tex', width=6,height=3.5)
sankey_diagramm
dev.off()


### Lernorte Vorher Nachher ###

library(RColorBrewer)
sum_before_pl <- data.frame(Names = factor(c("UB","EFB","CLS", "Galerie", "Fakultät","BCI", "Süd Campus", "SRG"), levels = c("UB","EFB","CLS", "Galerie", "Fakultät","BCI", "Süd Campus", "SRG")), Werte = sum_before[1:8])
sum_now_pl <- data.frame(Names = factor(c("Sebrath","EFB","CLS", "Galerie", "Fakultät","BCI", "Süd Campus", "SRG"), levels = c("Sebrath","EFB","CLS", "Galerie", "Fakultät","BCI", "Süd Campus", "SRG")), Werte = sum_now[1:8])

coul1 <- c("#1abc9c", "#e67e22", "#2c3e50", "#95a5a6", "#f1c40f", "#8e44ad", "#d35400", "#27ae60")
coul2 <- c("#ff5733", "#e67e22", "#2c3e50", "#95a5a6", "#f1c40f", "#8e44ad", "#d35400", "#27ae60")

library(tikzDevice)
tikz('Lernorte.tex', width=6,height=3.5)
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2) + 1)
bp1 <- barplot(sum_before_pl$Werte ~ sum_before_pl$Names, las = 1, ylim = c(0,100), col = coul1, xaxt = "n", xlab = "Lernorte", ylab = "Anzahl",  main="Lernortnutzung vorher", cex.names = 0.75, cex.lab = 1, cex.axis = 1)
text(
  x = bp,
  y = par("usr")[3] - 5,
  labels = sum_before_pl$Names,
  xpd = NA,
  srt = 35,
  adj = 0.98,
  cex = 0.87
)
bp2 <- barplot(sum_now_pl$Werte ~ sum_now_pl$Names, las = 1, ylim = c(0,100), col = coul2, xaxt = "n", xlab = "Lernorte", ylab = "Anzahl",  main="Lernortnutzung jetzt", cex.names = 0.75, cex.lab = 1, cex.axis = 1)
text(
  x = bp,
  y = par("usr")[3] -5,
  labels = sum_now_pl$Names,
  xpd = NA,
  srt = 35,
  adj = 0.98,
  cex = 0.87
)
dev.off()

## Andere Achsen ##

sum_before_pl$Names <- c("UB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd Campus", "SRG")
sum_now_pl$Names <- c("SB", "EFB", "CLS", "Galerie", "Fakultät", "BCI", "Süd Campus", "SRG")
sum_before_pl$Names <- factor(sum_before_pl$Names, levels =  c("UB", "Fakultät", "EFB", "CLS", "Galerie", "BCI", "Süd Campus", "SRG"))
sum_now_pl$Names <- factor(sum_now_pl$Names, levels =  c("SB", "Fakultät","EFB","CLS", "Galerie", "BCI", "Süd Campus", "SRG"))

palette1 <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00"))
coul1 <- palette1(8)
coul1 <- c("#E41A1C" , "#46A169" ,  "#FF7F00" , "#99445E" ,"#3D8C98" ,"navyblue","rosybrown2" , "#B2931F")
coul2 <- c("#FFFF00",  "#46A169" ,  "#FF7F00" , "#99445E" ,"#3D8C98" ,"navyblue","rosybrown2" , "#B2931F")
coul3 <- c("#99445E", "#E41A1C", "#FF7F00", "#FFFF00" ,"#46A169", "#4F6FA1")
coul4 <- c("#46A169", "#FFFF00")

#coul2 <- c("#FFFF00",  "#46A169" ,  "#FF7F00" , "#99445E" ,"#3D8C98" ,"#4F6FA1", "#66A83F" , "#B2931F")
# "springgreen3", "royalblue2"

library(tikzDevice)
tikz('Lernorte.tex', width=6,height=3.5)
par(mfrow = c(1, 2))
par(mgp = c(5.5, 1, 0))
par(mar = c(6.5, 4, 4, 2) + c(0, 1, 0, 0))
bp1 <- barplot(sum_before_pl$Werte ~ sum_before_pl$Names, las = 2, ylim = c(0,100), col = coul1, xlab = "Lernorte", ylab = "Anzahl",  main="Lernortnutzung Vorher", cex.names = 0.98, cex.lab = 1, cex.axis = 1)
bp2 <- barplot(sum_now_pl$Werte ~ sum_now_pl$Names, las = 2, ylim = c(0,100), col = coul2, xlab = "Lernorte", ylab = "Anzahl",  main="Lernortnutzung Jetzt", cex.names = 0.98, cex.lab = 1, cex.axis = 1)
dev.off()

library("ggplot2")
library("ggsci")

### Wofür Lernorte nutzen ###

farbPalette <- colorRampPalette(c("orange", "darkblue"))
coul2 <- brewer.pal(6, "Set2") 

library(tikzDevice)
tikz('Nutzung.tex', width=6,height=3.5)
barplot(colSums(na.omit(data[,21:26])), col = coul2)
dev.off()


means <- numeric(19)
for(i in 38:57){
  means[i-37] <- round(mean(na.omit(as.numeric(data[,i][[1]]))),2)
  print(colnames(data)[i])
  print(round(mean(na.omit(as.numeric(data[,i][[1]]))),2))
}


# [1] "Erreichbarkeit...38"
# [1] 1.64
# [1] "Barrierefreiheit...39"
# [1] 2.22
# [1] "Öffnungszeiten...40"
# [1] 1.93
# [1] "Platzgarantie...41"
# [1] 2.79
# [1] "Sicherheit...42"
# [1] 1.86
# [1] "Ruhe...43"
# [1] 2.13
# [1] "Stromversorgung...44"
# [1] 2.36
# [1] "Gruppenräume...45"
# [1] 2.59
# [1] "Pausebreiche...46"
# [1] 2.73
# [1] "Computer...47"
# [1] 2.5
# [1] "Erreichbarkeit...48"
# [1] 2.52
# [1] "Barrierefreiheit...49"
# [1] 2.62
# [1] "Öffnungszeiten...50"
# [1] 2.79
# [1] "Platzgarantie...51"
# [1] 3.61
# [1] "Sicherheit...52"
# [1] 2.05
# [1] "Ruhe...53"
# [1] 2.75
# [1] "Stromversorgung...54"
# [1] 2.78
# [1] "Gruppenräume...55"
# [1] 2.88
# [1] "Pausenbereiche"
# [1] 2.79
# [1] "Computer...57"
# [1] 3.08

(diff <- means[1:10] - means[11:20])

median <- numeric(20)
for(i in 38:57){
  median[i-37] <- median(na.omit(as.numeric(data[,i][[1]])))
}

(diff2 <- median[1:10] - median[11:20])

(median(na.omit(as.numeric(na.omit(data$`Frage 2 (Vorher)`))))) ## 10
(median(na.omit(as.numeric(na.omit(data$`Frage 2 (Jetzt)`))))) ## 7


### Mosaic-Plots ###

library(tikzDevice)
tikz('Mosaic.tex', width=4.3,height=3.8)
par(mfrow = c(1, 2))
mosaicplot(~ factor(`UB(V)`, levels = c(1,0), labels = c("Genutzt", "Nicht genutzt")) +
             factor(`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein")),
           data = data, ylab = "Ersatz ausreichend", xlab= "Universitäts-Bibiliothek", col = c("springgreen3", "royalblue2"), main = " ")
text(c(-1.28,0.8), labels = c("10"), col = "white", cex = 1.2)
mosaicplot(~ factor(`SB(J)`, levels = c(1,0), labels = c("Genutzt", "Nicht genutzt")) +
             factor(`Ersatzbew. (10)`, levels = c(1,0), labels = c("Ja", "Nein")),
           data = data, ylab = "Ersatz ausreichend", xlab= "Nutzung der Sebrath-Bibliothek",
           col = c("springgreen3", "royalblue2"), main = "Bewertung im Kontext der Sebrath Bibliothek")
text(c(0.5, 1.5), c(0.5, 1.5), labels = c("10", "20"), col = "white", cex = 1.2)
mtext(expression(bold("Bewertung im Kontext der Sebrath Bibliothek")), side = 3, line = -2.5, outer = TRUE)
dev.off()
