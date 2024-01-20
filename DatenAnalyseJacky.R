library("readxl")
data <- read_excel("C:/Users/Jacqu/Documents/Erhebungstechniken/Bericht/DatensatzFragebogenLJJY.xlsx")
data <- read_excel("C:/Users/yanni/Desktop/Studium/3. Semester/Erhebungstechniken/Fragebogen Projekt/DatensatzFragebogenLJJY.xlsx")
data <- DatensatzFragebogenLJJY
data$id <- c(seq(1, length(data$`[id]`)))


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
farben <- paste0('d3.scaleOrdinal().range(["', paste(farben_blau_gruen, collapse = '","'), '"])')

sankey_diagramm <- sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", nodePadding=17,
              sinksRight=FALSE, colourScale=farben, nodeWidth=45, fontSize=12.5)

sankey_diagramm

## Barplots der Lernortnutzung (Vorher und Jetzt)
par(mfrow = c(1, 2))
barplot(sum_before, las = 2, ylim = c(0,100))
barplot(sum_now, las = 2, ylim = c(0,100))

dev.off()


table(as.numeric(data$`Ersatzbew. (10)`))





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


library(tikzDevice)
tikz('Frage2.tex', width=6,height=3.5)
boxplot(as.numeric(na.omit(data$`Frage 2 (Vorher)`)), as.numeric(na.omit(data$`Frage 2 (Jetzt)`)), names = c("Vorher", "Nachher"), main = "Durschnittliche Lernzeit in Stunden pro Woche")
dev.off()

### Sankey Diagramm ###

library(tikzDevice)
tikz('Sankey.tex', width=6,height=3.5)
sankey_diagramm
dev.off()


### Lernorte Vorher Nachher ###

library(tikzDevice)
tikz('Lernorte.tex', width=6,height=3.5)
par(mfrow = c(1, 2))
barplot(sum_before, las = 2, ylim = c(0,100))
barplot(sum_now, las = 2, ylim = c(0,100))
dev.off()

