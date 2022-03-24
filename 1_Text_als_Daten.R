# Einführung R

# Berechnungen durchführen
1+2

# Objekte zuweisen
a <- 3*5
a

b <- 4-2

# Je nach Input können die Objekte auch kombiniert werden
a*b


# Objekte haben verschiedene Klassen
# Beispielsweise
character()
numeric()
integer()
date()
# Nicht mit jeder Klasse kann jede Berechnung durchgeführt werden
# Viele Fehlermeldungen lassen sich auf die falsche Klasse zurückführen
# So kann ein Objekt auch die Klasse character() beinhalten

ob <- c('Harry', 'Ron', "Hermine")

# Auf inhalte Objekte kann beispielsweise über [] zugegriffen werden
ob[1]

# Eine wichtige Objektklasse ist das data.frame()
Name <- c('Harry', 'Ron', "Hermine", "Hermine")
Zauberstablaenge <- c(77, 41, 323, 1)
data <- data.frame(Name, Zauberstablaenge)
class(data)
# Auf Daten im data.frame (und Matrizen) kann auf unterschiedliche weise zugegriffen werden
# Die erste Zahl definiert die Zeile die zweite die Spalte
data[1,2]

# Es kann auch auf ganze Zeilen oder Spalten zugegriffen werden
data[,2]
data[1,]

# Dateframes bieten den Vorteil auch direkt über den Spaltenamen zugriff zu erhalten
# Dies ist zu empfehlen

data$Name
data$Zauberstablaenge[1]


# Loops

for (i in 1:10){
  print(i)
}

# while

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

# Funktionen

word_printer <- function(word){
                              print(paste('Dies ist ein Wort:', word ))
                            }

word_printer('hallo')


# Neben diesen Bais R gibt es verschiedene Pakete die das Arbeiten mit R deutlich erleichtern
# Diese Pakete beinhalten verschiedenste Funktionen
# Eine der beliebtesten Pakte ist tidyverse
# tidyverse beinhaltet verschiedene Pakete zusammengesaat in einem
# Durch dieses Paket wird das ARbeiten mit Daten deutlich erleichtert
# Es gibt das entsprechnde Paket auch f�r Textdaten Tidytext
# Durch %>% werden die Daten vin die n�chste Funktion weiter gegeben
library(tidyverse)
data %>% select(Name)
data %>%
  group_by(Name) %>%
  summarise(avg_Zauberstablaenge = mean(Zauberstablaenge))


# Bundestag Überblick ############################################################

#Laden Library
library(plotly)
library(dplyr)
library(jsonlite)

# Path setzen 
setwd('C:\\Users\\Drecker\\Documents\\Lehre')

# Laden der Bundestag Daten
filename <- file.choose()
Bundestag <- readRDS(filename)



# Erste fünf Zeilen sehen
View(head(Bundestag))

# �ueberblick
str(Bundestag)
summary(Bundestag)

# Filtern der Daten nach Partein. Anschliessend nach Parteien gruppieren und die Anzahl der Reden zählen
party_speech <- Bundestag %>% filter(party != 	'NA' & date >= '2017-01-01' ) %>% group_by(party)  %>% count()



# Definieren der Farben - Parteifarben
colors <- c('rgb(139,69,19)', 'rgb(0,0,0)', 'rgb(255,237,0)', 'rgb(100,161,45)', 'rgb(255, 255, 255)', 'rgb(104,34,139)'  ,'rgb(227, 0, 15)')


# Plotly
fig <- plot_ly(party_speech, labels = ~party, values = ~n, type = 'pie',

               textposition = 'inside',

               textinfo = 'label+percent',

               insidetextfont = list(color = '#FFFFFF'),

               hoverinfo = 'text',

               text = ~paste( n, ' Reden'),

               marker = list(colors = colors,

                             line = list(color = '#FFFFFF', width = 1)),


               showlegend = FALSE)

# Layout
fig <- fig %>% layout(title = 'Verteilung der Reden nach Partei seit 2017',

                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),

                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Ausführen
fig





########################################################################################################
#Kreml Data


# load data
kreml <- lapply(readLines(".\\Daten\\kremlin_transcripts_en_2021_03_01.json"), fromJSON)


# Umwandeln von List in List zu dataframe
kreml_data <- data.frame(id = as.integer(), date = as.character(), title = as.character(), text = as.character())
for (i in 1:length(kreml)){
  print(i)
  doc <- data.frame(id = kreml[[i]]$`_source`$kremlin_id, date = kreml[[i]]$`_source`$date, title = kreml[[i]]$`_source`$title, text = kreml[[i]]$`_source`$transcript)
  kreml_data <- bind_rows(kreml_data, doc )
}

# Erstellen einer Day Spalte
kreml_data <- kreml_data %>% mutate(weekday = weekdays(as.Date(kreml_data$date)) )

# Group by weekday
plot_data <-kreml_data %>%
            group_by(weekday) %>%
            count()

# �ndern des weekdays auf factor um anschlie�ssend die Tage ordnen zu können
# Wenn dieser Schritt nicht durchgeführt wird werden die Tage nach Alphabet geordnet
plot_data$weekday<- factor(plot_data$weekday, levels= c( "Montag",
                                         "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag","Sonntag"))
plot_data <- plot_data %>% arrange(weekday)

  plot_ly(
  x = plot_data$weekday,
  y = plot_data$n,
  name = "",
  type = "bar"
)



