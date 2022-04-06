# Einfuehrung R

# Berechnungen durchf¸hren
1+2
1-2
2*2
6/2
2^2

# Objekte zuweisen
a <- 3*5
a

b <- 4-2

a* b

# Je nach Input k√∂nnen die Objekte auch kombiniert werden
a* b

# Objekte sind ver‰nderlich
a <- a* b -6 + (2*4)
a

# Objekte sinnvoll benennen

mittelwert <- (1+2+3+4+5+6)/6
Mittwelwert <- (5+6+7+8) / 4




# Funktionen
word_printer <- function(word){
  print(paste('Dies ist ein Wort:', word ))
}

word_printer('hallo')

# Datenstrukturen
text <- "Mittwoch ist Spritwoch"
class(text)
zahl <- 1
class(zahl)
zahl <- "1"
class(zahl)
zahl <- 1.1
class(zahl)
date <- as.Date("2020-05-05")
class(date)

x <- "apfel" == "birnen"
x
class(x)


# Vektoren


vektor_zahlen <- c(1,2,3,4,5)

vektor_zahlen

vektor_text <- c("Dumbledore", "Rubeus Hagrid", "Minerva McGonagall")
vektor_text

vektor_text[1]
vektor_text[c(1,2)]



vektor_zahlen_2 <- c(8,9,0,23,3)

vektor_zahlen * vektor_zahlen_2


mean(vektor_zahlen)
length(vektor_zahlen * vektor_zahlen_2)
sum(vektor_zahlen * vektor_zahlen_2)
sd(vektor_zahlen_2)

# Listen 
mitarbeiter <- list(name = "Dumbledore", "Rubeus Hagrid", "Minerva McGonagall", ort ="hogwarts", Land = "uk",  postleitzahl = 48151 )
mitarbeiter

mitarbeiter$name

mitarbeiter[1]
mitarbeiter[[2]]
mitarbeiter[2]



# Matrizen
x <- 1:9
m <- matrix(x, nrow = 3)
m

colnames(m) <- c("eins", "zwei", "drei")
rownames(m)<- c("eins", "zwei", "drei")
m

m[,2]

# Dataframe

hogwarts_mitarbeiter <- data.frame(
  name = c("Dumbledore", "Rubeus Hagrid", "Minerva McGonagall", "Severus Snape"),
  beruf = c("Schulleiter", "Wildh¸ter", "Lehrer_in", "Lehrer_in"),
  alter = c(102, 54, 65, 55)
)

hogwarts_mitarbeiter 

hogwarts_mitarbeiter[2,2]
hogwarts_mitarbeiter[2,1:3]
hogwarts_mitarbeiter$name
hogwarts_mitarbeiter$beruf[1]



# ‹berblick
str(hogwarts_mitarbeiter)
summary(hogwarts_mitarbeiter)
head(hogwarts_mitarbeiter,5)
mean(hogwarts_mitarbeiter$alter)
subset(hogwarts_mitarbeiter, name != "Dumbledore")


# Tidyverse
install.packages("tidyverse")
library(tidyverse)


hogwarts_mitarbeiter %>%  filter( name == "Dumbledore")

hogwarts_mitarbeiter %>%  group_by(beruf) %>% count()

hogwarts_mitarbeiter %>%  group_by(beruf) %>% summarise(d_alter = mean(alter))


# Loops

for (i in 1:10){
  print(i)
}



i <- 1
while (i < 6) {
  print(i)
  i = i+1
}














# Bundestag Ueberblick ############################################################

#Laden Library
library(plotly)
library(dplyr)
library(jsonlite)

# Path setzen 
setwd('C:\\Users\\Drecker\\Documents\\Lehre')

# Laden der Bundestag Daten
filename <- file.choose()
Bundestag <- readRDS(filename)



# Erste f√ºnf Zeilen sehen
View(head(Bundestag))

# √ueberblick
str(Bundestag)
summary(Bundestag)

# Filtern der Daten nach Partein. Anschliessend nach Parteien gruppieren und die Anzahl der Reden z√§hlen
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

# Ausf√ºhren
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

# √ndern des weekdays auf factor um anschlie√ssend die Tage ordnen zu k√∂nnen
# Wenn dieser Schritt nicht durchgef√ºhrt wird werden die Tage nach Alphabet geordnet
plot_data$weekday<- factor(plot_data$weekday, levels= c( "Montag",
                                         "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag","Sonntag"))
plot_data <- plot_data %>% arrange(weekday)

  plot_ly(
  x = plot_data$weekday,
  y = plot_data$n,
  name = "",
  type = "bar"
)

##############################################################################################################
#Sentiment

sen_train <-   read.csv(file = ".\\Daten\\Sentiment\\Corona_NLP_train.csv")  
sen_test <-   read.csv(file = ".\\Daten\\Sentiment\\Corona_NLP_test.csv")  


train_stat <- sen_train %>% group_by(Sentiment)  %>% 
  summarise(Anzahl = n(), Anteil = n()/ sen_train %>% group_by(Sentiment) %>% count() %>% ungroup(Sentiment) %>% summarise('Other'=sum(n)))

train_stat 


test_stat <- sen_test  %>% group_by(Sentiment)  %>% 
  summarise(Anzahl = n(), Anteil = n()/ sen_test %>% group_by(Sentiment) %>% count() %>% ungroup(Sentiment) %>% summarise('Other'=sum(n)))

test_stat 
  
  
  
#################################################################################################################  
#Fake News 
  
fake <-   read.csv(file = ".\\Daten\\Fakenews_detection\\Fake.csv")  
true <-   read.csv(file = ".\\Daten\\Fakenews_detection\\True.csv")  
  
  
  
  
  
  