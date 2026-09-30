install.packages("readr")
install.packages("rcompanion")

library(readr)
library(rcompanion)

# Working Direkctory setzen
setwd("C:/Users/felix/Downloads/")

# Daten einlesen
data <- read_csv("MAP_twitter_toxicity_topics.csv")
View(data)


#1. Histogram erstellen 
# 
hist(data$Toxicity_score, breaks = 10, col = "blue", xlab = "Toxizitäts-Score", ylab = "Frequenz", main = "Datenverteilung über Toxizität-Bins")

# Anteil an den tox_labels
prop.table(table(data$Tox_label))

#2. VergleichTabellen erstellen
tox_labels = c("low","medium","high")

# Neue Spalte Tox_label entsprechend befüllen 
data[data$Toxicity_score <= 0.1, "Tox_label"] <- "low"
data[data$Toxicity_score > 0.1 & data$Toxicity_score < 0.9, "Tox_label"] <- "medium"
data[data$Toxicity_score >= 0.9, "Tox_label"] <- "high"

# Anzahl der Daten pro Kategorie
table(data$Tox_label)


# Prop Tabellen erstellen 
low_prop_table <- prop.table(table(data[data$Tox_label == "low","Topic_name"]))
medium_prop_table <- prop.table(table(data[data$Tox_label == "medium","Topic_name"]))
high_prop_table <- prop.table(table(data[data$Tox_label == "high","Topic_name"]))

# Zusammenführen der prop_tables
df_topics_percentile <- data.frame(Topic_name  = names(low_prop_table),low = as.numeric(low_prop_table),medium = as.numeric(medium_prop_table), high   = as.numeric(high_prop_table)
)


# in Prozent umwandeln 
df_topics_percentile$low    <- round(df_topics_percentile$low    * 100, 1)
df_topics_percentile$medium <- round(df_topics_percentile$medium * 100, 1)
df_topics_percentile$high   <- round(df_topics_percentile$high   * 100, 1)


print(df_topics_percentile)


# 3. Unterschiedstest zwischen den clustern
# https://www.datacamp.com/de/tutorial/chi-square-test-r
# Kreuztabelle zwischen Topics des Labels low und medium
lowmedium <- data[data$Tox_label == "low" | data$Tox_label == "medium",]
lowmedium_table <- table(lowmedium$Tox_label,lowmedium$Topic)
# Kreuztabelle zwischen Topics des Labels medium und high
mediumhigh <- data[data$Tox_label == "medium" | data$Tox_label == "high",]
mediumhigh_table <- table(mediumhigh$Tox_label,mediumhigh$Topic)

# Kreuztabelle zwischen Topics des Labels low und medium
lowhigh <- data[data$Tox_label == "low" | data$Tox_label == "high",]
lowhigh_table <- table(lowhigh$Tox_label,lowhigh$Topic)
# Quadrat Test ausführen
result1 <- chisq.test(lowmedium_table)
result2 <- chisq.test(mediumhigh_table)
result3 <- chisq.test(lowhigh_table)

# Erwartete Häufigkeiten prüfen
expected <- result1$expected
sum(expected < 5) / length(expected)   # Anteil Zellen mit < 5
# Erwartete Häufigkeiten prüfen
expected <- result2$expected
sum(expected < 5) / length(expected)   # Anteil Zellen mit < 5
# Erwartete Häufigkeiten prüfen
expected <- result3$expected
sum(expected < 5) / length(expected)   # Anteil Zellen mit < 5
# resultate printen 
print(result1)
print(result2)
print(result3)

# Mit Cramers V Effektstärke berechnen
# https://www.geeksforgeeks.org/r-language/how-to-calculate-cramers-v-in-r/
cramers_v1 <- cramerV(lowmedium_table)
print(cramers_v1)
cramers_v2 <- cramerV(mediumhigh_table)
print(cramers_v2)
cramers_v3 <- cramerV(lowhigh_table)
print(cramers_v3)


#4. Themen nach gewichteter Toxicity abbilden

# Topics ermitteln 
topics = unique(data$Topic_name)
df_topics <- data.frame()
# data frame mit topics und NA-Werten befüllen
for (topic in topics){
  df_topics["low", topic] <- NA
  df_topics["medium", topic] <- NA
  df_topics["high", topic] <- NA
  df_topics["tox_score", topic] <- NA
  df_topics["p_topic",topics] <- NA
}
# durch Topics durch iterieren
for (topic in topics){
  
  # Neues df erstellen, bestehend aus den Tox_labeln des entsprechenden Topics
  df_topic_toxlabel <- data[data$Topic_name == topic, "Tox_label"]
  
  # Anzahl der Gesamtanzahl zählen 
  all_count = nrow(df_topic_toxlabel[, "Tox_label"])
  # Anzahl der low Labels zählen
  low_count = nrow(df_topic_toxlabel[df_topic_toxlabel$Tox_label == "low", "Tox_label"])
  # Anzahl der medium Labels zählen
  medium_count = nrow(df_topic_toxlabel[df_topic_toxlabel$Tox_label == "medium", "Tox_label"])
  # Anzahl der high Labels zählen
  high_count = nrow(df_topic_toxlabel[df_topic_toxlabel$Tox_label == "high", "Tox_label"])
  
  # Prozentuale Anteile berechnen
  low_percent = low_count/all_count
  medium_percent = medium_count/all_count
  high_percent = high_count/all_count
  
  # Im df_topics abspeichern
  df_topics["low", topic] <- low_percent
  df_topics["medium", topic] <- medium_percent
  df_topics["high",topic] <- high_percent
}


# Wahrscheinlichkeit pro Topic berechnen
all_count = nrow(data[,])
for (topic in topics){
  topic_count = nrow(data[data$Topic_name == topic,])
  p_topic = topic_count/all_count
  df_topics["p_topic",topic] <- p_topic
}

# Gewichteten Toxizitäts score aufstellen. Dafür Wahrscheinlichkeit eines labels innerhalb eines #Topics (P(topic|label)) mal dem entsprechendem gewicht rechnen. 
# Anschließend den score mit der Auftretenswahrscheinlichkeit des Topics im Gesamtdatensatz verrechnen. 
weight_h = 3
weight_m = 2
weight_l = 1
for (topic in topics){
  toxscore <- (weight_h * df_topics["high",topic]) + (weight_m * df_topics["medium", topic]) + (weight_l * df_topics["low", topic])
  df_topics["tox_score", topic] <- toxscore * df_topics["p_topic",topic]
}

t(df_topics)
