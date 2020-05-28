rm(list = ls())

library(rwhatsapp) # paquete para analisis de texto de whatsapp
library(lubridate) 
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(ggimage)
library(dplyr)
library(stopwords)

miChat <- rwa_read("grupo.txt")


# Make a variable with the station of the year 
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(estacion = case_when(
    day >= dmy(18082018) & day <= dmy(22092018) ~ "Summer 2018",
    day >= dmy(23092018) & day <= dmy(20122018) ~ "Autumn 2018",
    day >= dmy(21122018) & day <= dmy(20032019) ~ "Winter 2018",
    day >= dmy(21032019) & day <= dmy(21062019) ~ "Spring 2019",
    day >= dmy(22062019) & day <= dmy(23092019) ~ "Summer 2019",
    day >= dmy(23092019) & day <= dmy(20122019) ~ "Autumn 2019",
    day >= dmy(21122019) & day <= dmy(20032020) ~ "Winter 2020",
    day >= dmy(21032020) ~ "Spring 2020",
    T ~ "Out of range")
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))

paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]

# Messages per day
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Number of messages") + xlab("") +
  ggtitle("Messages") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

max(miChat)

miChat %>% 
  count(day) %>%
  mutate(n = as.numeric(n)) %>%
  filter(n == max(n))



# Messages per day of the week
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages per day") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")


# Messages per hour
diasemana <- c("sunday","monday","tuesday","wednesday","thursday","friday","satuday")
names(diasemana) <- 1:7

miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta.estaciones) +
  ylab("Number of messages") + xlab("Hour") +
  ggtitle("Number of messages per hour") +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom",
         panel.spacing.x=unit(0.0, "lines"))


# Number of messages by author
miChat %>%
  mutate(day = date(time)) %>%
  group_by(estacion) %>% 
  count(author) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta.estaciones) +
  ylab("Total messages") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages per user") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

# Most often used emojis
miChat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis") +
  theme_test()


# Most commonly used words
remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "media", "omitted", "y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa",
                      "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una",
                      "i'm", "bored", "m", "i", "b", "d", "red",
                      "del", "qué", "todo", "ti", "https", "sé", "ay", "tus", "también", "así", "le", "su", "va", "porque", "todos", "hay", "les",
                      "muy", "día", "ok", "estar", "eres", "como", "ed", 
                      "pue", "message", "bo", "red", "bor", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas",
                      "ir","voy", "n", "r", "bore", "ored", "creo","fue","solo", "ni","sólo","nada", "aqui", "q", "tú", "fez")

miChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_palabras) %>% 
  count(word) %>% 
  top_n(30,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Most commonly used words") +
  xlab("Words") +
  ylab("Number of times the word was used") +
  coord_flip() +
  theme_minimal()  

# Most commonly used words by author
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras &
           author == "Andrés Varela") %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 20, n) %>%
  slice(1:20) %>%
  ungroup() %>% 
  arrange(author, desc(n)) %>% 
  mutate(order=row_number()) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = author, color = author)) +
  geom_col(show.legend = FALSE, width = .1) +
  geom_point(show.legend = FALSE, size = 3) +
  xlab("Words") +
  ylab("Number of times the word was used") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle("Most used words per user") +
  theme_minimal()
