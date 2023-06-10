library(tidyverse)
library(dplyr)
library(here)
library(readr)
library(httr)
seasons <- c(1920, 2021, 2122, 2223) 

cat("Selecione a temporada: \n")
for (i in seq_along(seasons)) {
  cat(paste(i, ". ", seasons[i], "\n", sep = ""))
}

selecao <- as.integer(readline()) # Rodar o codigo até aqui se quiser ver as temporadas disponiveis
# Apaga o comentário e bota seu input aqui, numero de 1 ate 4 dependendo da temporada

season <- 0
if (1 <= selecao & selecao <= length(seasons)) {
  season <- seasons[selecao]
} else {
  cat("Seleção inválida.\n")
}

url <- paste0("https://www.football-data.co.uk/mmz4281/", 2021, "/E0.csv")


response <- GET(url)

if (status_code(response) == 200) {
  content <- content(response, "text")
  df <- read.csv(text = content, stringsAsFactors = FALSE)
} else {
  cat("Falha ao obter a página:", status_code(response), "\n")
}