library(dplyr)
library(janitor)
library(readxl)
library(ggplot2)
library(ggtext)
alunos <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/Perfil alunos mobiliza rio março de 23 editado.xlsx", 
                     sheet = "alunos(2)", col_types = c("text", 
                                                        "date", "text", "numeric", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "numeric", 
                                                        "text", "text", "text")) %>% clean_names()
names(alunos)

alunos$nascimento[2]

trunc(as.numeric(difftime(as.POSIXct.Date(Sys.Date()), alunos$nascimento[2], units = "days")) / 365.25)
alunos$idade = trunc(as.numeric(difftime(as.POSIXct.Date(Sys.Date()), alunos$nascimento, units = "days")) / 365.25)
max(alunos$idade, na.rm = TRUE)
min(alunos$idade, na.rm = TRUE)
alunos$idade = ifelse(alunos$idade<1,NA,alunos$idade)

alunos = alunos %>%
  mutate(
    faixa_idade = case_when(
      idade <= 18 ~ "01 (18 anos ou menos)",
      idade <= 25 ~ "02 (18 à 25 anos)",
      idade <= 30 ~ "03 (26 à 30 anos)",
      idade <= 35 ~ "04 (31 à 35 anos)",
      idade <= 40 ~ "05 (36 à 40 anos)",
      idade <= 45 ~ "06 (41 à 45 anos)",
      idade <= 50 ~ "07 (46 à 50 anos)",
      idade <= 55 ~ "08 (51 à 55 anos)",
      idade <= 60 ~ "09 (56 à 60 anos)",
      idade <= 90 ~ "10 (61 anos ou mais)",
      TRUE~ '999'))


alunos$faixa_idade = ifelse(alunos$faixa_idade=='999',NA,alunos$faixa_idade)
table(alunos$faixa_idade)


alunos = alunos %>%
  mutate(
    faixa_idade2 = case_when(
      idade <= 18 ~ "1",
      idade <= 29 ~ "2",
      idade <= 39 ~ "3",
      idade <= 49 ~ "4",
      idade <= 60 ~ "5",
      idade <= 90 ~ "6",
      TRUE~ '999'))
table(alunos$faixa_idade2)
alunos$faixa_idade2 = ifelse(alunos$faixa_idade2=='999',NA,alunos$faixa_idade2)
table(alunos$faixa_idade2)

library(stringr)
alunos$nucleo = str_match(alunos$atividade_1, "Núcleo:\\s\\d\\d")
table(alunos$nucleo)
alunos$nucleo = gsub('Núcleo: ','',alunos$nucleo)
table(alunos$nucleo)
