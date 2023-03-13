library(dplyr)
library(janitor)
library(readxl)
library(ggplot2)
library(ggtext)
alunos <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/Perfil alunos mobiliza rio 10 março de 23 editado.xlsx", 
                                                                col_types = c("text", "date", "text", 
                                                                              "numeric", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "numeric", "text", "text", "text", 
                                                                              "numeric", "text", "text", "text", 
                                                                              "text", "text", "text", "numeric", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "numeric", 
                                                                              "numeric", "text", "numeric", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "numeric", "text", "text", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "text", "text", "text", "text", "text", 
                                                                              "numeric", "numeric")) %>% clean_names()
names(alunos)

alunos = alunos[,c(1,2,5,7,8,27:34,83:87)]
alunos[1,]
alunos = alunos[-1,]

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
#1 - Ensino Infantil,2 - Ensino Fundamental
#3 - Ensino Médio,4 - Ensino Superior
#5 - Pós-Graduação,6 - Mestrado,7 - Doutorado
table(alunos$escolaridade)
alunos = alunos %>%
  mutate(
    escolaridade = case_when(
      escolaridade == 1 ~ "1.Ensino Infantil",
      escolaridade == 2 ~ "2.Ensino Fundamental",
      escolaridade == 3 ~ "3.Ensino Médio",
      escolaridade == 4 ~ "4.Ensino Superior",
      escolaridade == 5 ~ "4.Ensino Superior",
      TRUE~ '999'))


library(stringr)
faixas_01 = stringr::str_split(alunos$atividade_1," - ",, simplify = TRUE)
faixas_01 = data.frame(faixas_01)
colnames(faixas_01) = c('Modalidade','novo_nucleo','dias','horarios')
faixas_02 = stringr::str_split(alunos$atividade_2," - ",, simplify = TRUE)
faixas_02 = data.frame(faixas_02)
colnames(faixas_02) = c('Modalidade','novo_nucleo','dias','horarios')
faixas_03 = stringr::str_split(alunos$atividade_3," - ",, simplify = TRUE)
faixas_03 = data.frame(faixas_03)
colnames(faixas_03) = c('Modalidade','novo_nucleo','dias','horarios')

table(faixas_01$dias)
table(faixas_02$dias)
table(faixas_03$dias)

table(faixas_01$horarios)
table(faixas_02$horarios)
table(faixas_03$horarios)

alunos$terminou_estudos = ifelse(alunos$idade<25,'idade escolar',
                                 ifelse(alunos$idade>=25,'terminou os estudos',
                                        NA))
table(alunos$terminou_estudos)
table(alunos$escolaridade,alunos$nucleo)

three_way =xtabs(~ nucleo + escolaridade+ terminou_estudos, data=alunos)
ftable(three_way)

library(tidyr)
library(dplyr)
library(ggplot2)

alunos %>% drop_na(nucleo) %>%
  count(nucleo, escolaridade) %>%       
  group_by(nucleo) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(nucleo, pct, fill=escolaridade) +
  geom_bar(stat="identity") +
  ylab("Percentual") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Escolaridade por núcleo") +
  theme_bw()+
  scale_fill_brewer(name="Escolaridade")
  scale_color_brewer(palette = "Reds")
