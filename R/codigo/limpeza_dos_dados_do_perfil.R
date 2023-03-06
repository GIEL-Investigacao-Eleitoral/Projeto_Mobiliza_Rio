library(dplyr)
library(janitor)
library(readxl)
alunos <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/Perfil alunos mobiliza rio março de 23(1).xlsx", 
                     , 
                     col_types = c("text", "date", "text", 
                                   "numeric", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "numeric", "text", "text", "text", 
                                   "numeric", "text", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "numeric", "numeric"),
                     sheet = "alunos(1)") %>% clean_names()
names(alunos)

alunos = alunos[,c(1,2,5,7,8,27:34,83:87)]
alunos = alunos[-1,]

alunos$nascimento[2]
#as.POSIXct.Date(Sys.Date())-alunos$NASCIMENTO[2]

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
#str_match(alunos$atividade_1, "Núcleo:\\s*(.*?)\\s")
alunos$nucleo = str_match(alunos$atividade_1, "Núcleo:\\s\\d\\d")
table(alunos$nucleo)
alunos$nucleo = gsub('Núcleo: ','',alunos$nucleo)

library(stringr)
alunos$bairro = str_to_title(alunos$bairro)
table(alunos$bairro)

# Freguesia,Freguesia Jacarepaguá, Freguesia Jacarépagua

alunos$bairro = gsub('Tijuca 20530420','Tijuca',alunos$bairro)
alunos$bairro = gsub('Praca Da Bandeira', 'Praça Da Bandeira',alunos$bairro)

alunos$bairro = gsub('Recreio Dos Bandeitantes','Recreio Dos Bandeirantes',alunos$bairro)
alunos$bairro = gsub('Recreio Bandeirantes','Recreio Dos Bandeirantes',alunos$bairro)
alunos$bairro = gsub('Recreio Dos Bandeirantes','Recreio',alunos$bairro)

alunos$bairro = gsub('Realengp','Realengo',alunos$bairro)
alunos$bairro = gsub('Realengo Barata','Realengo',alunos$bairro)
alunos$bairro = gsub('Relengo','Realengo',alunos$bairro)

alunos$bairro = gsub('Quintino Bocaiúva','Quintino',alunos$bairro)
alunos$bairro = gsub('Quintino Bocaiuva','Quintino',alunos$bairro)
alunos$bairro = gsub('Quintino','Quintino Bocaiúva',alunos$bairro)

alunos$bairro = gsub('Pedra De Guaratiba Piraquê','Pedra De Guaratiba',alunos$bairro) 
alunos$bairro = gsub('Piraque Pedra De Guaratiba','Pedra De Guaratiba',alunos$bairro)          
alunos$bairro = gsub('Piraque','Pedra De Guaratiba',alunos$bairro) 

alunos[as.numeric(which(alunos$bairro == "Maré")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Mare")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Maré Bonsucesso")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Bonsucesso/ Maré")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Bonsucesso")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Vila Do Pinheiro, Maré")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Bonsucesso/ maré")),'bairro'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$bairro == "Bonsucesso/maré")),'bairro'] = 'Bonsucesso/Maré'

                       
alunos[as.numeric(which(alunos$bairro == "Freguesia Jacarépagua")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Freguesia Jacarepaguá")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Jacarepaguá")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Freguesia")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Manguinho")),'bairro'] = 'Manguinhos'
alunos[as.numeric(which(alunos$bairro == "Cjt Nova Sepetiba")),'bairro'] = 'Nova Sepetiba'
alunos[as.numeric(which(alunos$bairro == "Santa Cruz Areia Branca")),'bairro'] = 'Santa Cruz'

alunos[as.numeric(which(alunos$bairro == "Vila Do Joao")),'bairro'] = 'Vila Do João'

table(alunos$bairro)

alunos$bairro2 = forcats::fct_lump_min(alunos$bairro,10) 
table(alunos$bairro2)


table(alunos$bairro,alunos$nucleo)
# http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html

library(dplyr)
library(ggplot2)
agg = alunos %>% select(bairro2,sexo) %>% table() %>% data.frame()
agg

ggplot(agg) +
  geom_col(aes(x = bairro2, y = Freq, fill = sexo), position = "fill") +
  scale_fill_manual(values = c('red','blue')) 

mutate(agg, bairro2 = reorder(bairro2, Freq)) %>%
  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = bairro2,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("Localidade")
  
  

library(waffle)
stopifnot(packageVersion("waffle") >= "1.0.1")
ggplot(arrange(agg, bairro2), aes(values = Freq, fill = bairro2)) +
  geom_waffle(n_rows = 18, flip = TRUE, color = "white", size = 0.33) +
  coord_equal() +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = c('#00425A','#1F8A70','#BFDB38','#FC7300','#f0bb84','gray40','gray70','gray80',
'#F273E6','#CF4DCE','#912490')) +
  theme_minimal() +
  theme_enhance_waffle()


ggplot(arrange(agg, sexo), aes(values = Freq, fill = sexo)) +
  geom_waffle(n_rows = 18, flip = TRUE, color = "white", size = 0.33) +
  coord_equal() +
  facet_wrap(~ bairro2, ncol=2) +
  scale_fill_manual(values = c('#1F8A70','#FC7300')) +
                                          theme_minimal() +
  theme_enhance_waffle()


group_by(alunos, bairro2,sexo) %>% mutate(percent = value/sum(value))

library(dplyr)
table(agg$sexo)
agg %>% group_by(bairro2) %>% summarise(total=sum(Freq))
agg %>% group_by(bairro2,sexo) %>% mutate(percent =  Freq/sum(Freq)*100)

library(dplyr)
agg = agg %>%
  group_by(bairro2) %>%
  mutate(countT= sum(Freq)) %>%
  group_by(Freq, add=TRUE) %>%
  mutate(per=100*Freq/countT,2)
  #mutate(per=paste0(round(100*Freq/countT,2),'%'))


ggplot(arrange(agg, sexo), aes(values = per, fill = sexo)) +
  geom_waffle(n_rows = 20, flip = TRUE, color = "white", size = 0.33) +
  coord_equal() +
  facet_wrap(~ bairro2, ncol=2) +
  scale_fill_manual(values = c('#1F8A70','#FC7300')) +
  theme_minimal() +
  theme_enhance_waffle()


#----------------------------------------------------------------------

library(dplyr)
library(ggplot2)
agg2 = alunos %>% select(faixa_idade,bairro2,sexo) %>% table() %>% data.frame()
agg2

  
agg2 %>%  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = faixa_idade,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("IDADE")

agg2 %>%  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
                     y = faixa_idade,
                     fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  facet_wrap(~ bairro2, ncol=2) +
  xlab("Quantidade de participantes")+
  ylab("IDADE")


#----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
agg3 = alunos %>% select(nucleo, faixa_idade2) %>% table() %>% data.frame()
agg3
agg3 = agg3 %>%
  group_by(nucleo) %>%
  mutate(countT= sum(Freq)) %>%
  group_by(faixa_idade2, add=TRUE) %>%
  mutate(per=100*Freq/countT,2)
agg3 %>% filter(nucleo=='02')

ggplot()+
  geom_bar(data = agg3, aes(x = nucleo, y=per, fill=faixa_idade2), position="stack", stat="identity")+
  coord_flip() + 
  ggtitle("Faixa etária por núcleo")+
  ylab("Percentual")+
  xlab("Núcleo")+
  scale_fill_brewer(palette="RdBu",labels =c("18 ou menos", "18 até 29", "30 até 39","40 até 49","50 até 59", "60 ou mais"))+
  theme(legend.position="bottom") 


agg4 = alunos %>% select(nucleo, idade) %>% group_by(nucleo) %>% summarise(média=mean(idade, na.rm =T))
agg4

ggplot()+
  geom_point(data = agg4, aes(x = nucleo, y=média), stat="identity", size =5, fill='blue')+
  coord_flip() + 
  ggtitle("Idade média por núcleo")+
  ylab("Idade Média")+
  xlab("Núcleo")

