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

#alunos = alunos[,c(1,2,5,7,8,27:34,83:87)]
#alunos = alunos[-1,]

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
alunos$nucleo = str_match(alunos$atividade_1, "Núcleo:\\s\\d\\d")
table(alunos$nucleo)
alunos$nucleo = gsub('Núcleo: ','',alunos$nucleo)

# http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html

agg = alunos %>% select(nucleo,sexo) %>% table() %>% data.frame()
agg

ggplot(agg) +
  geom_col(aes(x = nucleo, y = Freq, fill = sexo), position = "fill") +
  scale_fill_manual(values = c('red','blue')) 

mutate(agg, nucleo ) %>%
  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = nucleo,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("Núcleo")+ theme_minimal()
  
library(waffle)
stopifnot(packageVersion("waffle") >= "1.0.1")
ggplot(arrange(agg, nucleo), aes(values = Freq, fill = nucleo)) +
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
  facet_wrap(~ nucleo, ncol=2) +
  scale_fill_manual(values = c('#1F8A70','#FC7300')) +
  theme_minimal() +
  theme_enhance_waffle()


group_by(alunos, bairro2,sexo) %>% mutate(percent = value/sum(value))

table(agg$sexo)
agg %>% group_by(nucleo) %>% summarise(total=sum(Freq))
agg %>% group_by(nucleo,sexo) %>% mutate(percent =  Freq/sum(Freq)*100)

agg = agg %>%
  group_by(nucleo) %>%
  mutate(countT= sum(Freq)) %>%
  group_by(Freq, add=TRUE) %>%
  mutate(per=100*Freq/countT,2)
  #mutate(per=paste0(round(100*Freq/countT,2),'%'))


ggplot(arrange(agg, sexo), aes(values = per, fill = sexo)) +
  geom_waffle(n_rows = 20, flip = TRUE, color = "white", size = 0.33) +
  coord_equal() +
  facet_wrap(~ nucleo, ncol=2) +
  scale_fill_manual(values = c('#1F8A70','#FC7300')) +
  theme_minimal() +
  theme_enhance_waffle()


#----------------------------------------------------------------------
library(dplyr)
library(ggplot2)
agg2 = alunos %>% select(faixa_idade2,nucleo,sexo) %>% table() %>% data.frame()
agg2

agg2 %>%  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = faixa_idade2,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("IDADE")

agg2 %>%  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
                     y = faixa_idade2,
                     fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  facet_wrap(~ nucleo, ncol=2) +
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


library(tidyr)
sum(is.na(alunos$nucleo))

agg4 = alunos %>% select(nucleo, idade) %>% drop_na(nucleo) %>% group_by(nucleo) %>% summarise(Mínimo =min(idade, na.rm =T),Média=mean(idade, na.rm =T),Máximo=max(idade,na.rm = T))
agg4

ggplot()+
  geom_pointrange(data = agg4, aes(x = nucleo, y=Média,ymin = Mínimo,
                                ymax = Máximo))+
  geom_point(data = agg4, aes(x = nucleo, y=Média), stat="identity", size =5, colour='red')+
  geom_point(data = agg4, aes(x = nucleo, y=Mínimo), stat="identity", size =5, colour='royalblue')+
  geom_point(data = agg4, aes(x = nucleo, y=Máximo), stat="identity", size =5, colour='royalblue')+
  coord_flip() + 
  ggtitle("Gráfico - Idade média por núcleo. Março/2023")+
  ylab("Idade")+
  xlab("Núcleo")+
  labs(caption =  "idade mínima e máxima em azul. idade média em vermelho")+
  theme_minimal()


agg5 = alunos %>% select(idade) %>% summarise(Mínimo =min(idade, na.rm =T),Média=mean(idade, na.rm =T),Máximo=max(idade,na.rm = T))
agg5

ggplot()+
  geom_pointrange(data = agg5, aes(x = 1, y=Média,ymin = Mínimo,
                                   ymax = Máximo))+
  geom_point(data = agg5, aes(x = 1, y=Média), stat="identity", size =15, colour='red')+
  geom_point(data = agg5, aes(x = 1, y=Mínimo), stat="identity", size =15, colour='royalblue')+
  geom_point(data = agg5, aes(x = 1, y=Máximo), stat="identity", size =15, colour='royalblue')+
  coord_flip() + 
  ggtitle("Gráfico - Idade média por núcleo. Março/2023")+
  ylab("Idade")+
  xlab(" ")+
  #labs(caption =  "idade mínima e máxima em azul. idade média em vermelho")+
  theme_minimal()+  
  geom_curve(
    aes(x = 1.2, y = 41, xend = 1.1, yend = 35),
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  annotate("text", label = "idade média de 35 anos",
           x = 1.2, y = 55, size = 4, colour = "grey50",    family = "Bebas",fontface = "bold"
  ) +  xlim(0.5, 1.5)+
  geom_segment(
    aes(x = 1.3, y = 15, xend = 1.1, yend = 7),
    arrow =arrow(length = unit(0.5, "cm"))
  )+
  annotate("text", label = "idade mínima de 5 anos",
           x = 1.35, y = 15, size = 4, colour = "grey50",    family = "Bebas",fontface = "bold"
  )+
  geom_segment(
    aes(x = 1.3, y = 75, xend = 1.1, yend = 85),
    arrow =arrow(length = unit(0.5, "cm"))
  )+
  annotate("text", label = "idade máxima de 89 anos",
           x = 1.35, y = 75, size = 4, colour = "grey50",    family = "Bebas",fontface = "bold"
  )+
  theme(axis.title.y=element_blank(),
         axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

