
# http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html


# SEXO 
agg = alunos %>% select(nucleo,sexo) %>% table() %>% data.frame()
agg

mutate(agg, nucleo ) %>%
  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = nucleo,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("Núcleo")+ theme_minimal()
  
agg = agg %>%
  group_by(nucleo) %>%
  mutate(countT= sum(Freq)) %>%
  group_by(Freq, add=TRUE) %>%
  mutate(per=100*Freq/countT,2)
  #mutate(per=paste0(round(100*Freq/countT,2),'%'))

#----------------------------------------------------------------------
agg2 = alunos %>% select(faixa_idade2,nucleo,sexo) %>% table() %>% data.frame()
agg2

agg2 %>%  ggplot(aes(x = ifelse(sexo == "MASCULINO", Freq, -Freq),
             y = faixa_idade2,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de participantes")+
  ylab("Faixa etária")

#----------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
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
  scale_y_continuous(limits =c(0,100),breaks = seq(0,90,10))+
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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#             SEXO 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
