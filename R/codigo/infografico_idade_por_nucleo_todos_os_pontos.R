library(tidyr)
library(dplyr)
library(ggplot2)

sum(is.na(alunos$nucleo))

agg4 = alunos %>% select(nucleo, idade) %>% drop_na(nucleo) %>% group_by(nucleo) %>% summarise(Mínimo =min(idade, na.rm =T),Média=mean(idade, na.rm =T),Máximo=max(idade,na.rm = T))
agg4

agg5 = alunos %>% select(nucleo, idade) %>% drop_na(nucleo) 
agg5


ggplot(data = agg4, aes(x = nucleo, y=Média))+
  geom_rect(fill = "#FFFFFF",xmin = 0,xmax = 1.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#F7FBFF",xmin = 1.5,xmax = 2.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#DEEBF7",xmin = 2.5,xmax = 3.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#C6DBEF",xmin = 3.5,xmax = 4.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#9ECAE1",xmin = 4.5,xmax = 5.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#6BAED6",xmin = 5.5,xmax = 6.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#4292C6",xmin = 6.5,xmax = 7.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#2171B5",xmin = 7.5,xmax = 8.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#08519C",xmin = 8.5,xmax = 9.5,ymin = -1, ymax = 110) +   
  geom_rect(fill = "#07498c",xmin = 9.5,xmax = 10.5,ymin = -1, ymax = 110) +   
  #geom_jitter(data = agg5, aes(x = as.numeric(nucleo), y=idade), size =2, colour=ifelse(agg5$nucleo=='10'|agg5$nucleo=='09'|agg5$nucleo=='08'|agg5$nucleo=='07'|agg5$nucleo=='06','#FFFFFF','black'))+
  geom_jitter(data = agg5, aes(x = as.numeric(nucleo), y=idade), size =2)+
  geom_point(data = agg4, aes(x = nucleo, y=Média), stat="identity", size =5, colour='red')+
  #geom_point(data = agg4, aes(x = nucleo, y=Mínimo), stat="identity", size =5, colour='royalblue')+
  #geom_point(data = agg4, aes(x = nucleo, y=Máximo), stat="identity", size =5, colour='royalblue')+
  scale_y_continuous(limits =c(0,100),breaks = seq(0,90,10))+
  coord_flip() + 
  ggtitle("Gráfico - Idade média por núcleo. Março/2023")+
  ylab("Idade")+
  xlab("Núcleo")+
  labs(caption =  "Idade média em vermelho")+
  theme_minimal()


library(RColorBrewer)
display.brewer.all(n=10)
RColorBrewer::brewer.pal(10,'Spectral')
RColorBrewer::brewer.pal(10,'RdYlBu')
RColorBrewer::brewer.pal(9,'Blues')

RColorBrewer::display.brewer.all()

alunos %>% select(nucleo, idade) %>% filter(nucleo=='05') %>% drop_na(nucleo) %>% group_by(nucleo) %>% summarise(Mínimo =min(idade, na.rm =T),Média=mean(idade, na.rm =T),Máximo=max(idade,na.rm = T))
agg4
