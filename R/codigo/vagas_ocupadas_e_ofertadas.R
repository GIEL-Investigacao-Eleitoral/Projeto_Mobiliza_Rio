

library(dplyr)
library(janitor)
library(readxl)

MobilizaBD <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/MobilizaBD.xlsx") %>% clean_names()
head(MobilizaBD)
names(MobilizaBD)


library(ggplot2)
library(ggtext)
MobilizaBD  %>%
  ggplot(
    aes(
      x = nucleo,
      y = vgs_ocu
    ),
    color = "white",
  ) +
  geom_col(aes(x = nucleo, y = vgs_ofe), fill = "lightgrey", color = "transparent", alpha = 0.7, width = 0.7) +
  geom_col(fill="royalblue") +
  geom_curve(
    aes(x = 5, y = 130, xend = 1, yend = 100.5),
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  annotate("text", label = "vagas ofertadas",
    x = 5, y = 125, size = 4, colour = "grey50",    family = "Bebas",fontface = "bold"
  ) + scale_x_continuous(
    breaks = seq(1,10,1)
  )+
  theme_minimal()+
  labs(
    x = "Núcleo",
    y = "Vagas ocupadas",
    caption = "Dados: ProjetoMobiliza Rio",
    title = "Vagas ocupadas e ofertadas",
    subtitle = "no projeto Mobiliza Rio 06/03/2023"
  )+ scale_y_continuous(
    breaks = seq(0,175,25)
  )

MobilizaBD  %>% group_by(nucleo) %>% summarise(vagas=sum(vgs_ofe))
MobilizaBD  %>% group_by(nucleo) %>% summarise(vagas=sum(vgs_ocu))  

library(ggplot2)
library(ggtext)

tabela <- MobilizaBD %>% 
  group_by(modalidade) %>% 
  summarise(vagas_ofertadas=sum(vgs_ofe),vagas_ocupadas=sum(vgs_ocu))

tabela = tabela %>% mutate(tx_ocu = vagas_ocupadas/vagas_ofertadas)
sum(tabela$vagas_ofertadas)
sum(tabela$vagas_ocupadas)

tabela  %>%
  ggplot(
    aes(
      x = modalidade,
      y = tx_ocu
    ),
    color = "white",
  ) +
  geom_col(fill=c("red","red","red","red","red","royalblue","red") )+
  theme_minimal()+
  labs(
    x = "Modalidade",
    y = "Taxa de ocupação",
    caption = "Dados: ProjetoMobiliza Rio",
    title = "Taxa de ocupação",
    subtitle = "das vagas por modalidade no projeto Mobiliza Rio 06/03/2023"
  )+
  geom_curve(
    aes(x = 3, y = 1.4, xend = 5.5, yend = 1.3),
    curvature = -0.1,
    arrow = arrow(length = unit(0.03, "npc"))
  )+
  annotate("text", label = "Modalidade Muay Thai  \nteve 71 alunos e 50 \nvagas oferecidas.",
           x = 2, y = 1.35, size = 4, colour = "grey50",    family = "Bebas",fontface = "bold"
  ) + scale_y_continuous(
    breaks = seq(0,2,0.25)
  )+geom_hline(yintercept =1)+
  annotate("text", label = " Ocupação de 100%.",
           x = 1, y = 1.05, size = 3, colour = "grey50",    family = "Bebas",fontface = "bold"
  ) 


ggsave(
  "tt_columns.png",
  plot = tt_col,
  device = ragg::agg_png(
    width = 10,
    height = 10,
    units = "in",
    scaling = 0.8,
    res = 500
  )
)

vi = 107 
vf = 718
(vf-vi)/vi
vf/vi -1

107*5.71028
5.71028*100
