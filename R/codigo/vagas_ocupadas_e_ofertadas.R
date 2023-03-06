

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
