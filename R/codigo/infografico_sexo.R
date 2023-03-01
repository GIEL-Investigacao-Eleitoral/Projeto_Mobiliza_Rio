table(alunos$SEXO)


# load packages ----------------------------------------------------------
library(tidyverse)
library(here)
library(ragg)
library(ggtext)


# load data ---------------------------------------------------------------

movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


# prepare data to plot ----------------------------------------------------

tabela <- 
  alunos %>% 
  group_by(SEXO) %>% 
  summarise(n = n()) %>% 
  mutate(percentual = paste0(round(n/sum(n) * 100, 1), "%"),
         label = c("Feminino", "Masculino"),
         test = "test", 
         SEXO = factor(SEXO, levels = c("FEMININO", "MASCULINO"))) 
tabela


p.title <- "Gênero dos participantes"
p.description <- 
  "Gênero dos participantes a partir do cadastro:
1 - 336 pessoas declararam sero do gênero feminino;
2 - 128 pessoas declararam sero do gênero masculino;
3 - data/período de coleta: 03/2022 até 03/2023
"
col.mov <- c("#0e2f44", "#daa520")


# plot --------------------------------------------------------------------

g_plot <-
  ggplot(tabela, aes(y = test, x = n)) +
  geom_col(aes(fill = forcats::fct_rev(SEXO)), 
           show.legend = F) +
  scale_fill_manual(values = col.mov) +
  geom_richtext(
    aes(
      x = c(50, 340),
      y = rep(1.3, 2),
      label = percentual
    ),
    fill = NA, 
    label.color = NA,
    family = "Bebas",
    color = "white", 
    hjust = 0,
    size = 10
  ) +
  geom_richtext(
    aes(
      x = c(50, 340),
      y = rep(1.15, 2),
      label = label
    ),
    fill = NA, 
    label.color = NA,
    family = "Bebas",
    color = "white", 
    hjust = 0,
    size = 6
  ) +
  theme_void() +
  labs(title = p.title,
       subtitle = p.description, 
       caption = "Fonte: Grupo de Investigação Eleitoral - GIEL http://giel.uniriotec.br") +
  theme(
    plot.title = element_text(
      family = "Inter", 
      size = 28, 
      hjust = 0.5,
      margin = margin(t = 10, b = 10)
    ),
    plot.subtitle = element_text(
      family = "Inter", 
      size = 10,
      margin = margin(b = 0)
    ),
    plot.caption = element_text(
      family = "Inter", 
      size = 7
    ),
    plot.margin = margin(10,20,5,20),
    plot.background = element_rect(fill = "grey95", color = NA)
  )

g_plot

ggsave('C:/Users/Hp/Desktop/Borba Mobiliza Rio/mobiliza_rio.png',
       width = 6,
       height = 5,
       units = "in",
       dpi = 300)
# save progress -----------------------------------------------------------

agg_png('C:/Users/Hp/Desktop/Borba Mobiliza Rio/mobiliza_rio.png',
   width = 6,
   height = 5,
   units = "in",
   res = 300
 )
dev.off()

