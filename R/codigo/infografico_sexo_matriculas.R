library(dplyr)
library(janitor)
library(readxl)

MobilizaBD <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/MobilizaBD.xlsx") %>% clean_names()
head(MobilizaBD)
names(MobilizaBD)

MobilizaBD %>% sum(fem)

sum(MobilizaBD$fem)
sum(MobilizaBD$mas)
tabela_matric = data.frame(sexo=c('Feminino','Masculino'),qtd=c(509,209))


tabela_matric = tabela_matric  %>% 
  mutate(percentual = paste0(round(qtd/sum(qtd) * 100, 1), "%"),
         label = c("Feminino", "Masculino"),
         test = "test" ) 

p.title <- "Gênero das matrículas"
col.mov <- c('blue','red')

# plot --------------------------------------------------------------------

g_plot2 <-
  ggplot(tabela_matric, aes(y = test, x = qtd)) +
  geom_col(aes(fill = forcats::fct_rev(sexo)), 
           show.legend = F) +
  scale_fill_manual(values = col.mov) +
  geom_richtext(
    aes(
      x = c(50, 515),
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
      x = c(50, 515),
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
       caption = "Fonte: Mobiliza Rio") 

g_plot2


tabela2_matric = MobilizaBD %>% select(nucleo,mas,fem) %>% group_by(nucleo) %>% summarise(MASCULINO=sum(mas),FEMININO=sum(fem))
tabela2_matric

library(tidyr)
tabela2_matric %>% pivot_longer(everything())
tabela2_matric = tabela2_matric %>% pivot_longer(MASCULINO:FEMININO,names_to = "sexo")

tabela2_matric$nucleo = paste0('0',tabela2_matric$nucleo) 
tabela2_matric$nucleo = gsub('010','10',tabela2_matric$nucleo) 


mutate(tabela2_matric, nucleo ) %>%
  ggplot(aes(x = ifelse(sexo == "MASCULINO", value, -value),
             y = nucleo,
             fill = sexo)) +
  geom_col() +
  scale_fill_manual(values = c('red','blue')) +
  xlab("Quantidade de matrícula")+
  ylab("Núcleo")+ theme_minimal()

