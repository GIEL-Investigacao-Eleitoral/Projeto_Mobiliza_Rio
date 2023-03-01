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


alunos[as.numeric(which(alunos$bairro == "Freguesia Jacarépagua")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Freguesia Jacarepaguá")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Jacarepaguá")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Freguesia")),'bairro'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$bairro == "Manguinho")),'bairro'] = 'Manguinhos'
alunos[as.numeric(which(alunos$bairro == "Cjt Nova Sepetiba")),'bairro'] = 'Nova Sepetiba'
alunos[as.numeric(which(alunos$bairro == "Santa Cruz Areia Branca")),'bairro'] = 'Santa Cruz'

alunos[as.numeric(which(alunos$bairro == "Vila Do Joao")),'bairro'] = 'Vila Do João'

            

table(alunos$bairro)


# http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html





alunos$bairro = gsub('Freguesia/Freguesia/Jacarepaguá','Freguesia/Jacarepaguá',alunos$bairro)
alunos$bairro = gsub('Freguesia/Freguesia/Jacarepaguá/Freguesia/Jacarepaguá','Freguesia/Jacarepaguá',alunos$bairro)
