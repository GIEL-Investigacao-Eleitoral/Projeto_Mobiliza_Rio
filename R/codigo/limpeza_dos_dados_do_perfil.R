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
                     sheet = "alunos(1)")
names(alunos)

alunos = alunos[,c(1,2,5,7,8,27:34,83:87)]
alunos = alunos[-1,]

alunos$NASCIMENTO[2]
#as.POSIXct.Date(Sys.Date())-alunos$NASCIMENTO[2]

trunc(as.numeric(difftime(as.POSIXct.Date(Sys.Date()), alunos$NASCIMENTO[2], units = "days")) / 365.25)
alunos$idade = trunc(as.numeric(difftime(as.POSIXct.Date(Sys.Date()), alunos$NASCIMENTO, units = "days")) / 365.25)
max(alunos$idade, na.rm = TRUE)
min(alunos$idade, na.rm = TRUE)

alunos$idade = ifelse(alunos$idade<1,NA,alunos$idade)

library(stringr)
alunos$BAIRRO = str_to_title(alunos$BAIRRO)
table(alunos$BAIRRO)

# Freguesia,Freguesia Jacarepaguá, Freguesia Jacarépagua
# MELHOR QUE O IF_ELSE
 
alunos$BAIRRO = gsub('Tijuca 20530420','Tijuca',alunos$BAIRRO)
alunos$BAIRRO = gsub('Praca Da Bandeira', 'Praça Da Bandeira',alunos$BAIRRO)

alunos$BAIRRO = gsub('Recreio Dos Bandeitantes','Recreio Dos Bandeirantes',alunos$BAIRRO)
alunos$BAIRRO = gsub('Recreio Bandeirantes','Recreio Dos Bandeirantes',alunos$BAIRRO)
alunos$BAIRRO = gsub('Recreio Dos Bandeirantes','Recreio',alunos$BAIRRO)

alunos$BAIRRO = gsub('Realengp','Realengo',alunos$BAIRRO)
alunos$BAIRRO = gsub('Realengo Barata','Realengo',alunos$BAIRRO)
alunos$BAIRRO = gsub('Relengo','Realengo',alunos$BAIRRO)

alunos$BAIRRO = gsub('Quintino Bocaiúva','Quintino',alunos$BAIRRO)
alunos$BAIRRO = gsub('Quintino Bocaiuva','Quintino',alunos$BAIRRO)
alunos$BAIRRO = gsub('Quintino','Quintino Bocaiúva',alunos$BAIRRO)


alunos$BAIRRO = gsub('Pedra De Guaratiba Piraquê','Pedra De Guaratiba',alunos$BAIRRO) 
alunos$BAIRRO = gsub('Piraque Pedra De Guaratiba','Pedra De Guaratiba',alunos$BAIRRO)          
alunos$BAIRRO = gsub('Piraque','Pedra De Guaratiba',alunos$BAIRRO) 

alunos[as.numeric(which(alunos$BAIRRO == "Maré")),'BAIRRO'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$BAIRRO == "Mare")),'BAIRRO'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$BAIRRO == "Maré Bonsucesso")),'BAIRRO'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$BAIRRO == "Bonsucesso/ Maré")),'BAIRRO'] = 'Bonsucesso/Maré'
alunos[as.numeric(which(alunos$BAIRRO == "Bonsucesso")),'BAIRRO'] = 'Bonsucesso/Maré'

alunos[as.numeric(which(alunos$BAIRRO == "Freguesia Jacarépagua")),'BAIRRO'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$BAIRRO == "Freguesia Jacarepaguá")),'BAIRRO'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$BAIRRO == "Jacarepaguá")),'BAIRRO'] = 'Freguesia/Jacarepaguá'
alunos[as.numeric(which(alunos$BAIRRO == "Freguesia")),'BAIRRO'] = 'Freguesia/Jacarepaguá'

table(alunos$BAIRRO)

# http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html

alunos$BAIRRO = gsub('[Freguesia Jacarepaguá]','',alunos$BAIRRO)
alunos$BAIRRO = gsub('[Freguesia Jacarépagua]','Freguesia/Jacarepaguá',alunos$BAIRRO)
alunos$BAIRRO = gsub('Freguesia','Freguesia/Jacarepaguá',alunos$BAIRRO)
alunos$BAIRRO = gsub('Jacarepaguá','Freguesia/Jacarepaguá',alunos$BAIRRO)


Freguesia

 
Jacarepaguá



alunos$BAIRRO = gsub('Freguesia/Freguesia/Jacarepaguá','Freguesia/Jacarepaguá',alunos$BAIRRO)
alunos$BAIRRO = gsub('Freguesia/Freguesia/Jacarepaguá/Freguesia/Jacarepaguá','Freguesia/Jacarepaguá',alunos$BAIRRO)
