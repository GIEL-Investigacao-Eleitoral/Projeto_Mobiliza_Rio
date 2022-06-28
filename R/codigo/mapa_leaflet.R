lat=c(-23.0264287,
-22.9936794,
-22.9093494,
-22.9333642,
-22.9383432,
-22.8907391,
-22.8938886,
-22.9357211,
-22.8707830,
-22.9400941)

long=c(-43.5020194,
-43.6108923,
-43.2188184,
-43.7036211,
-43.2563192,
-43.3239208,
-43.4419713,
-43.3326451,
-43.2485968,
-43.2478114)

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addMarkers(lng=long, lat=lat)
  addCircles(lng=long, lat=lat)
m  # Print the map

#-------------------------------------------------
# https://rstudio.github.io/leaflet/popups.html
#-------------------------------------------------
library(htmltools)
library(leaflet)
library(janitor)
library(readxl)
grade <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/Grade.xlsx") %>% clean_names()
head(grade)

rotulos <- paste0("Núcleo: ",grade$nucleo,"<br>",
                  "Bairro: ", grade$bairro, "<br>")


rotulos2 <- paste0("<b><a href='http://giel.uniriotec.br/?file=giel'>GIEL</a></b><br><br>",
                   "Núcleo: ",grade$nucleo,"<br>",
                   "Endereço: ", grade$endereco,"<br>",
                   "Bairro: ", grade$bairro, "<br>",
                   "Atividades: ", grade$atividade,"<br>",
                   "Dia: ", grade$dias,"<br>",
                   "Horários: ", grade$horarios)


df <- data.frame(long=long,lat=lat,rotulos=rotulos,rotulos2=rotulos2)
       
leaflet(df) %>%
  addTiles() %>%  
  addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  addCircles(lng=long, lat=lat,weight = 10, opacity = 0.5,label = HTML(rotulos),popup =HTML(rotulos),color = "red",radius = 5.5, fill = T)

#-------------------------------------------------

leaflet(df) %>%
  addTiles() %>%  
  addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  addCircles(lng=long[1],lat=lat[1],weight = 10, opacity = 0.5,label = HTML(rotulos[1]),popup =HTML(rotulos2[1]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[2],lat=lat[2],weight = 10, opacity = 0.5,label = HTML(rotulos[2]),popup =HTML(rotulos2[2]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[3],lat=lat[3],weight = 10, opacity = 0.5,label = HTML(rotulos[3]),popup =HTML(rotulos2[3]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[4],lat=lat[4],weight = 10, opacity = 0.5,label = HTML(rotulos[4]),popup =HTML(rotulos2[4]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[5],lat=lat[5],weight = 10, opacity = 0.5,label = HTML(rotulos[5]),popup =HTML(rotulos2[5]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[6],lat=lat[6],weight = 10, opacity = 0.5,label = HTML(rotulos[6]),popup =HTML(rotulos2[6]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[7],lat=lat[7],weight = 10, opacity = 0.5,label = HTML(rotulos[7]),popup =HTML(rotulos2[7]),color = "red",radius = 5.5, fill = T) %>% 
  addCircles(lng=long[8],lat=lat[8],weight = 10, opacity = 0.5,label = HTML(rotulos[8]),popup =HTML(rotulos2[8]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[9],lat=lat[9],weight = 10, opacity = 0.5,label = HTML(rotulos[9]),popup =HTML(rotulos2[9]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[10],lat=lat[10],weight = 10, opacity = 0.5,label = HTML(rotulos[10]),popup =HTML(rotulos2[10]),color = "red",radius = 5.5, fill = T)


#-------------------------------------------------

#pal <- colorFactor("OrRd", c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))

pal <- colorFactor(c("red","blue","purple"), c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))

leaflet(df) %>%
  addTiles() %>%  
  #addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
  addCircles(lng=long[1],lat=lat[1],weight = 10, opacity = 0.5,label = HTML(rotulos[1]),popup =HTML(rotulos2[1]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[2],lat=lat[2],weight = 10, opacity = 0.5,label = HTML(rotulos[2]),popup =HTML(rotulos2[2]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[3],lat=lat[3],weight = 10, opacity = 0.5,label = HTML(rotulos[3]),popup =HTML(rotulos2[3]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[4],lat=lat[4],weight = 10, opacity = 0.5,label = HTML(rotulos[4]),popup =HTML(rotulos2[4]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[5],lat=lat[5],weight = 10, opacity = 0.5,label = HTML(rotulos[5]),popup =HTML(rotulos2[5]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[6],lat=lat[6],weight = 10, opacity = 0.5,label = HTML(rotulos[6]),popup =HTML(rotulos2[6]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[7],lat=lat[7],weight = 10, opacity = 0.5,label = HTML(rotulos[7]),popup =HTML(rotulos2[7]),color = "purple",radius = 5.5, fill = T) %>% 
  addCircles(lng=long[8],lat=lat[8],weight = 10, opacity = 0.5,label = HTML(rotulos[8]),popup =HTML(rotulos2[8]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[9],lat=lat[9],weight = 10, opacity = 0.5,label = HTML(rotulos[9]),popup =HTML(rotulos2[9]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[10],lat=lat[10],weight = 10, opacity = 0.5,label = HTML(rotulos[10]),popup =HTML(rotulos2[10]),color = "blue",radius = 5.5, fill = T) %>%  
  addLegend('bottomright',
            title = "Modalidade",
            pal = pal,
            values = c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))



#-------------------------------------------------

pal <- colorFactor(c("red","blue","purple"), c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))



rotulos3 <- paste0("<b><a href='http://www.unirio.br/'>UNIRIO</a></b><br><br>",
                   "<b>Percentual de satisfação: ",rpois(10,80),"</b><br><br>",
                   "Núcleo: ",grade$nucleo,"<br>",
                   "Endereço: ", grade$endereco,"<br>",
                   "Bairro: ", grade$bairro, "<br>",
                   "Atividades: ", grade$atividade,"<br>",
                   "Dia: ", grade$dias,"<br>",
                   "Horários: ", grade$horarios)


df <- data.frame(long=long,lat=lat,rotulos=rotulos,rotulos2=rotulos2)


leaflet(df) %>%
  addTiles() %>%  
  #addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
  addCircles(lng=long[1],lat=lat[1],weight = 10, opacity = 0.5,label = HTML(rotulos[1]),popup =HTML(rotulos3[1]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[2],lat=lat[2],weight = 10, opacity = 0.5,label = HTML(rotulos[2]),popup =HTML(rotulos3[2]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[3],lat=lat[3],weight = 10, opacity = 0.5,label = HTML(rotulos[3]),popup =HTML(rotulos3[3]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[4],lat=lat[4],weight = 10, opacity = 0.5,label = HTML(rotulos[4]),popup =HTML(rotulos3[4]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[5],lat=lat[5],weight = 10, opacity = 0.5,label = HTML(rotulos[5]),popup =HTML(rotulos3[5]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[6],lat=lat[6],weight = 10, opacity = 0.5,label = HTML(rotulos[6]),popup =HTML(rotulos3[6]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[7],lat=lat[7],weight = 10, opacity = 0.5,label = HTML(rotulos[7]),popup =HTML(rotulos3[7]),color = "purple",radius = 5.5, fill = T) %>% 
  addCircles(lng=long[8],lat=lat[8],weight = 10, opacity = 0.5,label = HTML(rotulos[8]),popup =HTML(rotulos3[8]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[9],lat=lat[9],weight = 10, opacity = 0.5,label = HTML(rotulos[9]),popup =HTML(rotulos3[9]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[10],lat=lat[10],weight = 10, opacity = 0.5,label = HTML(rotulos[10]),popup =HTML(rotulos3[10]),color = "blue",radius = 5.5, fill = T,
             labelOptions = labelOptions(style = list(
               "color" = "red",
               "font-family" = "serif",
               "font-style" = "bold",
               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
               "font-size" = "15px",
               "border-color" = "rgba(0,0,0,0.5)"))) %>%  
  addLegend('bottomright',
            title = "Modalidade",
            pal = pal,
            values = c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))




