lat=c(-23.0264287,-22.9936794,-22.9093494,-22.9333642,-22.9383432,
-22.8907391,-22.8938886,-22.9357211,-22.8707830,-22.9400941)

long=c(-43.5020194,-43.6108923,-43.2188184,-43.7036211,-43.2563192,
-43.3239208,-43.4419713,-43.3326451,-43.2485968,-43.2478114)

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
list.files("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/")
grade <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/grade para leaflet.xlsx") %>% clean_names()
head(grade)

names(grade)

pal <- colorFactor(c("red","blue","purple"), c("Dança/Ginástica","Futebol/Boxe/Judô","Muay-Thai/Dança"))

rotulos <- paste0("<b><a href='http://www.unirio.br/'>UNIRIO</a></b><br><br>",
                   "Núcleo: ",grade$nucleo,"<br>",
                   "Endereço: ", grade$endereco,"<br>",
                   "Coordenador: ",grade$coordenador,"<br>",
                   "Professor: ", grade$professor ,"<br>",
                   "Agente de Saúde: ", grade$agente_de_saude ,"<br>",
                   "Agente Educador: ", grade$agente_educador ,"<br>",
                   "Modalidade: ", grade$modalidade,"<br>",
                   "Dia: ", grade$dias_da_semana,"<br>",
                   "Horários: ", grade$x9,"<br>",
                   "Modalidade 2: ",grade$modalidade_2,"<br>",
                   "Dia: ", grade$dias_da_semana_2,"<br>",
                   "Horários: ", grade$horario_2,"<br>",
                   "<b>Nº de Beneficiarios: ",rpois(10,80),"</b><br><br>")


                                    )


df <- data.frame(long=long,lat=lat,rotulos=rotulos)

leaflet(df) %>%
  addTiles() %>%  
  #addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
  addCircles(lng=long[1],lat=lat[1],weight = 10, opacity = 0.5,label = HTML(rotulos[1]),popup =HTML(rotulos[1]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[2],lat=lat[2],weight = 10, opacity = 0.5,label = HTML(rotulos[2]),popup =HTML(rotulos[2]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[3],lat=lat[3],weight = 10, opacity = 0.5,label = HTML(rotulos[3]),popup =HTML(rotulos[3]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[4],lat=lat[4],weight = 10, opacity = 0.5,label = HTML(rotulos[4]),popup =HTML(rotulos[4]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[5],lat=lat[5],weight = 10, opacity = 0.5,label = HTML(rotulos[5]),popup =HTML(rotulos[5]),color = "blue",radius = 5.5, fill = T) %>%
  addCircles(lng=long[6],lat=lat[6],weight = 10, opacity = 0.5,label = HTML(rotulos[6]),popup =HTML(rotulos[6]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[7],lat=lat[7],weight = 10, opacity = 0.5,label = HTML(rotulos[7]),popup =HTML(rotulos[7]),color = "purple",radius = 5.5, fill = T) %>% 
  addCircles(lng=long[8],lat=lat[8],weight = 10, opacity = 0.5,label = HTML(rotulos[8]),popup =HTML(rotulos[8]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[9],lat=lat[9],weight = 10, opacity = 0.5,label = HTML(rotulos[9]),popup =HTML(rotulos[9]),color = "red",radius = 5.5, fill = T) %>%
  addCircles(lng=long[10],lat=lat[10],weight = 10, opacity = 0.5,label = HTML(rotulos[10]),popup =HTML(rotulos[10]),color = "blue",radius = 5.5, fill = T,
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




