
Bairros <- rgdal::readOGR("C:/Users/Hp/Documents/GitHub/Projeto_Mobiliza_Rio/dados/original/Limite_de_Bairros.geojson") 
head(Bairros@data)

library(leaflet)
pal <- colorNumeric("viridis", NULL)
pal <- colorQuantile("viridis",as.numeric(Bairros@data$CODBAIRRO))

leaflet(Bairros) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(as.numeric(CODBAIRRO)),
              label = ~paste0(NOME) )
