
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)

dados <- read.csv2(".../HomicidiosAL/2021/2021.csv", header = T, stringsAsFactors = F)

dados <- subset(dados, Cidade == "Maceió")

bairros <- read.csv(".../bairrosgeo.csv", header = T, stringsAsFactors = F)
populacao <- read.csv2(".../bairrospop.csv", header = T, stringsAsFactors = F)



dados %>%
  group_by(Bairro) %>%
  summarise(qtd=n()) %>%
  merge(populacao, by.x="Bairro", by.y="Bairro") %>%
  mutate(Taxa = 100000*qtd/Total) %>%
  arrange(desc(Taxa)) %>%
  select(Bairro, QtdHomicidios=qtd, Populacao=Total, Taxa) %>%
  merge(bairros, by.x="Bairro", by.y="nome", all.x=T) %>%
  mutate(lat=as.numeric(lat), long=as.numeric(long)) %>%
  mutate(Taxa=round(Taxa, 1)) %>%
  mutate(Texto=paste0("<b>", Bairro, "</b><br>",
                      "<i>Maceió - AL | 2021</i><hr>",
                      "<table><tr><td>",
                      "Quantidade de homicídios: ", 
                      "</td><td>", QtdHomicidios,
                      "</td></tr><td>",
                      "Taxa (100 mil hab.): ", 
                      "</td><td>", Taxa,
                      "</td></tr><td>",
                      "População: ", 
                      "</td><td>", Populacao,
                      "</td></tr></table>"
                      )) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  
  addCircleMarkers(
    group = "Quantidade de homicídios",
    lng=~long, lat=~lat, 
    radius = ~sqrt(QtdHomicidios)*4,
    weight = 1, color = "white",
    fillColor = "#aa2222", fillOpacity = .6,
    popup=~Texto) %>%
  addCircleMarkers(
    group = "Taxa por 100 mil hab.",
    lng=~long, lat=~lat, 
    radius = ~sqrt(Taxa)*2,
    weight = 1, color = "white",
    fillColor = "orange", fillOpacity = .6,
    popup=~Texto) %>%
  addLayersControl(baseGroups = c("Quantidade de homicídios", 
                                  "Taxa por 100 mil hab."),
                   options = layersControlOptions(collapsed = F))

