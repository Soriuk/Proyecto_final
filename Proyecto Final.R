# Carga de librerias
library(sf)
library(raster)
library(dplyr)
library(spData)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(plotly)
library(DT)
library(tidyr)
library(flexdashboard)
library(formattable)

# Carga de datos
orqui <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )
# Proyección
st_crs(orqui) = 4326

# Carga de cantones y provincias
cantones <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
  )
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
    quiet = TRUE
  )

# Junte espacial
orqui <- 
  orqui %>%
  st_join(cantones["canton"]) %>%
  st_join(provincias["provincia"])

#Limpieza de los datos

orqui <-
  orqui %>%
  mutate(eventDate = as.Date(eventDate, "%Y-%m-%d")) %>%
  filter(!is.na(coordinateUncertaintyInMeters) &
           coordinateUncertaintyInMeters <= 1000)

# Tabla de registros

# Tabla de registros de presencia
orqui %>%
  st_drop_geometry() %>%
  dplyr::select(species, eventDate ,stateProvince, locality) %>%
  datatable(
    colnames = c("Especies","Fecha","Provincia","Cantón"),
    options = list(
      searchHighlight = TRUE,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    )
  )

# Estiquetas
orqui_or <- 
  orqui %>%
  st_drop_geometry() %>%
  summarise(n(), especies = n_distinct(species, na.rm = TRUE), genero = n_distinct(genus, na.rm = TRUE)) %>%
  dplyr::rename(total_especies = "n()")

# Especies con más registros

orqui$species[orqui$species == ""] <- "N/A"



orqui_top <- data.frame(table(orqui$species))

orquideas <- orqui_top%>%  mutate(Var1 = case_when(
  .$Freq <= 117 ~ "Otros",  .$Freq == 7481  ~ "Otros"))
orquideas <- aggregate(orquideas$Freq, by=list(Nombre=orquideas$Var1), FUN=sum)
orquideas  <- dplyr::rename(orquideas,Cantidad = "x",) 
orquideas1 <- orqui_top%>%filter(Freq >= 118 & Var1 != "N/A") %>% dplyr::rename(Cantidad = "Freq",Nombre = "Var1")
orquideas1 <- orquideas1[with(orquideas1,order(-orquideas1$Cantidad )),]
total <- full_join(orquideas1, orquideas,by=c("Cantidad","Nombre"))


# Grafico de pastel
orquideas1 %>%
  plot_ly(
    labels = ~ Nombre,
    values = ~ Cantidad,
    type = "pie",
    textposition = "inside",
    textinfo = "label+percent"
  ) %>%
  config(locale = "es") %>%
  layout(
    title = "Registros por especie de orquideas",
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )


# Conjunto de datos de especies por provincia

orqui_species <-
  provincias %>%
  st_join(orqui) %>%
  group_by(provincia.x) %>%
  summarize(especies = n())

# paleta de colores

colores_especies <-
  colorNumeric(palette = "RdPu",
               domain = orqui_species$especies,
               na.color = "transparent")
# Mapa leaftlet

orqui %>%
  select(species,
         canton,
         stateProvince,
         eventDate) %>%
  leaflet() %>%
  setView(lng = -84.0,
          lat = 10.0,
          zoom = 8) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
  addPolygons(
    data = orqui_species,
    fillColor = ~ colores_especies(orqui_species$especies),
    fillOpacity = 0.3,
    stroke = TRUE,
    color = "black",
    weight = 1,
    group = "Datos por provincias"
  ) %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = "#c90094",
    fillOpacity = 1,
    popup = paste(
      paste("<strong>Localidad:</strong>",
            orqui$stateProvince),
      paste("<strong>Cantidad de especies:</strong>",
            orqui$species),
      paste("<strong>Cantón:</strong>",
            orqui$canton),
      paste("<strong>Fecha:</strong>",
            orqui$eventDate),
      sep = '<br/>'
    ),
    clusterOptions = markerClusterOptions(),
    group = "Datos de orquideas"
  ) %>%
  addLayersControl(
    baseGroups = c("Esri.WorldGrayCanvas", "OpenStreetMap"),
    overlayGroups = c("Registros por provincias", "Datos de orquideas")
  ) %>%
  addResetMapButton() %>%
  addSearchOSM() %>%
  addMouseCoordinates() %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>%
  addMiniMap(
    tiles = providers$Esri.WorldGrayCanvas,
    position = "bottomright",
    toggleDisplay = TRUE
  ) %>%
  addLegend(
    position = "bottomleft",
    values = orqui_species$especies,
    pal = colores_especies,
    group = "Registros por provincias",
    title = "Cantidad de especies de orquideas"
  )



# Capa leaflet raster
# Obtención de la capa de altitud
alt <-
  raster::getData(
    "worldclim",
    var = "alt",
    res = 0.5,
    lon = -84,
    lat = 10
  )
# Reproyección de la capa de altitud a WGS84
alt <-
  alt %>%
  projectRaster(crs = 4326)

# Recorte de la capa de altitud
altitud <-
  alt %>%
  crop(provincias) %>%
  mask(provincias)

# Plantilla del raster
plant_rast <-
  altitud %>%
  aggregate(fact = 10)

# Rasterización
registro_orqui_rast <-
  rasterize(orqui,
            plant_rast,
            field = 1,
            fun = "count")
# Visualización de la Rasterización
plot(
  registro_orqui_rast,
  ext = extent(280000, 660000, 880000, 1250000),
  main = "Cantidad de registros de orquideas",
  axes = TRUE
)
plot(provincias$geometry,
     add = TRUE)
# Paleta de colores
pallet <-
  colorNumeric(
    c("#FF8C00", "#BDB76B", "#FFD700", "#CD5C5C", "#FA8072", "#DC143C"),
    values(registro_orqui_rast), 
    na.color = "transparent"
  )
# Mapa de registros de presencia
leaflet() %>%
  setView(lng = -84.0, lat = 10.0, zoom = 8) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', group = "Google") %>%
  addPolygons(
    data = provincias,
    fillColor = FALSE,
    fillOpacity = 0,
    stroke = TRUE,
    color = "black",
    weight = 1,
    group = "Límite de provincia" 
  ) %>%
  addRasterImage(
    registro_orqui_rast,
    colors = pallet,
    opacity = 1,
    group = "Registros de orquídeas"
  ) %>%
  addLayersControl(
    baseGroups = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Google"),
    overlayGroups = c("Límite de provincia", "Registros de orquídeas")
  ) %>%
  addResetMapButton() %>%
  addSearchOSM() %>%
  addMouseCoordinates() %>%
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) %>%
  addMiniMap(
    tiles = providers$Esri.WorldGrayCanvas,
    position = "bottomright",
    toggleDisplay = TRUE
  ) %>% 
  addLegend(
    pal = pallet,
    values = values(registro_orqui_rast),
    position = "bottomleft",
    title = "Cantidad de especies por celda",
    group = "Registros de orquídeas"
  )
