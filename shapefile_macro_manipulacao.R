banco_macro_saude_analise_aux

mapa$CODMUNRES <- as.numeric(substring(mapa$CD_GEOCMU ,1,last = 6))


mapa_macro_aux <- left_join(mapa,banco_macro_saude_analise_aux,by = c("CODMUNRES")) %>%
  select(macro_cod)


macros_mapa <- mapa_macro_aux %>%
  group_by(macro_cod) %>% 
  summarize(geometry = st_union(geometry))


#plot(macros_mapa)


sf::st_write(macros_mapa,localarquivo("shapefiles/macro_saude_sc/macro_saude_sc.shp"), quiet = TRUE)












banco_aux2 <- banco_anomalias_analise %>%
  filter(ANO_NASC  == 2018) %>%
  merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))

banco_aux2 <- banco_aux2 %>%
  group_by(macro_cod,macrorregiao) %>%
  summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
            prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
  ungroup() 
banco_aux2

aux <- banco_aux2
names(aux)[2] = "Nome"
names(aux)[5] = "variavel"
names(aux)[1] = "cod"
dataset <- aux
limites <-
  c(round(min(dataset$variavel), 0) - 1, round(max(dataset$variavel), 0) +
      1)

pal <- colorBin("plasma",domain = dataset$variavel, bins = seq(limites[1], limites[2], length.out = 6))
pal2 <- function(x) {
  ifelse(x == 0, "#808080", pal(x))
}
tidy <- dataset %>%
  left_join(macro_saude_shape,
            by = c("cod" = "macro_cod"))
tidy = st_as_sf(tidy)

tidy <- tidy %>%  st_transform()

leaflet(tidy) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(
    fillColor = ~ pal(variavel),
    weight = 1.5,
    opacity = 0.7,
    fillOpacity = 0.7,
    color = "gray",
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = sprintf(
      "<strong>%s</strong><br/>Prevalência ao nascimento:
                                  %s</strong><br/>Número nascidos vivos: %s<br/>Número nascidos vivos com anomalia: %s",
      tidy$Nome,
      round(tidy$variavel, 3),
      tidy$numero_nascidos_vivos,
      tidy$nascidos_vivos_anomalia
    ) %>%
      lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "6px 11px"),
      textsize = "13px",
      opacity = 0.75,
      direction = "bottom"
    )
  ) %>%
  leaflet::addLegend(
    pal = pal,
    values = ~ tidy$variavel,
    opacity = 0.7,
    title = "Prevalência ao nascimento",
    labFormat = labelFormat(digits = 3, big.mark = " "),
    position = "bottomright"
  ) %>%
  addScaleBar(position = 'bottomleft')
