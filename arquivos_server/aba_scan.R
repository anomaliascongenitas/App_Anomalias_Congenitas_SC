
output$scan_result_texto_red <- renderUI({
  cid_selecionado <- as.numeric(input$cid_scan)
  
  resultado <- lista_completa[[cid_selecionado]][1]$MC_pvalue
  cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
  
  n <- length(cluster)
  texto <- str_c("<br>Distribuição: ",resultado$distribution," <br/>",
                 "Monte Carlo p-valor: ", resultado$MC_pvalue, "<br>",
                 "Duração de tempo considerada: ",resultado$MLC$duration," anos<br/>",
                 "Risco Relativo do cluster: ",round(resultado$MLC$relative_risk,3)," <br/>",
                 "Municípios que pertencem ao cluster: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
  
  HTML(texto)
})

output$scan_result_texto <- renderUI({
  cid_selecionado <- as.numeric(input$cid_scan)
  
  resultado <- lista_completa[[cid_selecionado]][1]$MC_pvalue
  cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
  
  n <- length(cluster)
  texto <- str_c("<br>Distribuição: ",resultado$distribution," <br/>",
                 "Tipo de estatística Scan: univariado"," <br/>",
                 "Número de Regiões consideradas: ",resultado$n_zones," <br>",
                 "Duração de tempo considerada: ",resultado$MLC$duration," anos<br/>",
                 "Risco Relativo do cluster: ",round(resultado$MLC$relative_risk,3)," <br/>",
                 "Número de Replicações de Monte Carlo: ",resultado$n_mcsim," <br>",
                 "Monte Carlo p-valor: ", resultado$MC_pvalue, "<br>",
                 "Gumbel p-valor: ",round(resultado$Gumbel_pvalue,5)," <br>",
                 "Municípios que pertencem ao cluster: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
  
  HTML(texto)
})

output$scan_cluster_texto <- renderText({
  cid_selecionado <- as.numeric(input$cid_scan)
  cluster <- lista_completa[[cid_selecionado]][2]$cluster$municipio
  n <- length(cluster)
  str_c("Os municípios que pertencem ao cluster são: ",paste(cluster[1:(n-1)],collapse=", ")," e ",cluster[n],".")
  
})

output$mapa_scan_cluster <- renderPlot({
  cid_selecionado <- as.numeric(input$cid_scan)
  
  resultado <- lista_completa[[cid_selecionado]][1]
  cluster <- lista_completa[[cid_selecionado]][2]
  banco <- lista_completa[[cid_selecionado]][3]
  banco_modelo <- lista_completa[[10]]
  
  banco$NOMEMUN <- unique(banco_modelo$NOMEMUN)
  banco$municipio  <- str_to_lower(banco$NOMEMUN)
  
  dataset <- merge(mapa_modelo, banco, by = "municipio", all.y = TRUE)# %>%
  dataset$variavel <- dataset$municipio %in% cluster$cluster$municipio

  dataset$variavel <- as.factor(dataset$variavel)
  
  level <- levels(dataset$variavel) <- c("Não pertence","Pertence")
  
  factpal <- colorFactor("plasma", dataset$variavel)
  
  grafico <- ggplot(dataset) + geom_sf(aes(fill = variavel ),alpha = 1) +
    scale_fill_manual(values = c(factpal(level[1]),factpal(level[2])), name= "Cluster") +
    theme_bw()
  
  grafico 
})






output$mapa_scan <- renderLeaflet({
  cid_selecionado <- as.numeric(input$cid_scan)
  banco <- lista_completa[[cid_selecionado]][3]
  banco_modelo <- lista_completa[[10]]
  
  banco$NOMEMUN <- unique(banco_modelo$NOMEMUN)
  banco$municipio  <- str_to_lower(banco$NOMEMUN)
  
  dataset <- merge(mapa_modelo, banco, by = "municipio", all.y = TRUE) %>%
    arrange(CD_GEOCMU)
  
  names(dataset)[7]=c("variavel")
  pal <- colorBin("plasma", domain =dataset$variavel, bins = seq(0,1,length.out = 7))
  
  tidy = st_as_sf(dataset)
  tidy <- st_transform(tidy, "+init=epsg:4326") 
  
  leaflet(tidy) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addPolygons(fillColor = ~pal(variavel), 
                weight = 1.5,
                opacity = 0.7,
                fillOpacity = 0.7,
                color = "gray",
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = sprintf("<strong>%s</strong><br/>Relative score:
                                  %s",
                                tidy$municipio, round(tidy$variavel,3)) %>%
                  lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "13px",
                  direction = "bottom")) %>%
    leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, title = "Relative score",
                       labFormat = labelFormat(digits = 3,big.mark = " "),
                       position = "bottomright") %>%
    addScaleBar(position = 'bottomleft')
})
