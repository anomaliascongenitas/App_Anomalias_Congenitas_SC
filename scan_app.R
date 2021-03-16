library(scanstatistics)
library(ggplot2)
library(dplyr)
#library(sp)
#library(magrittr)
library(stringr)
library(INLA)
#library(rlist)

## here deve estar localizado na pasta principal onde esta o aplicativo Shiny (arquivo global.r)
localarquivo <- function(x){
  str_c(here::here(),"/",x)
}
source(str_c(here::here(),"/","global.r"),encoding = "UTF-8")


gerar_banco_modelo_aux <- function(cid2){
  
  banco_aux <- banco_cid %>%
    filter(cid_num %in% cid2) %>%
    group_by(ANO_NASC,CODMUNRES) %>%
    summarise(nascidos_vivos_anomalia = n())
  
  banco_aux2 <- banco_nascimentos %>%
    left_join(banco_aux,by=c("CODMUNRES","ANO_NASC")) 
  
  
  banco_aux2 <- banco_aux2 %>%
    replace_na(list(nascidos_vivos_anomalia = 0))%>%
    arrange(CODMUNRES)

  
  return(banco_aux2)
}


mapa_modelo <- sf::st_read(localarquivo("shapefiles/sc_municipios/42MUE250GC_SIR.shp"), quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP))

mapa_modelo$CD_GEOCMU <- as.numeric(substring(mapa_modelo$CD_GEOCMU,1,6))


lista_completa <- list()






for (i in 1:9) {
  banco_modelo <- gerar_banco_modelo_aux(i)%>%
    arrange(CODMUNRES)
  
  
  counts <- banco_modelo  %>% 
    select(CODMUNRES,ANO_NASC,nascidos_vivos_anomalia) %>%
    df_to_matrix(time_col = "ANO_NASC", location_col = "CODMUNRES", value_col = "nascidos_vivos_anomalia")
  
  
  
  
  banco_modelo$municipio <- str_to_lower(banco_modelo$NOMEMUN) 
  
  banco_modelo_aux <- banco_modelo %>%
    group_by(CODMUNRES) %>%
    summarise(municipio = str_to_lower(unique(NOMEMUN)))
  
  
 
  
  banco_modelo_mapa <-  banco_modelo_aux %>%
    left_join(mapa_modelo,by=c("CODMUNRES" = "CD_GEOCMU"))%>%
    select(-3,-5) %>%
    rename(municipio = municipio.x)
  
  
  
  banco_modelo_mapa <- banco_modelo_mapa %>%
    arrange(CODMUNRES)
  
  banco_modelo_mapa_sf  = st_as_sf(banco_modelo_mapa)
  banco_modelo_mapa_sf  <- st_transform(banco_modelo_mapa_sf )
  nb <- poly2nb(banco_modelo_mapa_sf)
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = localarquivo("map.adj"))
  
  
  banco_modelo_mapa_sf$municipio
  
  proporcao_media <- banco_modelo %>%
    group_by(ANO_NASC) %>%
    summarise(valor = sum(nascidos_vivos_anomalia)/sum(numero_nascidos_vivos))
  
  
  banco_modelo$valor_esperado <- banco_modelo$numero_nascidos_vivos*proporcao_media$valor[banco_modelo$ANO_NASC - 2009]
  banco_modelo$valor_esperado[banco_modelo$valor_esperado == 0] = 0.001
  
  
  ebp_baselines <- banco_modelo  %>% 
    df_to_matrix(time_col = "ANO_NASC", location_col = "CODMUNRES", value_col = "valor_esperado")
  
  
  
  set.seed(1)
  poisson_result <- scan_eb_poisson(counts = counts, 
                                    zones = g$nbs , #max_duration = 3,
                                    baselines = ebp_baselines,
                                    n_mcsim = 9999)
  print(poisson_result)
  
  
  
  
  
  lista2 <- banco_modelo_mapa %>%
    filter(CODMUNRES %in% colnames(counts[,c(poisson_result$MLC$zone_number,poisson_result$MLC$locations)])) %>%
    select(CODMUNRES,municipio = municipio)
  

  
  county_scores <- score_locations(poisson_result, g$nbs)
  lista_completa[[i]] <- list(MC_pvalue = poisson_result,cluster = lista2,relative_score = county_scores)
}





lista_completa[[10]] <- banco_modelo




save(lista_completa, file="scan_app.RData")


load("scan_app.RData")
