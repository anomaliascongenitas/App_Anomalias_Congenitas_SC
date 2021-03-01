#library(profvis)
#library(Rcpp)
library(readxl)
#library(RColorBrewer) ??
#library(dplyr)
#library(reshape2)
#library(dygraphs)
#library(xts)
#library(ggrepel)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(DT)
library(leaflet)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(sf)
library(ps)
library(spdep) #######?###########
library(kableExtra)

library(viridis)

#library(haven)
library(ggbeeswarm)


options(OutDec= ",") #Muda de ponto para virgula nos decimais! 


#options(OutDec= ".") #Muda de ponto para virgula nos decimais! 


localarquivo <- function(x){
  #str_c(here::here(),"/",x)
  x
}



banco_nascimentos <- utils::read.csv2(localarquivo("nascidos_vivos_sc.csv")) %>%
  rename(numero_nascidos_vivos = nascidos_vivos) %>%
  filter(CODMUNRES != 420000) %>%
  replace_na(list(numero_nascidos_vivos = 0))

banco_cid <- utils::read.csv(file= localarquivo("banco_anomalias_2010-2019_sc.csv")) %>%
  rename(ANO_NASC = ANONASC)



banco_aux <- banco_cid %>%
  # filter(cid_num %in% 1:9) %>%
  group_by(NUMERODN) %>%
  summarise(ANO_NASC = unique(ANO_NASC),CODMUNRES = unique(CODMUNRES),nascidos_vivos_anomalia = 1)

banco_aux2 <- banco_aux %>%
  group_by(ANO_NASC,CODMUNRES) %>%
  summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))


banco_aux3 <- banco_nascimentos %>%
  left_join(banco_aux2,by  = c("CODMUNRES","ANO_NASC" = "ANO_NASC")) %>%
  mutate(nascidos_vivos_anomalia = replace_na(nascidos_vivos_anomalia, 0),prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4) %>%
  mutate(prevalencia = ifelse(is.nan(prevalencia),0,prevalencia)) # Pinto Bandeira 2010-2012



banco_anomalias_analise <- banco_aux3 %>% select(1,2,4,3,5,6)

banco_anomalias_analise$municipio <- str_to_lower(banco_anomalias_analise$NOMEMUN)

remove(banco_aux,banco_aux2,banco_aux3)

#



mapa <- sf::st_read(localarquivo("shapefiles/sc_municipios/42MUE250GC_SIR.shp"), quiet = TRUE) %>%
  mutate(municipio= str_to_lower(NM_MUNICIP)) 


############################################################################
### Intervalos cores mapa prevalencia
#Prevalencia
linha_ano_intervalo_defalt = which(banco_anomalias_analise$prevalencia==
                                     max(banco_anomalias_analise$prevalencia,na.rm = TRUE))
ano_intervalo_defalt = banco_anomalias_analise$ANO_NASC[linha_ano_intervalo_defalt][1]
variavel_intervalo = banco_anomalias_analise %>% 
  filter(ANO_NASC == ano_intervalo_defalt) %>%
  select(prevalencia)
bins_defalt = classInt::classIntervals(var = variavel_intervalo[[1]], n = 5, style = "fisher")

### nascidos_vivos_anomalia
linha_ano_intervalo_defalt_nascidos_vivos_anomalia = which(banco_anomalias_analise$nascidos_vivos_anomalia==
                                              max(banco_anomalias_analise$nascidos_vivos_anomalia))
ano_intervalo_defalt_nascidos_vivos_anomalia = banco_anomalias_analise$ANO_NASC[linha_ano_intervalo_defalt_nascidos_vivos_anomalia]
variavel_intervalo = banco_anomalias_analise %>% 
  filter(ANO_NASC == ano_intervalo_defalt_nascidos_vivos_anomalia) %>%
  select(nascidos_vivos_anomalia)


bins_defalt_nascidos_vivos_anomalia = classInt::classIntervals(var = variavel_intervalo[[1]], n = 5, style = "fisher")










####################################################################################
# ## Moran 
banco_i_moran_matriz = banco_anomalias_analise %>%
  filter(ANO_NASC == 2019) %>%
  right_join(mapa)
# 
# 
w <- poly2nb(banco_i_moran_matriz$geometry, row.names=banco_i_moran_matriz$municipio)
matriz_w <-  nb2listw(w, style='B') #faz a matriz de pesos 0 ou 1
teste_moran <- read.table(localarquivo("teste_moran.txt"), quote="\"", comment.char="")
names(teste_moran)=c("estatistica_teste", "p_valor", "ano_teste")
#####################################################################################################

top_20_munic <- banco_nascimentos %>%
  group_by(NOMEMUN) %>%
  summarise(Total = sum(numero_nascidos_vivos)) %>%
  arrange(desc(Total)) %>%
  top_n(20, Total) %>%
  select(NOMEMUN)

tabela_box <- banco_anomalias_analise %>%
  group_by(ANO_NASC) %>%
  summarise(total_nascidos_vivos = sum(numero_nascidos_vivos),
            total_anomalias = sum(nascidos_vivos_anomalia),
            prevalencia  = total_anomalias / total_nascidos_vivos*10^4)





banco_macro_saude <- read.csv(localarquivo("sc_divisao_macro_saude.csv")) %>%
  rename(macrorregiao = macro)


banco_macro_saude$IBGE <- substring(banco_macro_saude$IBGE,1,6)


macro_saude_shape <- sf::st_read(localarquivo("shapefiles/macro_saude_sc/macro_saude_sc2.shp"), quiet = TRUE) 
macro_saude_shape <- macro_saude_shape %>%
  select(macroregiao = Secondary,macroregiao_num = Primary.ID,geometry)
macro_saude_shape$macroregiao_num <- c(6,7,2,1,5,3,4)

banco_macro_saude_analise_aux <- banco_anomalias_analise  %>%
  merge(.,banco_macro_saude,by.x=c("CODMUNRES"),by.y = c("IBGE"))


banco_macro_saude_analise <- banco_macro_saude_analise_aux  %>%
  group_by(macro_cod,ANO_NASC) %>%
  summarise(numero_nascidos_vivos = sum(numero_nascidos_vivos), nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia),
            prevalencia = nascidos_vivos_anomalia/numero_nascidos_vivos*10^4,macrorregiao) %>%
  ungroup() %>%
  select(macrorregiao = macro_cod,2,3,4,5,macro_label = "macrorregiao")


mapa_modelo <- mapa




load("scan_app.RData")











