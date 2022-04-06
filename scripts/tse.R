# Informação Geral --------------------------------------------------------

# Projeto: Analise do mandato da Dep. Andreia de Jesus
# Alexandre Freitas
# Email: alexandre.freitas@fjp.mg.gov.br

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(abjutils)
library(geobr)
library(sf)
library(ggspatial)
library(ggrepel)

options(scipen=999)

# Abrir base de dados -----------------------------------------------------

votacao <- read.csv("data/votacao_secao_2018_MG/votacao_secao_2018_MG.csv", sep = ";", encoding = "latin1")


# Filtrar dados da a Dep -----------------------------------------------------------

dep_estadual_votacao <- votacao %>%
  filter(DS_CARGO == "DEPUTADO ESTADUAL")

andreia_de_jesus <- dep_estadual_votacao %>%
  filter(NR_VOTAVEL == 50130)


# Produzir informacoes --------------------------------------

andreia_de_jesus_mun <- andreia_de_jesus %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(total = sum(QT_VOTOS)) %>%
  mutate(soma = sum(total),
         percentual = total / soma) %>%
  select(-soma) %>%
  arrange(-total)

# Write xlsx
write.xlsx(andreia_de_jesus_mun, "products/votacao_municipal_andreia_de_jesus.xlsx", asTable = FALSE)


# Mapa --------------------------------------------------------------------

andreia_de_jesus_mun <- andreia_de_jesus_mun %>%
  mutate(NM_MUNICIPIO = rm_accent(NM_MUNICIPIO))

mun <- read_municipality(code_muni=31, year=2018)

mun <- mun %>%
  mutate(NM_MUNICIPIO = rm_accent(name_muni),
         NM_MUNICIPIO = str_to_upper(NM_MUNICIPIO))

andreia_de_jesus_mun_2 <- andreia_de_jesus_mun %>%
  mutate(NM_MUNICIPIO = ifelse(NM_MUNICIPIO == "OLHOS D AGUA", "OLHOS-D'AGUA", NM_MUNICIPIO)) %>%
  full_join(mun, by = "NM_MUNICIPIO") %>%
  mutate(percentual = ifelse(is.na(percentual), 0, percentual),
         percentual = percentual * 100,
         percentual = round(percentual, digits = 1))

teste <- andreia_de_jesus_mun %>%
  anti_join(mun, by = "NM_MUNICIPIO")

rm(teste)

andreia_de_jesus_mun_2 <- st_as_sf(andreia_de_jesus_mun_2)

andreia_de_jesus_mun %>%
  mutate(bh = ifelse(NM_MUNICIPIO == "BELO HORIZONTE", TRUE, FALSE)) %>%
  group_by(bh) %>%
  summarise(total = sum(total))

mapa_votacao <- andreia_de_jesus_mun_2 %>%
  ggplot() +
  geom_sf(aes(fill = percentual)) +
  scale_fill_distiller(palette = "RdPu", direction = 1) +
  geom_label_repel(data = filter(andreia_de_jesus_mun_2, percentual >= 2), aes(label = paste(name_muni, " ",percentual, "%", sep = ""), geometry = geom), label.size = 0.1,
                  stat = "sf_coordinates", min.segment.length = 0, box.padding = 2) +
  theme_bw() +
  labs(title = "Distribuição do percentual de votos da Dep. Andréia de Jesus - 2018",
       x = NULL,
       y = NULL,
       fill = "Percentual (%)",
       caption = "Foram indicados os municípios que representaram 2% ou mais do total de votos que a Deputada Andréia de Jesus teve em 2018.",)

mapa_votacao

ggsave("products/Votação da Deputada Andréia de Jesus - 2018.png", mapa_votacao, units = "px", width = 3000, height = 2800)

mapa_distancia <- cl_geral %>%
  ggplot() +
  geom_sf(color = "black") +
  geom_sf(data = cras_territorio, aes(fill = distancia_2),color = NA) +
  geom_sf(data = cras_geo, color = "red")