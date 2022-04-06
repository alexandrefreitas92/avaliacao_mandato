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

tabela_andreia_de_jesus_mun <- andreia_de_jesus_mun %>%
  mutate(percentual = round(percentual * 100, digits = 1)) %>%
  rename(`Município` = NM_MUNICIPIO,
         `Votação` = total,
         `Percentual (%)` = percentual)

# Write xlsx
wb <- createWorkbook()
addWorksheet(wb, "Votação")
writeData(wb, "Votação", tabela_andreia_de_jesus_mun)
headerStyle <- createStyle(border = "TopBottom", textDecoration = c("BOLD"))
addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:3, gridExpand = TRUE)
setColWidths(wb, 1, cols = 1:3, widths = "auto")
saveWorkbook(wb, "products/votacao_municipal_andreia_de_jesus.xlsx", overwrite = TRUE)

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
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  labs(title = "Distribuição do percentual de votos da Dep. Andréia de Jesus - 2018",
       x = NULL,
       y = NULL,
       fill = "Percentual (%)",
       caption = "Foram indicados os municípios que representaram 2% ou mais do total de votos que a Deputada Andréia de Jesus teve em 2018.",)

mapa_votacao

ggsave("products/Votação da Deputada Andréia de Jesus - 2018.png", mapa_votacao, units = "px", width = 3000, height = 2800)
