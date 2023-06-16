



# Miscelânia ------------------------------------------------------------------

library(tidyverse)


# 1) Despesa Educ vs Ideb


f <- read_rds('data-raw/finbra_mun.rds') |>
  filter(ano == 2019, estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor)

d <- read_rds('data-raw/ideb.rds') |>
  filter(ano == 2019, sigla_uf == 'CE', rede == 'municipal') |>
  select(ano, id_municipio, ideb)


p <- read_rds('data-raw/pop.rds') |>
  filter(ano == 2019) |>
  select(-sigla_uf)

# purrr ----

# df <- left_join(x = d, y = f, by = c('ano', 'id_municipio')) |>
#   left_join(y = p, by = c('ano', 'id_municipio'))


bases <- list(f, d, p)




df <- reduce(bases, left_join) |>
  mutate(desp_per_capita = round(valor / populacao, 2)) |>
  select(-c(valor, populacao))


#' `ggplot()`

# ggplot(data = df, mapping = aes(x = ideb, y = desp_per_capita))+
#   geom_point(color = 'blue', size = 4)+
#   theme_bw()



g1 <- ggplot(data = df, mapping = aes(x = ideb, y = desp_per_capita))+
  geom_point(color = 'blue', size = 3)+
  theme_bw()



# Fazer um mapa ----

# install.packages('geobr')

library(geobr)
library(sf)



geom <- read_municipality(year = 2019) |>
  filter(abbrev_state == 'CE') |>
  select(id_municipio = code_muni, nome_mun = name_muni, geom) |>
  mutate(id_municipio = as.character(id_municipio))




df_1 <- left_join(df, geom, by = 'id_municipio') |>
  st_as_sf() # funcao do pacote sf que torna essa base compativel com analise de mapas



g2 <- ggplot(df_1, aes(fill = desp_per_capita))+
  geom_sf(color = NA)+
  scale_fill_viridis_c()+
  theme_minimal()+
  theme(#axis.line = element_blank(),
    axis.text = element_blank())+
  labs(title = 'Mapa do Ceará - Despesa 2019', fill = '')



# Fazer gráfico de linha com a despesa per capita, de 2010 a 2020.


pop <- read_rds('data-raw/pop.rds') |>
  filter(ano %in% c(2013:2020)) |>
  select(-sigla_uf) |>
  arrange(ano)


ff <- read_rds('data-raw/finbra_mun.rds') |>

  filter(ano %in% c(2013:2020), estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor) |>

  left_join(pop, by = c('ano', 'id_municipio')) |>
  group_by(ano) |>
  summarise(valor = mean(valor, na.rm = TRUE),
            pop = sum(populacao, na.rm = TRUE)) |>
  mutate(desp_per_capita = round(valor/pop, 2))


library(scales)


g3 <- ggplot(data = ff, aes(x = ano, y = desp_per_capita)) +
  geom_line(size = 1, color = 'blue')+
  geom_point(color = 'blue', size = 3)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(2013, 2020, 1))+
  scale_y_continuous(
    n.breaks = 8,
    labels = label_comma(decimal.mark = ',', big.mark = '.',
                         prefix = 'R$ '))+

  labs(title = 'Evolução das Despesas com Educ - CE',
       x = NULL, y = 'Despesa com Educação Per Capita')



# salvar os graficos


lista_graf <- list(g1, g2, g3)

file_name <- c('g1.pdf', 'g2.pdf', 'g3.pdf')



# Salvar os gráficos em pdf

walk2(.x = lista_graf, .y = file_name,
      .f = ~ ggsave(filename = .y, plot = .x, path = 'figures',
                    device = cairo_pdf, width = 10, height = 8, scale = 0.7)
)



# Salvar os gráficos em PNG
# ggsave(filename = glue::glue('{.y}.png'), plot = .x, path = 'figures',
#        device = 'png',
#        width = 1280, height = 720, units = 'px',
#        scale = 3)







