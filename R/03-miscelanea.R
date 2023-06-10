# Miscelânia ------------------------------------------------------------------

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

library(geobr)

geom <- read_municipality(year = 2019) |>
  filter(abbrev_state == 'CE') |>
  select(id_municipio = code_muni, nome_mun = name_muni, geom) |>
  mutate(id_municipio = as.character(id_municipio))


df_1 <- left_join(df, geom, by = 'id_municipio') |>
  sf::st_as_sf()


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
  filter(ano %in% c(2010:2020), estagio == 'Despesas Empenhadas',
         str_detect(string = id_conta_bd, pattern = "3.12.000")
  ) |>
  select(ano, id_municipio, valor) |>
  left_join(pop, by = c('ano', 'id_municipio')) |>

  group_by(ano) |>
  summarise(
    across(c(valor, populacao), ~ mean(.x, na.rm = TRUE))
  ) |>

  mutate(desp_per_capita = round(valor/populacao, 2))


g3 <- ggplot(ff, aes(x = ano, y = desp_per_capita))+
  geom_line(size = 1, color = 'blue')+
  geom_point(size = 3, color = 'red')+
  scale_x_continuous(breaks = seq(2013, 2020, 1))+
  scale_y_continuous(n.breaks = 10)


# salvar os graficos


lista_graf <- list(g1, g2, g3)

file_name <- c('g1', 'g2', 'g3')



walk2(
  lista_graf,  file_name,
  ~ ggsave(filename = glue::glue('{.y}.pdf'), plot = .x, path = 'figures',
           device = cairo_pdf, width = 10, height = 7, scale = 2))


# PNG
# ggsave(filename = glue::glue('{.y}.png'), plot = .x, path = 'figures',
#        device = 'png',
#        width = 1280, height = 720, units = 'px',
#        scale = 3)

