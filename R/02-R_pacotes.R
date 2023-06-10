


# INTRODUCAO A CIENCIA DE DADOS (tidyverse) ------------------------------------

# installed.packages("dplyr")

# library(dplyr)

library(tidyverse)


# PIPE ----


num <- 16

num |> sqrt() |> sum(10)


# TIBBLE ----

df <- data.frame(v1 = c(100000,2,3), v2 = c('a', 'b', NA)) |>
  as_tibble()

tb <- tibble(v1 = c(10000000,2,3), v2 = c('a', 'b', NA))





# IMPORTACAO / EXPORTACAO ---------------
# readr, readxl, data.table, arrow

library(data.table)

library(arrow)


df <- read_csv(file = 'data-raw/municipio.csv')

df <- fread(input = 'data-raw/municipio.csv', sep = ',')


df <- read_delim(file = 'data-raw/municipio.csv', delim = ',')


write_rds(x = df, file = 'data/dff.rds')

write_parquet(x = df, 'data/dd.parquet')

df <- read_parquet(file = 'data/dd.parquet')




# ARRUMACAO / MANIPULACAO (wrangling) -----------------
# dplyr, tidyr

# filter

d1 <- df |> filter(sigla_uf == 'CE')


d1 <- df |> filter(sigla_uf == 'CE' & nome == 'Coreaú') |>
  as_tibble()


d2 <- df |> filter(sigla_uf == 'CE' | sigla_uf == 'PB')

d3 <- df |> filter(sigla_uf %in% c('CE', 'PB'))



# Select e rename


d4 <- df |> select(id_municipio, id_mesorregiao)


d5 <- df |> select(starts_with('id_'))



d6 <- df |>  select(!starts_with('id_'))


d7 <-  df |> select(id_mun_6 = id_municipio_6, id_mun = id_municipio)

d8 <- df |> rename(id_mun_6 = id_municipio_6, id_mun = id_municipio)


# mutate

d9 <- df |> filter(sigla_uf == 'CE') |>
  mutate(dd_interior = ifelse(ddd == 88, 1, 0)) |>
  select(nome, dd_interior) |>
  summarise(prop = mean(dd_interior))


d10 <- df |> filter(sigla_uf == 'CE' & nome == 'Cruz')



# Base finbra ----

df <- read_rds(file = 'data-raw/finbra_uf.rds')


d1 <- filter(df, ano == 2021) |>
  select(sigla_uf, valor) |>
  group_by(sigla_uf) |>
  summarise(med = sum(valor))


d2 <- df |> select(ano, sigla_uf, valor) |>
  group_by(sigla_uf, ano) |>
  summarise(med = sum(valor), .groups = 'drop')














