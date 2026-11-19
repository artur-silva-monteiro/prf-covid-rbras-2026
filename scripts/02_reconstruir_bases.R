# Execute na raiz do repositório.
# Esperado em Downloads: datatran2021/datatran2021.csv até 2025.
# COVID: HIST_PAINEL_COVIDBR_05set2025/*.csv.
# Edite pasta_dados se os arquivos estiverem em outro local.
library(readr)
library(dplyr)
pasta_dados <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
anos <- 2021:2025
prf <- bind_rows(lapply(anos, function(ano) {
  caminho <- file.path(pasta_dados, paste0("datatran", ano),
                       paste0("datatran", ano, ".csv"))
  read_delim(
    caminho, delim = ";", locale = locale(encoding = "Latin1"),
    col_types = cols_only(id = col_double(), br = col_double(),
                          data_inversa = col_date(), mortos = col_double()),
    show_col_types = FALSE
  ) %>% mutate(ano = ano)
}))
duplicados <- prf %>% count(ano, id) %>% filter(!is.na(id), n > 1)
if (nrow(duplicados) > 0) stop("Há IDs repetidos no mesmo ano. Revise a base.")
top10 <- prf %>%
  filter(!is.na(br)) %>% count(br, sort = TRUE) %>%
  slice_head(n = 10) %>% pull(br)
acidentes <- prf %>%
  filter(br %in% top10) %>% count(ano, name = "acidentes") %>% arrange(ano)
arquivos <- list.files(
  file.path(pasta_dados, "HIST_PAINEL_COVIDBR_05set2025"),
  pattern = "\\.csv$", full.names = TRUE
)
if (length(arquivos) == 0) stop("Arquivos da COVID não encontrados.")
covid <- bind_rows(lapply(arquivos, function(arquivo) {
  read_delim(
    arquivo, delim = ";",
    col_types = cols_only(regiao = col_character(), estado = col_character(),
                          municipio = col_character(), data = col_date(),
                          obitosNovos = col_double()),
    show_col_types = FALSE
  ) %>% filter(regiao == "Brasil", is.na(estado), is.na(municipio))
}))
if (anyDuplicated(covid$data)) stop("Há datas nacionais repetidas na base da COVID.")
covid_anual <- covid %>%
  mutate(ano = as.integer(format(data, "%Y"))) %>%
  filter(ano %in% 2021:2024) %>%
  group_by(ano) %>%
  summarise(obitos_covid = sum(obitosNovos, na.rm = TRUE), .groups = "drop")
obitos <- prf %>%
  filter(ano %in% 2021:2024) %>%
  group_by(ano) %>%
  summarise(mortos_rodovias = sum(mortos, na.rm = TRUE), .groups = "drop") %>%
  inner_join(covid_anual, by = "ano") %>% arrange(ano)
print(acidentes)
print(obitos)
# Confira com os CSVs agregados antes de substituir.
# write_csv(acidentes, "dados_agregados/acidentes_top10.csv")
# write_csv(obitos, "dados_agregados/obitos.csv")
