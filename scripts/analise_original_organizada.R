# ARQUIVO HISTÓRICO — preserva as cinco análises originais.
# Para os resultados revisados do pôster, use os scripts 01 e 02.
# Este arquivo mantém 2025 parcial na comparação da COVID e tabelas digitadas.

# Artur Silva Monteiro
# Análise exploratória PRF e COVID-19
# Execute as seções na ordem abaixo.
# Mantidos os métodos, os períodos e as tabelas da análise original.
# ATENÇÃO: COVID-19 em 2025 disponível somente até 05/09.
# A comparação anual original inclui esse período parcial.
# Antes da versão final do pôster, ajustar a comparação para 2021–2024.
# A tabela_resumo_q1 contém valores digitados no trabalho original.

# 1. PACOTES
# Instalação necessária apenas se algum pacote ainda não estiver disponível:
# install.packages(c("readr", "dplyr", "ggplot2", "broom",
#                    "scales", "stringr", "tidyr"))
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(scales)
library(stringr)
library(tidyr)
    
# 2. EXTRAÇÃO DOS ARQUIVOS PRF
# Etapa mantida do código enviado. Os ZIPs devem estar em Downloads.
pasta_downloads <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
anos <- c(2018, 2019, 2021, 2022, 2023, 2024, 2025)

for (ano in anos) {
  unzip(
    zipfile = file.path(pasta_downloads, paste0("datatran", ano, ".zip")),
    exdir = file.path(pasta_downloads, paste0("datatran", ano))
  )
}

# 3. IMPORTAÇÃO E PREPARAÇÃO DOS DADOS PRF
dados2018 <- read_delim("C:/Users/silva/Downloads/datatran2018/datatran2018.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2019 <- read_delim("C:/Users/silva/Downloads/datatran2019/datatran2019.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2021 <- read_delim("C:/Users/silva/Downloads/datatran2021/datatran2021.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2022 <- read_delim("C:/Users/silva/Downloads/datatran2022/datatran2022.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2023 <- read_delim("C:/Users/silva/Downloads/datatran2023/datatran2023.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2024 <- read_delim("C:/Users/silva/Downloads/datatran2024/datatran2024.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2025 <- read_delim("C:/Users/silva/Downloads/datatran2025/datatran2025.csv",
                        delim = ";",
                        locale = locale(encoding = "Latin1"))

dados2018$ano <- 2018
dados2019$ano <- 2019
dados2021$ano <- 2021
dados2022$ano <- 2022
dados2023$ano <- 2023
dados2024$ano <- 2024
dados2025$ano <- 2025
dados2018$latitude <- as.character(dados2018$latitude)
dados2019$latitude <- as.character(dados2019$latitude)
dados2021$latitude <- as.character(dados2021$latitude)
dados2022$latitude <- as.character(dados2022$latitude)
dados2023$latitude <- as.character(dados2023$latitude)
dados2024$latitude <- as.character(dados2024$latitude)
dados2025$latitude <- as.character(dados2025$latitude)
dados <- bind_rows(
  dados2018,
  dados2019,
  dados2021,
  dados2022,
  dados2023,
  dados2024,
  dados2025
)


# 4. IMPORTAÇÃO DOS REGISTROS NACIONAIS DA COVID-19
pasta_covid <- file.path(
  pasta_downloads, "HIST_PAINEL_COVIDBR_05set2025"
)

arquivos_covid <- list.files(
  pasta_covid,
  pattern = "\\.csv$",
  full.names = TRUE
)

covid_brasil <- bind_rows(
  lapply(arquivos_covid, function(arquivo) {
    read_delim(
      arquivo,
      delim = ";",
      col_types = cols_only(
        regiao = col_character(),
        estado = col_character(),
        municipio = col_character(),
        data = col_date(),
        obitosNovos = col_double()
      ),
      show_col_types = FALSE
    ) %>%
      filter(
        regiao == "Brasil",
        is.na(estado),
        is.na(municipio)
      )
  })
)

# 5. QUESTÃO 1 — EVOLUÇÃO DOS ACIDENTES NAS PRINCIPAIS RODOVIAS
# Principais rodovias do Brasil (2021–2025)
principais_brs <- dados %>%
  filter(ano >= 2021) %>%
  filter(!is.na(br)) %>%
  count(br, sort = TRUE)
head(principais_brs, 10)

#Evolução anual das 10 principais BRs
top10_brs <- principais_brs %>%
  slice(1:10) %>%
  pull(br)
evolucao_brs <- dados %>%
  filter(
    ano >= 2021,
    br %in% top10_brs
  ) %>%
  count(br, ano)
evolucao_brs

# Total anual de acidentes nas 10 principais BRs
total_anual_top10 <- evolucao_brs %>%
  group_by(ano) %>%
  summarise(
    acidentes = sum(n),
    .groups = "drop"
  )
total_anual_top10

# Teste de tendência por regressão linear
modelo_tendencia <- lm(
  acidentes ~ ano,
  data = total_anual_top10
)
summary(modelo_tendencia)
# Tendência em cada uma das 10 principais BRs
resultado_brs <- evolucao_brs %>%
  group_by(br) %>%
  do(
    tidy(
      lm(n ~ ano, data = .)
    )
  ) %>%
  filter(term == "ano") %>%
  transmute(
    BR = br,
    coeficiente = estimate,
    p_valor = p.value,
    conclusao = case_when(
      p.value < 0.05 & estimate > 0 ~ "Aumento significativo",
      p.value < 0.05 & estimate < 0 ~ "Redução significativa",
      TRUE ~ "Sem evidência significativa"
    )
  )
resultado_brs
# Crescimento percentual (2021 -> 2025)
crescimento <- total_anual_top10 %>%
  summarise(
    acidentes_2021 = acidentes[ano == 2021],
    acidentes_2025 = acidentes[ano == 2025]
  ) %>%
  mutate(
    aumento_absoluto = acidentes_2025 - acidentes_2021,
    aumento_percentual =
      (aumento_absoluto / acidentes_2021) * 100
  )
crescimento
by(evolucao_brs$n, evolucao_brs$ano, shapiro.test)
# Teste de Kruskal Wallis
kruskal.test(
  n ~ factor(ano),
  data = evolucao_brs
)
# Tabela 1 - Dez principais BRs (2021–2025)
tabela_top10_brs <- principais_brs %>%
  slice_head(n = 10) %>%
  transmute(
    BR = paste0("BR-", br),
    `Total de acidentes` = n
  )
tabela_top10_brs
# Tabela 2 - Resumo dos resultados
tabela_resumo_q1 <- tibble(
  Indicador = c(
    "Acidentes em 2021",
    "Acidentes em 2025",
    "Aumento absoluto",
    "Aumento percentual",
    "Tendência anual estimada",
    "Coeficiente de determinação",
    "p-valor da tendência",
    "BRs com aumento significativo"
  ),
  Resultado = c(
    "39.016",
    "44.293",
    "5.277",
    "13,5%",
    "1.618,5 acidentes/ano",
    "R² = 0,876",
    "p = 0,0192",
    "6 de 10"
  )
)
tabela_resumo_q1
grafico_q1 <-
  ggplot(
    total_anual_top10,
    aes(
      x = factor(ano),
      y = acidentes
    )
  )+
  
  geom_col(
    fill="#2C7FB8",
    width=.65
  )+
  
  geom_text(
    aes(
      label = comma(
        acidentes,
        big.mark=".",
        decimal.mark=","
      )
    ),
    vjust=-0.4,
    fontface="bold",
    size=4
  )+
  
  labs(
    title="Acidentes nas 10 principais rodovias federais",
    subtitle="Brasil (2021–2025)",
    x="Ano",
    y="Número de acidentes"
  )+
  
  scale_y_continuous(
    labels=comma_format(
      big.mark=".",
      decimal.mark=","
    ),
    expand=expansion(mult=c(0,.10))
  )+
  
  theme_minimal(base_size=13)+
  
  theme(
    plot.title=element_text(
      face="bold",
      hjust=.5
    ),
    plot.subtitle=element_text(
      hjust=.5
    ),
    panel.grid.minor=element_blank()
  )
grafico_q1

# 6. QUESTÃO 2 — DISTRIBUIÇÃO DAS CAUSAS
# Períodos conforme a análise original
dados_periodo <- dados %>%
  filter(ano != 2020) %>%
  mutate(
    periodo = if_else(
      ano %in% c(2018, 2019),
      "Pré-pandemia",
      "Pós-pandemia"
    )
  )
# Frequência das causas
causas_periodo <- dados_periodo %>%
  filter(!is.na(causa_acidente)) %>%
  count(periodo, causa_acidente)
causas_periodo
# Percentual por período
causas_percentual <- causas_periodo %>%
  group_by(periodo) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup()
causas_percentual
# Agrupamento das causas em categorias comparáveis
dados_periodo <- dados_periodo %>%
  mutate(
    grupo_causa = case_when(
      str_detect(
        causa_acidente,
        regex(
          "Falta de Atenção|Ausência de reação|Reação tardia|celular",
          ignore_case = TRUE
        )
      ) ~ "Desatenção e reação",
      str_detect(
        causa_acidente,
        regex("Velocidade|racha", ignore_case = TRUE)
      ) ~ "Velocidade",
      str_detect(
        causa_acidente,
        regex(
          "Álcool|alcool|Substâncias Psicoativas|substâncias psicoativas",
          ignore_case = TRUE
        )
      ) ~ "Álcool e drogas",
      str_detect(
        causa_acidente,
        regex("Dormindo|Mal Súbito|Mal súbito", ignore_case = TRUE)
      ) ~ "Sono e mal súbito",
      str_detect(
        causa_acidente,
        regex(
          "Defeito Mecânico|falhas mecânicas|falhas elétricas|pneu|freio|suspensão|Faróis desregulados|Iluminação/Sinalização do Veículo|farol da motocicleta|Carga excessiva|mal acondicionada|Modificação proibida",
          ignore_case = TRUE
        )
      ) ~ "Falhas do veículo e carga",
      str_detect(
        causa_acidente,
        regex(
          "Ultrapassagem|contramão|distância|mudança de faixa|Conversão proibida|Retorno proibido|Estacionar ou parar|Transitar no acostamento|Transitar no Acostamento|Transitar na calçada|Trafegar com motocicleta|Desrespeitar a preferência|semáforo|Desobediência às normas de trânsito pelo condutor|Acessar a via sem observar|Frear bruscamente",
          ignore_case = TRUE
        )
      ) ~ "Manobras e infrações do condutor",
      str_detect(
        causa_acidente,
        regex(
          "Pedestre|travessia de pedestres|Entrada inopinada",
          ignore_case = TRUE
        )
      ) ~ "Fatores relacionados ao pedestre",
      str_detect(
        causa_acidente,
        regex(
          "Animais na Pista|Chuva|Neblina|Fumaça|Fenômenos da Natureza|Demais Fenômenos|Restrição de Visibilidade",
          ignore_case = TRUE
        )
      ) ~ "Clima, natureza e visibilidade",
      str_detect(
        causa_acidente,
        regex(
          "Defeito na Via|falhas na via|Acostamento|pavimento|Pista em desnível|Pista esburacada|Pista Escorregadia|sinalização|Iluminação deficiente|Obras na pista|Obstrução na via|Desvio temporário|Faixas de trânsito|Falta de acostamento|contenção|Redutor de velocidade|drenagem|Curva acentuada|Declive acentuado|Acesso irregular|Objeto estático",
          ignore_case = TRUE
        )
      ) ~ "Infraestrutura e condições da via",
      str_detect(
        causa_acidente,
        regex(
          "Agressão Externa|tentativa Assalto|Suicídio|Transtornos Mentais",
          ignore_case = TRUE
        )
      ) ~ "Eventos externos e pessoais",
      TRUE ~ "Outras causas"
    )
  )
dados_periodo %>%
  count(grupo_causa, sort = TRUE)
# Frequência por período
comparacao_causas <- dados_periodo %>%
  count(periodo, grupo_causa) %>%
  group_by(periodo) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup()
comparacao_causas
# Tabela cruzada
tabela_causas <- table(
  dados_periodo$periodo,
  dados_periodo$grupo_causa
)
tabela_causas
# Teste do Qui-quadrado
teste_q2 <- chisq.test(tabela_causas)
teste_q2
# Resíduos padronizados
round(teste_q2$stdres, 2)
comparacao_final <- comparacao_causas %>%
  select(periodo, grupo_causa, percentual) %>%
  tidyr::pivot_wider(
    names_from = periodo,
    values_from = percentual
  ) %>%
  mutate(
    diferenca = `Pós-pandemia` - `Pré-pandemia`
  ) %>%
  arrange(desc(abs(diferenca)))
comparacao_final
# Gráfico - Comparação das causas pré e pós-pandemia
ggplot(
  comparacao_causas,
  aes(
    x = reorder(grupo_causa, percentual),
    y = percentual,
    fill = periodo
  )
) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "Causas dos acidentes",
    subtitle = "Comparação entre os períodos pré e pós-pandemia",
    x = "",
    y = "Percentual (%)",
    fill = "Período"
  ) +
  scale_fill_manual(
    values = c(
      "Pré-pandemia" = "grey70",
      "Pós-pandemia" = "#2C7FB8"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
# 7. QUESTÃO 3 — PESSOAS ENVOLVIDAS POR ANO
cronologia_pessoas <- dados %>%
  group_by(ano) %>%
  summarise(
    acidentes = n(),
    total_pessoas = sum(pessoas, na.rm = TRUE),
    media_pessoas = mean(pessoas, na.rm = TRUE),
    total_mortos = sum(mortos, na.rm = TRUE),
    total_feridos = sum(feridos, na.rm = TRUE),
    total_ilesos = sum(ilesos, na.rm = TRUE),
    .groups = "drop"
  )
cronologia_pessoas
# Regressões lineares - Questão 3
###############################################################
modelo_pessoas <- lm(total_pessoas ~ ano, data = cronologia_pessoas)
modelo_media <- lm(media_pessoas ~ ano, data = cronologia_pessoas)
modelo_mortos <- lm(total_mortos ~ ano, data = cronologia_pessoas)
modelo_feridos <- lm(total_feridos ~ ano, data = cronologia_pessoas)

summary(modelo_pessoas)
summary(modelo_media)
summary(modelo_mortos)
summary(modelo_feridos)
# Questão 3 Cronologia das pessoas envolvidas em acidentes
grafico_q3 <-
  ggplot(
    cronologia_pessoas,
    aes(
      x = ano,
      y = total_pessoas
    )
  ) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1.3
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "grey55",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text(
    aes(
      label = comma(
        total_pessoas,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    vjust = -1.2,
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(
    breaks = cronologia_pessoas$ano
  ) +
  scale_y_continuous(
    labels = comma_format(
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  labs(
    title = "Pessoas envolvidas em acidentes",
    subtitle = "Rodovias federais brasileiras (2018–2025)",
    x = "Ano",
    y = "Número de pessoas"
  ) +
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 10,
      l = 10
    )
  )
grafico_q3

# 8. QUESTÃO 4 — COMPARAÇÃO DOS ÓBITOS
# Óbitos anuais por COVID-19 no Brasil
covid_anual <- covid_brasil %>%
  mutate(
    ano = as.integer(format(data, "%Y"))
  ) %>%
  group_by(ano) %>%
  summarise(
    obitos_covid = sum(obitosNovos, na.rm = TRUE),
    .groups = "drop"
  )
covid_anual
# Junção PRF x COVID
comparacao <- cronologia_pessoas %>%
  select(
    ano,
    mortos_rodovias = total_mortos
  ) %>%
  inner_join(
    covid_anual,
    by = "ano"
  )
comparacao
cor.test(
  comparacao$mortos_rodovias,
  comparacao$obitos_covid
)
comparacao_indice <- comparacao %>%
  mutate(
    covid_indice = obitos_covid / first(obitos_covid) * 100,
    rodovia_indice = mortos_rodovias / first(mortos_rodovias) * 100
  )
comparacao_long <- comparacao_indice %>%
  pivot_longer(
    cols = c(
      covid_indice,
      rodovia_indice
    ),
    names_to = "Origem",
    values_to = "Indice"
  )
ggplot(
  comparacao_long,
  aes(
    ano,
    Indice,
    color = Origem
  )
)+
  geom_line(
    linewidth=1.3
  )+
  geom_point(
    size=3
  )+
  scale_color_manual(
    values=c(
      "covid_indice"="#C0504D",
      "rodovia_indice"="#2C7FB8"
    ),
    labels=c(
      "COVID-19",
      "Rodovias"
    )
  )+
  labs(
    title="Comparação da dinâmica dos óbitos",
    subtitle="Índice com base em 2021 (=100)",
    x="Ano",
    y="Índice"
  )+
  theme_minimal(base_size=13)
# 9. QUESTÃO 5 — RANKING DAS RODOVIAS
# Top 10 de rodovias por ano
top10_ano <- dados %>%
  filter(ano >= 2021) %>%
  count(ano, br, name = "acidentes") %>%
  group_by(ano) %>%
  arrange(desc(acidentes), .by_group = TRUE) %>%
  mutate(posicao = row_number()) %>%
  filter(posicao <= 10) %>%
  ungroup()
top10_ano
print(top10_ano, n = 50)
ranking_final <- top10_ano %>%
  mutate(
    BR = paste0("BR-", br)
  ) %>%
  select(
    ano,
    BR,
    posicao
  ) %>%
  pivot_wider(
    names_from = ano,
    values_from = posicao
  )
ranking_final
# Amplitude das posições no ranking
variacao_ranking <- ranking_final %>%
  rowwise() %>%
  mutate(
    variacao = max(c_across(`2021`:`2025`)) -
      min(c_across(`2021`:`2025`))
  ) %>%
  ungroup()
variacao_ranking
