# Execute a partir da raiz do repositório.
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
acidentes <- read_csv("dados_agregados/acidentes_top10.csv", show_col_types = FALSE)
obitos <- read_csv("dados_agregados/obitos.csv", show_col_types = FALSE)
dir.create("figuras", showWarnings = FALSE)
rotulo_numero <- label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")
tema <- theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none")
p1 <- ggplot(acidentes, aes(ano, acidentes)) +
  geom_line(color = "#073A70", linewidth = 1.2) +
  geom_point(color = "#073A70", size = 3) +
  geom_text(aes(label = rotulo_numero(acidentes)), vjust = -1, size = 5) +
  scale_x_continuous(breaks = 2021:2025) +
  scale_y_continuous(labels = rotulo_numero,
                     expand = expansion(mult = c(.12, .25))) +
  labs(title = "Acidentes nas dez BRs selecionadas",
       subtitle = "Seleção pelo total acumulado de ocorrências em 2021–2025",
       x = NULL, y = "Acidentes registrados", caption = "Fonte: PRF.") +
  tema
obitos_long <- obitos %>%
  pivot_longer(-ano, names_to = "serie", values_to = "obitos") %>%
  mutate(serie = recode(serie, obitos_covid = "COVID-19 no Brasil",
                       mortos_rodovias = "Rodovias federais"))
p2 <- ggplot(obitos_long, aes(ano, obitos, color = serie)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = rotulo_numero(obitos)), vjust = -.8, size = 5) +
  facet_wrap(~serie, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("COVID-19 no Brasil" = "#52606D",
                                "Rodovias federais" = "#073A70")) +
  scale_x_continuous(breaks = 2021:2024,
                     expand = expansion(mult = c(.18, .18))) +
  scale_y_continuous(limits = c(0, NA), labels = rotulo_numero,
                     expand = expansion(mult = c(0, .18))) +
  labs(title = "Óbitos por COVID-19 e em rodovias federais",
       subtitle = "2021–2024: COVID-19 −98,6% | PRF +14,1%",
       x = NULL, y = "Óbitos registrados",
       caption = "Escalas verticais próprias. Fontes: Ministério da Saúde e PRF.") +
  tema
ggsave("figuras/Figura_1_Acidentes.pdf", p1, width = 12, height = 7)
ggsave("figuras/Figura_2_Obitos_Paineis.pdf", p2, width = 14, height = 7)
print(p1)
print(p2)
