# Acidentes em rodovias federais brasileiras no contexto da pandemia de COVID-19
## Uma análise exploratória dos dados da PRF

Material complementar do pôster aceito na **70ª RBras**, Campinas, 23–25 de novembro de 2026. Trabalho 153.

**Autores** — Artur Silva Monteiro, Maurício Alexandre de Lacerda Ribeiro e Gabriel Naves Queiroz Buffon.  
**Instituição** — Universidade Federal do Espírito Santo (UFES).  
**Apresentador e contato** — Artur Silva Monteiro · silvaartur2001@gmail.com

## Acesse o trabalho

- [Relatório técnico deste recorte](Relatorio_PRF_COVID.pdf)
- [Pôster A0 em PDF](Poster_PRF_COVID_A0.pdf)
- [Código R das figuras](scripts/01_figuras.R)
- [Reconstrução das tabelas a partir dos CSVs originais](scripts/02_reconstruir_bases.R)
- [Código histórico das cinco análises da disciplina](scripts/analise_original_organizada.R)
- [Dados agregados das figuras](dados_agregados)

## Resultados

Nas dez BRs selecionadas pelo total acumulado de ocorrências em 2021–2025, os acidentes passaram de **39.016 para 44.293**, aumento de **13,5%**.

Na comparação de anos completos, 2021–2024, os óbitos por COVID-19 passaram de **424.107 para 5.959** (−98,6%). Os óbitos registrados pela PRF passaram de **5.397 para 6.160** (+14,1%). A comparação é descritiva, com coberturas distintas e escalas próprias nos painéis.

## Como reproduzir

Abra um projeto R na raiz deste repositório. Instale os pacotes readr, dplyr, tidyr, ggplot2 e scales. Execute:

~~~r
source("scripts/01_figuras.R", echo = TRUE)
~~~

Esse script lê as duas tabelas agregadas e grava os gráficos na pasta figuras. Os totais foram conferidos com a execução local do código pelo apresentador em 19/11/2026.

Para recalcular a partir dos arquivos de origem, consulte as instruções no início de scripts/02_reconstruir_bases.R. Os dados brutos devem ser obtidos das fontes oficiais ou recuperados dos arquivos locais usados no trabalho.

## Fontes e cobertura

- [PRF — Dados Abertos](https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-da-prf), arquivos agrupados por ocorrência, 2021–2025.
- [Ministério da Saúde — Painel Coronavírus Brasil](https://covid.saude.gov.br/), versão HIST_PAINEL_COVIDBR_05set2025, com registros até 05/09/2025.
- O pôster utiliza acidentes de **2021–2025** e compara os óbitos apenas em **2021–2024**, com anos completos nas duas fontes.
- As dez BRs são selecionadas por contagens acumuladas. O número de acidentes também depende do volume de tráfego e da extensão das rodovias.
- A análise de óbitos da PRF abrange todas as ocorrências da base, enquanto a Figura 1 se limita às dez BRs selecionadas.

## Sobre o material histórico

O arquivo histórico preserva as cinco análises originais, incluindo testes exploratórios e uma comparação antiga com 2025 parcial. Ele está identificado como histórico. Os resultados do pôster e do relatório atual são os descritos nos dois CSVs agregados e nos scripts numerados.

Este PDF é um relatório técnico disponibilizado pelos autores. O aceite refere-se à apresentação do pôster na RBras. O relatório não passou por avaliação editorial de periódico.

## Agradecimentos

Ao professor Fábio pelas orientações na disciplina Métodos Computacionais para Ciência de Dados I, na qual foi desenvolvida a análise que deu origem ao pôster.

## English summary

Exploratory analysis of Brazilian Federal Highway Police crash records and Ministry of Health COVID-19 deaths. Crashes on the ten selected federal highways increased by 13.5% from 2021 to 2025. Between 2021 and 2024, recorded COVID-19 deaths decreased by 98.6%, while deaths recorded by the Federal Highway Police increased by 14.1%. These are descriptive comparisons across different reporting systems.
