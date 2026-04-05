## Instalação das bibliotecas
install.packages(c(
  "tidyverse",
  "sidrar",
  "janitor",
  "knitr",
  "kableExtra"
))

## Ativação das bibliotecas
library(tidyverse)
library(sidrar)
library(janitor)
library(knitr)
library(kableExtra)


## Leitura das familias do Cadunico 2025 baixada em formato .csv, usando read_delim
familias_cadunico <- read_delim(
  "familias_cadunico_2025.csv",
  delim = ",",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  clean_names()

## correção do nome do município
familias_cadunico <- familias_cadunico %>%
  mutate(
    unidade_territorial = if_else(
      unidade_territorial == "AMPARO DE SÃO FRANCISCO",
      "AMPARO DO SÃO FRANCISCO",
      unidade_territorial
    )
  )

## Renomeando as colunas da tabela
familias_cadunico <- familias_cadunico %>%
  rename(
    nome_mun = unidade_territorial,
    qtd_fam = quantidade_total_de_familias_inscritas_no_cadastro_unico
  )

## renomear as colunas
names(familias_cadunico) <- c(
  "cod_mun",
  "nome_mun",
  "uf",
  "mes_referencia",
  "qtd_fam"
)

## Calculando a quantidade média de familias por municipio por ano
familias_cadunico <- familias_cadunico %>%
  group_by(cod_mun, nome_mun) %>%
  summarise(
    qte_fam_medio = mean(qtd_fam, na.rm = TRUE)
  ) %>%
  ungroup()

## Arredondando a coluna qte_fam_medio
familias_cadunico <- familias_cadunico %>%
  mutate(qte_fam_benef = round(qte_fam_medio, 1))




## Leitura de familias por condição com read_csv
familias_cond_pbf <- read_csv(
  "familias_por_condicao_pbf_2025.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  clean_names()


## correção do nome do município
familias_cond_pbf <- familias_cond_pbf %>%
  mutate(
    unidade_territorial = if_else(
      unidade_territorial == "AMPARO DE SÃO FRANCISCO",
      "AMPARO DO SÃO FRANCISCO",
      unidade_territorial
    )
  )

## renomear as colunas
names(familias_cond_pbf) <- c(
  "cod_mun",
  "nome_mun",
  "uf",
  "mes_referencia",
  "num_familias",
  "ate_meio_salario",
  "acima_meio_salario"
)

glimpse(familias_cond_pbf)

## Calculando a média mensal para num_familias, ate_meio_salario e acime_meio_salario
familias_cond_pbf <- familias_cond_pbf %>%
  group_by(cod_mun, nome_mun) %>%
  summarise(
    num_familias = round(mean(num_familias, na.rm = TRUE), 1),
    ate_meio_salario = round(mean(ate_meio_salario, na.rm = TRUE), 1),
    acima_meio_salario = round(mean(acima_meio_salario, na.rm = TRUE), 1)
  ) %>%
  ungroup()






## Leitura de pessoas beneficiadas por bolsa familia com read_csv
pessoas_benef_pbf_2025 <- read_csv(
  "pessoas_benef_pbf_2025.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  clean_names()

## correção do nome do município
pessoas_benef_pbf_2025 <- pessoas_benef_pbf_2025 %>%
  mutate(
    unidade_territorial = if_else(
      unidade_territorial == "AMPARO DE SÃO FRANCISCO",
      "AMPARO DO SÃO FRANCISCO",
      unidade_territorial
    )
  )

## renomear as colunas
names(pessoas_benef_pbf_2025) <- c(
  "cod_mun",
  "nome_mun",
  "uf",
  "mes_referencia",
  "ate_out_21",
  "depois_mar_23"
)

## Verificaçao dos tipos de dados do dataframe
glimpse(pessoas_benef_pbf_2025)

## Trocando a coluna ate_out_21 que está com tipo lógico por tipo inteiro com valor -1
## Talvez isto não seja necessário caso não seja usado porteriormente
pessoas_benef_pbf_2025 <- pessoas_benef_pbf_2025 %>%
  mutate(ate_out_21 = replace_na(as.integer(ate_out_21), -1L))


## Calculando a média mensal para ate_out_21 e depois_mar_23
pessoas_benef_pbf_2025 <- pessoas_benef_pbf_2025 %>%
  group_by(cod_mun, nome_mun) %>%
  summarise(
    ate_out_21 = round(mean(ate_out_21, na.rm = TRUE), 1),
    depois_mar_23 = round(mean(depois_mar_23, na.rm = TRUE), 1)
  ) %>%
  ungroup()






## Leitura de pessoas no cadastro único por sexo com read_csv
pessoas_cadun_sexo <- read_csv(
  "pessoas_cadun_sexo_2025.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  clean_names()


## correção do nome do município
pessoas_cadun_sexo <- pessoas_cadun_sexo %>%
  mutate(
    unidade_territorial = if_else(
      unidade_territorial == "AMPARO DE SÃO FRANCISCO",
      "AMPARO DO SÃO FRANCISCO",
      unidade_territorial
    )
  )

## renomear as colunas
names(pessoas_cadun_sexo) <- c(
  "cod_mun",
  "nome_mun",
  "uf",
  "mes_referencia",
  "qtd_homens",
  "qtd_mulheres"
)

## Calculando a média mensal para qtd_homens e qtd_mulheres
pessoas_cadun_sexo <- pessoas_cadun_sexo %>%
  group_by(cod_mun, nome_mun) %>%
  summarise(
    qtd_homens = round(mean(qtd_homens, na.rm = TRUE), 1),
    qtd_mulheres = round(mean(qtd_mulheres, na.rm = TRUE), 1)
  ) %>%
  ungroup()






## Leitura dda tabela de valores com read_csv
valores_pbf <- read_csv(
  "valores_pbf_2025.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
) %>%
  clean_names()

valores_pbf <- read_csv(
  "valores_pbf_2025.csv",
  locale = locale(
    encoding = "Latin1",
    decimal_mark = ",",
    grouping_mark = "."
  ),
  show_col_types = FALSE
) %>%
  clean_names()


## Verificaçao dos tipos de dados do dataframe
glimpse(valores_pbf)


## correção do nome do município
valores_pbf <- valores_pbf %>%
  mutate(
    unidade_territorial = if_else(
      unidade_territorial == "AMPARO DE SÃO FRANCISCO",
      "AMPARO DO SÃO FRANCISCO",
      unidade_territorial
    )
  )

## renomear as colunas
names(valores_pbf) <- c(
  "cod_mun",
  "nome_mun",
  "uf",
  "mes_referencia",
  "qtd_ate_out_21",
  "qtd_depois_mar_23",
  "valor_ate_out_21",
  "valor_depois_mar_23",
  "medio_ate_out_21",
  "medio_depois_mar_23"
)

## Calculando a média mensal para qtd_depois_mar_23, valor_depois_mar_23 e medio_depois_mar_23
## aproveitei e mudei o nome das colunas para facilitar a leitura
valores_pbf <- valores_pbf %>%
  group_by(cod_mun, nome_mun) %>%
  summarise(
    qte_fam_benef = round(mean(qtd_depois_mar_23, na.rm = TRUE), 1),
    valor_total_benef = round(mean(valor_depois_mar_23, na.rm = TRUE), 1),
    valor_medio_benef = round(mean(medio_depois_mar_23, na.rm = TRUE), 1)
  ) %>%
  ungroup()






## Leitura da taxa de alfabetização do município com Sidra
tx_alfab_mun <- get_sidra(
  api = "/t/9543/n3/28/n6/all/v/all/p/all/c2/6794/c86/95251/c287/100362/d/v2513%202"
) %>%
  clean_names()

## Selecionando as colunas desejadas
tx_alfab_mun <- tx_alfab_mun %>%
  select(5, 6, 7)

names(tx_alfab_mun) <- c(
  "taxa",
  "cod_mun",
  "nome_mun"
)

tx_alfab_mun <- tx_alfab_mun %>%
  select(2, 3, 1)


## Filtrar os municipios
tx_alfab_sergipe <- tx_alfab_mun %>%
  filter(cod_mun >= 2800000, cod_mun < 2900000)

glimpse(tx_alfab_sergipe)

## Remover o digito final do código do municipio para seguir o padrão dos outros dataframes
tx_alfab_sergipe <- tx_alfab_sergipe %>%
  mutate(cod_mun = str_sub(cod_mun, 1, -2))

## Formatar o nome do municipio para seguir o padrão dos outros dataframes
tx_alfab_sergipe <- tx_alfab_sergipe %>%
  mutate(nome_mun = toupper(substr(nome_mun, 1, nchar(nome_mun) - 5)))






## Leitura dos dados da população por município usando Sidra
pop_mun <- get_sidra(
  api = "/t/9514/n6/all/v/allxp/p/all/c2/allxt/c287/100362/c286/113635"
) %>%
  clean_names()


## Selecionando as colunas desejadas
pop_mun <- pop_mun %>%
  select(6, 7, 5, 13)

## Definindo o nome das colunas
names(pop_mun) <- c(
  "cod_mun",
  "nome_mun",
  "valor",
  "sexo"
)

## Filtrar os municipios
pop_mun_sergipe <- pop_mun %>%
  filter(cod_mun >= 2800000, cod_mun < 2900000)

glimpse(pop_mun_sergipe)

## Remover o digito final do código do municipio para seguir o padrão dos outros dataframes
pop_mun_sergipe <- pop_mun_sergipe %>%
  mutate(cod_mun = str_sub(cod_mun, 1, -2))

## Formatar o nome do municipio para seguir o padrão dos outros dataframes
pop_mun_sergipe <- pop_mun_sergipe %>%
  mutate(nome_mun = toupper(substr(nome_mun, 1, nchar(nome_mun) - 5)))

## Fazendo o pivot do campo sexo para ficar apenas uma linha por municipio
pop_mun_sergipe <- pop_mun_sergipe %>%
  pivot_wider(
    names_from = sexo,
    values_from = valor
  ) %>%
  rename(
    pop_masc = Homens,
    pop_fem = Mulheres
  )



## Contrução do dataframe geral base_lab

base_lab <- familias_cadunico %>%
  transmute(
    cod_mun = cod_mun,
    nome_mun = nome_mun,
    qte_fam_cadun = qte_fam_benef
  )

base_lab <- base_lab %>%
  left_join(
    familias_cond_pbf %>%
      transmute(
        cod_mun = cod_mun,
        qte_fam_pobre_renda_pbf = ate_meio_salario
      ),
    by = "cod_mun"
  )

base_lab <- base_lab %>%
  left_join(
    pessoas_benef_pbf_2025 %>%
      transmute(
        cod_mun = cod_mun,
        qte_pess_benef_pbf = depois_mar_23
      ),
    by = "cod_mun"
  )

base_lab <- base_lab %>%
  left_join(
    pessoas_cadun_sexo %>%
      transmute(
        cod_mun = cod_mun,
        qte_homens_cadun = qtd_homens,
        qte_mulheres_cadun = qtd_mulheres
      ),
    by = "cod_mun"
  )

base_lab <- base_lab %>%
  left_join(
    valores_pbf %>%
      transmute(
        cod_mun = cod_mun,
        qte_fam_benef = qte_fam_benef, ## correcao do nome da coluna
        valor_medio_benef = valor_medio_benef
      ),
    by = "cod_mun"
  )

glimpse(base_lab)


base_lab <- base_lab %>%
  mutate(cod_mun = as.character(cod_mun))


base_lab <- base_lab %>%
  left_join(
    pop_mun_sergipe %>%
      transmute(
        cod_mun = cod_mun,
        qte_pop_masc = pop_masc,
        qte_pop_fem = pop_fem
      ),
    by = "cod_mun"
  )


base_lab <- base_lab %>%
  left_join(
    tx_alfab_sergipe %>%
      transmute(
        cod_mun = cod_mun,
        tx_alfab = taxa
      ),
    by = "cod_mun"
  )

glimpse(base_lab)


##########################################################

# Questão 3.1


base_lab <- base_lab %>%
  mutate(
    classe_alfab = ntile(tx_alfab, 4),
    classe_alfab = recode(
      classe_alfab,
      `1` = "Baixa alfabetização",
      `2` = "Média baixa",
      `3` = "Média alta",
      `4` = "Alta"
    )
  )

library(dplyr)
library(knitr)
library(kableExtra)

tabela_alfab <- base_lab %>%
  mutate(
    classe_alfab = ntile(tx_alfab, 4),
    classe_alfab = recode(
      classe_alfab,
      `1` = "Baixa alfabetização",
      `2` = "Média baixa",
      `3` = "Média alta",
      `4` = "Alta"
    )
  ) %>%
  arrange(desc(tx_alfab)) %>%
  mutate(posicao = row_number()) %>%
  transmute(
    Posição = posicao,
    Município = nome_mun,
    `Taxa de alfabetização` = format(
      round(tx_alfab, 2),
      nsmall = 2,
      decimal.mark = ",",
      big.mark = "."
    ),
    Classificação = classe_alfab
  )

tab_alfab_latex <- knitr::kable(
  tabela_alfab,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = TRUE,
  col.names = c(
    "Posição",
    "Município",
    "Taxa de alfabetização",
    "Classificação"
  ),
  align = c("c", "l", "c", "l"),
  caption = "Municípios sergipanos segundo a taxa de alfabetização e a classificação em quartos.",
  label = "alfab_sergipe"
) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "hold_position"),
    font_size = 9
  )

cat(tab_alfab_latex, file = "../lab-latex/tabela_alfabetizacao.tex")

library(dplyr)
library(ggplot2)
library(sf)
library(geobr)

## Baixar o mapa com os municipios
mapa_se <- read_municipality(
  code_muni = "SE",
  year = 2010,
  simplified = TRUE
) %>%
  mutate(
    cod_mun = substr(as.character(code_muni), 1, 6)
  )

## Join com o mapa
mapa_alfab <- mapa_se %>%
  left_join(
    base_lab %>%
      select(cod_mun, nome_mun, tx_alfab, classe_alfab),
    by = "cod_mun"
  )

# Gerar mapa
grafico_mapa_alfab <- ggplot(mapa_alfab) +
  geom_sf(aes(fill = classe_alfab), color = "white", linewidth = 0.2) +
  labs(
    title = "Sergipe: taxa de alfabetização por quartis",
    subtitle = "Classificação dos municípios segundo a taxa de alfabetização",
    fill = "Classificação"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

# Salvar na pasta do latex
ggsave(
  "../lab-latex/mapa_alfabetizacao.pdf",
  plot = grafico_mapa_alfab,
  width = 8,
  height = 6
)



# Questão 3.2


base_lab <- base_lab %>%
  mutate(
    pop_total = qte_pop_masc + qte_pop_fem,
    prop_benef_pbf = (qte_pess_benef_pbf / pop_total) * 100,
    classe_prop_benef_pbf = ntile(prop_benef_pbf, 4),
    classe_prop_benef_pbf = recode(
      classe_prop_benef_pbf,
      `1` = "Baixa participação",
      `2` = "Média baixa",
      `3` = "Média alta",
      `4` = "Alta participação"
    )
  )


tabela_benef_pbf <- base_lab %>%
  arrange(desc(prop_benef_pbf)) %>%
  mutate(posicao = row_number()) %>%
  transmute(
    Posição = posicao,
    Município = nome_mun,
    `População total` = pop_total,
    `Proporção de beneficiários (%)` = round(prop_benef_pbf, 2),
    Classificação = classe_prop_benef_pbf
  )


tab_benef_pbf_latex <- knitr::kable(
  tabela_benef_pbf,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = TRUE,
  align = c("l", "r", "r", "c"),
  caption = "Municípios sergipanos segundo a proporção de residentes beneficiados pelo Programa Bolsa Família (2025) e classificação em quartos.",
  label = "benef_pbf_sergipe"
) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "hold_position"),
    font_size = 9
  )

cat(tab_benef_pbf_latex, file = "../lab-latex/tabela_benef_pbf.tex")



## Join com o mapa
mapa_benef_pbf <- mapa_se %>%
  left_join(
    base_lab %>%
      select(cod_mun, nome_mun, prop_benef_pbf, classe_prop_benef_pbf),
    by = "cod_mun"
  )


# Gerar mapa
grafico_mapa_benef_pbf <- ggplot(mapa_benef_pbf) +
  geom_sf(aes(fill = classe_prop_benef_pbf), color = "white", linewidth = 0.2) +
  labs(
    title = "Sergipe: proporção de residentes beneficiados pelo Bolsa Família",
    subtitle = "Classificação dos municípios em quartos",
    fill = "Classificação"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )


# Salvar na pasta do latex
ggsave(
  "../lab-latex/mapa_benef_pbf.pdf",
  plot = grafico_mapa_benef_pbf,
  width = 8,
  height = 6
)



# Item 3.3


grafico_disp <- ggplot(base_lab, aes(x = prop_benef_pbf, y = tx_alfab)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Taxa de alfabetização e proporção de beneficiários do Bolsa Família",
    x = "Proporção de beneficiários do Bolsa Família (%)",
    y = "Taxa de alfabetização (%)"
  ) +
  theme_minimal()


ggsave(
  "../lab-latex/grafico_dispersao_alfab_pbf.pdf",
  plot = grafico_disp,
  width = 8,
  height = 6
)


# Item 3.4

grafico_boxplot_pbf <- ggplot(
  base_lab,
  aes(x = classe_prop_benef_pbf, y = valor_medio_benef, fill = classe_prop_benef_pbf)
) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Baixa participação" = "#66c2a5",
    "Média baixa" = "#fc8d62",
    "Média alta" = "#8da0cb",
    "Alta participação" = "#e78ac3"
  )) +
  labs(
    title = "Valor médio do Bolsa Família por classes de participação",
    x = "Classes de participação",
    y = "Valor médio do benefício"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

grafico_boxplot_pbf

ggsave(
  "../lab-latex/boxplot_valor_pbf.pdf",
  plot = grafico_boxplot_pbf,
  width = 8,
  height = 6
)

# Encontrando os outliers
limites_outlier <- base_lab %>%
  group_by(classe_prop_benef_pbf) %>%
  summarise(
    q1 = quantile(valor_medio_benef, 0.25, na.rm = TRUE),
    q3 = quantile(valor_medio_benef, 0.75, na.rm = TRUE),
    iqr = IQR(valor_medio_benef, na.rm = TRUE),
    limite_inf = q1 - 1.5 * iqr,
    limite_sup = q3 + 1.5 * iqr
  )

outliers_pbf <- base_lab %>%
  left_join(limites_outlier, by = "classe_prop_benef_pbf") %>%
  filter(valor_medio_benef < limite_inf | valor_medio_benef > limite_sup) %>%
  select(nome_mun, classe_prop_benef_pbf, valor_medio_benef)


tabela_outliers_pbf <- outliers_pbf %>%
  transmute(
    Município = nome_mun,
    `Tipo de participação` = classe_prop_benef_pbf,
    `Valor médio do benefício (R$)` = format(
      round(valor_medio_benef, 2),
      nsmall = 2,
      decimal.mark = ",",
      big.mark = "."
    )
  )

tab_outliers_pbf_latex <- knitr::kable(
  tabela_outliers_pbf,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  escape = TRUE,
  col.names = c(
    "Município",
    "Tipo de participação",
    "Valor médio do benefício (R\$)"
  ),
  align = c("l", "l", "r"),
  caption = "Municípios com valores atípicos do benefício médio do Programa Bolsa Família, segundo classes de participação.",
  label = "outliers_pbf"
) %>%
  kableExtra::kable_styling(
    latex_options = c("repeat_header", "hold_position"),
    font_size = 9
  )

cat(tab_outliers_pbf_latex, file = "../lab-latex/tabela_outliers_pbf.tex")

