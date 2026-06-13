install.packages("lubridate")
install.packages("scales")
# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS - 2018
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
# Instale uma vez com: install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)   # leitura, limpeza e gráficos (ggplot2 incluso)
library(lubridate)   # manipulação de datas
library(scales)      # formatação de eixos nos gráficos


# ── 2. LEITURA E LIMPEZA ────────────────────────────────────
df <- read_csv2("feminicidio_2018.csv",
               locale = locale(encoding = "UTF-8"),
               name_repair = "universal") |>  # remove BOM e caracteres especiais dos nomes
  
  # renomear pelo índice (posição) para evitar erro com BOM mark na col 1
  rename(
    cod_municipio = 1,
    municipio     = 2,   # municipio_fato
    data          = 3,   # data_fato
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,   # tentado_consumado
    vitimas       = 9    # qtde_vitimas
  ) |>
  
  # garantir tipo correto para data
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),    municipio = str_to_title(municipio),   # "UBERABA" → "Uberaba"
    risp      = str_trim(risp)
  )


# ── 3. VISÃO GERAL DO BANCO ─────────────────────────────────
glimpse(df)       # tipos e primeiros valores
summary(df)       # estatísticas gerais
count(df, tipo)   # quantos tentados x consumados
# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO (bases para gráficos) ──────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por mês
por_mes <- df |>
  group_by(mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(mes)

# 4B ── Vítimas por região (RISP)
por_risp <- df |>
  group_by(risp) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4C ── Tentado vs Consumado por mês
por_tipo_mes <- df |>
  group_by(mes, mes_label, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Top 10 municípios
top_municipios <- df |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4E ── Por área (BH / RMBH / Interior)
por_area <- df |>
  group_by(rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")


# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# Tema base
tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Vítimas por mês (linha)
ggplot(por_mes, aes(x = mes, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  labs(title = "Vítimas de Feminicídio por Mês – MG 2018",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Tentado vs Consumado por mês (barras empilhadas)
ggplot(por_tipo_mes, aes(x = mes, y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Mês – MG 2018",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 3 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2018",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 4 ── Vítimas por RISP (barras verticais)
por_risp |>
  mutate(cidade = str_extract(risp, "(?<=- ).+")) |>
  ggplot(aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2018",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Vítimas por área (pizza simples)
ggplot(por_area, aes(x = "", y = total_vitimas, fill = rmbh)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(rmbh, "\n",
                               round(total_vitimas / sum(total_vitimas) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribuição de Vítimas por Área – MG 2018", fill = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))


# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS - 2019
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
# Instale uma vez com: install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)   # leitura, limpeza e gráficos (ggplot2 incluso)
library(lubridate)   # manipulação de datas
library(scales)      # formatação de eixos nos gráficos


# ── 2. LEITURA E LIMPEZA ────────────────────────────────────
df2 <- read_csv2("feminicidio_2019.csv",
                 locale = locale(encoding = "UTF-8"),
                 name_repair = "universal") |>  # remove BOM e caracteres especiais dos nomes
  
  # renomear pelo índice (posição) para evitar erro com BOM mark na col 1
  rename(
    cod_municipio = 1,
    municipio     = 2,   # municipio_fato
    data          = 3,   # data_fato
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,   # tentado_consumado
    vitimas       = 9    # qtde_vitimas
  ) |>
  
  # garantir tipo correto para data
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),    municipio = str_to_title(municipio),   # "UBERABA" → "Uberaba"
    risp      = str_trim(risp)
  )


# ── 3. VISÃO GERAL DO BANCO ─────────────────────────────────
glimpse(df2)       # tipos e primeiros valores
summary(df2)       # estatísticas gerais
count(df2, tipo)   # quantos tentados x consumados

# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO (bases para gráficos) ──────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por mês
por_mes <- df2 |>
  group_by(mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(mes)

# 4B ── Vítimas por região (RISP)
por_risp <- df2 |>
  group_by(risp) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4C ── Tentado vs Consumado por mês
por_tipo_mes <- df2 |>
  group_by(mes, mes_label, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Top 10 municípios
top_municipios <- df2 |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4E ── Por área (BH / RMBH / Interior)
por_area <- df2 |>
  group_by(rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")


# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# Tema base
tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Vítimas por mês (linha)
ggplot(por_mes, aes(x = mes, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  labs(title = "Vítimas de Feminicídio por Mês – MG 2019",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Tentado vs Consumado por mês (barras empilhadas)
ggplot(por_tipo_mes, aes(x = mes, y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Mês – MG 2019",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 3 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2019",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 4 ── Vítimas por RISP (barras verticais)
por_risp |>
  mutate(cidade = str_extract(risp, "(?<=- ).+")) |>
  ggplot(aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2019",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Vítimas por área (pizza simples)
ggplot(por_area, aes(x = "", y = total_vitimas, fill = rmbh)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(rmbh, "\n",round(total_vitimas / sum(total_vitimas) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribuição de Vítimas por Área – MG 2019", fill = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS - 2020
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
# Instale uma vez com: install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)   # leitura, limpeza e gráficos (ggplot2 incluso)
library(lubridate)   # manipulação de datas
library(scales)      # formatação de eixos nos gráficos


# ── 2. LEITURA E LIMPEZA ────────────────────────────────────
df1 <- read_csv2("feminicidio_2020.csv",
                 locale = locale(encoding = "UTF-8"),
                 name_repair = "universal") |>  # remove BOM e caracteres especiais dos nomes
  
  # renomear pelo índice (posição) para evitar erro com BOM mark na col 1
  rename(
    cod_municipio = 1,
    municipio     = 2,   # municipio_fato
    data          = 3,   # data_fato
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,   # tentado_consumado
    vitimas       = 9    # qtde_vitimas
  ) |>
  
  # garantir tipo correto para data
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),    municipio = str_to_title(municipio),   # "UBERABA" → "Uberaba"
    risp      = str_trim(risp)
  )


# ── 3. VISÃO GERAL DO BANCO ─────────────────────────────────
glimpse(df1)       # tipos e primeiros valores
summary(df1)       # estatísticas gerais
count(df1, tipo)   # quantos tentados x consumados

# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO (bases para gráficos) ──────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por mês
por_mes <- df1 |>
  group_by(mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(mes)

# 4B ── Vítimas por região (RISP)
por_risp <- df1 |>
  group_by(risp) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4C ── Tentado vs Consumado por mês
por_tipo_mes <- df1 |>
  group_by(mes, mes_label, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Top 10 municípios
top_municipios <- df1 |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4E ── Por área (BH / RMBH / Interior)
por_area <- df1 |>
  group_by(rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")


# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# Tema base
tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Vítimas por mês (linha)
ggplot(por_mes, aes(x = mes, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  labs(title = "Vítimas de Feminicídio por Mês – MG 2020",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Tentado vs Consumado por mês (barras empilhadas)
ggplot(por_tipo_mes, aes(x = mes, y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Mês – MG 2020",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 3 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2020",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 4 ── Vítimas por RISP (barras verticais)
por_risp |>
  mutate(cidade = str_extract(risp, "(?<=- ).+")) |>
  ggplot(aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2020",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Vítimas por área (pizza simples)
ggplot(por_area, aes(x = "", y = total_vitimas, fill = rmbh)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(rmbh, "\n", total_vitimas)),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribuição de Vítimas por Área – MG 2020", fill = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS - 2021
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
# Instale uma vez com: install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)   # leitura, limpeza e gráficos (ggplot2 incluso)
library(lubridate)   # manipulação de datas
library(scales)      # formatação de eixos nos gráficos


# ── 2. LEITURA E LIMPEZA ────────────────────────────────────
df3 <- read_csv2("feminicidio_2021.csv",
                 locale = locale(encoding = "UTF-8"),
                 name_repair = "universal") |>  # remove BOM e caracteres especiais dos nomes
  
  # renomear pelo índice (posição) para evitar erro com BOM mark na col 1
  rename(
    cod_municipio = 1,
    municipio     = 2,   # municipio_fato
    data          = 3,   # data_fato
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,   # tentado_consumado
    vitimas       = 9    # qtde_vitimas
  ) |>
  
  # garantir tipo correto para data
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),    municipio = str_to_title(municipio),   # "UBERABA" → "Uberaba"
    risp      = str_trim(risp)
  )


# ── 3. VISÃO GERAL DO BANCO ─────────────────────────────────
glimpse(df3)       # tipos e primeiros valores
summary(df3)       # estatísticas gerais
count(df3, tipo)   # quantos tentados x consumados

# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO (bases para gráficos) ──────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por mês
por_mes <- df3 |>
  group_by(mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(mes)

# 4B ── Vítimas por região (RISP)
por_risp <- df3 |>
  group_by(risp) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4C ── Tentado vs Consumado por mês
por_tipo_mes <- df3 |>
  group_by(mes, mes_label, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Top 10 municípios
top_municipios <- df3 |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4E ── Por área (BH / RMBH / Interior)
por_area <- df3 |>
  group_by(rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")


# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# Tema base
tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Vítimas por mês (linha)
ggplot(por_mes, aes(x = mes, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  labs(title = "Vítimas de Feminicídio por Mês – MG 2021",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Tentado vs Consumado por mês (barras empilhadas)
ggplot(por_tipo_mes, aes(x = mes, y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Mês – MG 2021",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 3 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2021",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 4 ── Vítimas por RISP (barras verticais)
por_risp |>
  mutate(cidade = str_extract(risp, "(?<=- ).+")) |>
  ggplot(aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2021",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Vítimas por área (pizza simples)
ggplot(por_area, aes(x = "", y = total_vitimas, fill = rmbh)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(rmbh, "\n", total_vitimas)),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribuição de Vítimas por Área – MG 2021", fill = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS - 2022
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
# Instale uma vez com: install.packages(c("tidyverse", "lubridate", "scales"))
library(tidyverse)   # leitura, limpeza e gráficos (ggplot2 incluso)
library(lubridate)   # manipulação de datas
library(scales)      # formatação de eixos nos gráficos


# ── 2. LEITURA E LIMPEZA ────────────────────────────────────
df4 <- read_csv2("feminicidio_2022.csv",
                 locale = locale(encoding = "UTF-8"),
                 name_repair = "universal") |>  # remove BOM e caracteres especiais dos nomes
  
  # renomear pelo índice (posição) para evitar erro com BOM mark na col 1
  rename(
    cod_municipio = 1,
    municipio     = 2,   # municipio_fato
    data          = 3,   # data_fato
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,   # tentado_consumado
    vitimas       = 9    # qtde_vitimas
  ) |>
  
  # garantir tipo correto para data
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),    municipio = str_to_title(municipio),   # "UBERABA" → "Uberaba"
    risp      = str_trim(risp)
  )


# ── 3. VISÃO GERAL DO BANCO ─────────────────────────────────
glimpse(df4)       # tipos e primeiros valores
summary(df4)       # estatísticas gerais
count(df4, tipo)   # quantos tentados x consumados

# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO (bases para gráficos) ──────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por mês
por_mes <- df4 |>
  group_by(mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(mes)

# 4B ── Vítimas por região (RISP)
por_risp <- df4 |>
  group_by(risp) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4C ── Tentado vs Consumado por mês
por_tipo_mes <- df4 |>
  group_by(mes, mes_label, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Top 10 municípios
top_municipios <- df4 |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4E ── Por área (BH / RMBH / Interior)
por_area <- df4 |>
  group_by(rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")


# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# Tema base
tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Vítimas por mês (linha)
ggplot(por_mes, aes(x = mes, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  labs(title = "Vítimas de Feminicídio por Mês – MG 2022",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Tentado vs Consumado por mês (barras empilhadas)
ggplot(por_tipo_mes, aes(x = mes, y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1:12, labels = por_mes$mes_label) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Mês – MG 2022",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 3 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2022",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 4 ── Vítimas por RISP (barras verticais)
por_risp |>
  mutate(cidade = str_extract(risp, "(?<=- ).+")) |>
  ggplot(aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2022",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Vítimas por área (pizza simples)
ggplot(por_area, aes(x = "", y = total_vitimas, fill = rmbh)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(rmbh, "\n", total_vitimas)),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribuição de Vítimas por Área – MG 2022", fill = NULL) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))