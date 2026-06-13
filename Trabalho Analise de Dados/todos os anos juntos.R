# ============================================================
# ANÁLISE DE DADOS: FEMINICÍDIO EM MINAS GERAIS – 2018 A 2023
# ============================================================

# ── 1. PACOTES ──────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(gganimate)
library(gifski)
library(geobr)
library(sf)

# ── 2. LEITURA E COMBINAÇÃO DOS ARQUIVOS ────────────────────
# Liste apenas os arquivos CSV na pasta de trabalho
arquivos <- list.files(pattern = "feminicidio_\\d{4}\\.csv", full.names = TRUE)

# Lê e empilha todos num único data frame
df <- map(arquivos, \(f) read_csv2(f,
                                   locale = locale(encoding = "UTF-8"),
                                   name_repair = "universal")) |>
  list_rbind()


# ── 3. LIMPEZA ──────────────────────────────────────────────
df <- df |>
  rename(
    cod_municipio = 1,
    municipio     = 2,
    data          = 3,
    mes           = 4,
    ano           = 5,
    risp          = 6,
    rmbh          = 7,
    tipo          = 8,
    vitimas       = 9
  ) |>
  mutate(
    data      = as_date(data),
    mes_label = factor(mes, levels = 1:12,
                       labels = c("Janeiro","Fevereiro","Março","Abril","Maio","Junho",
                                  "Julho","Agosto","Setembro","Outubro","Novembro","Dezembro")),
    municipio = str_to_title(municipio),
    risp      = str_trim(risp),
    cidade    = str_extract(risp, "(?<=- ).+")   # só o nome da cidade da RISP
  )

# Visão geral
glimpse(df)
cat("Anos disponíveis:", sort(unique(df$ano)), "\n")
cat("Total de vítimas:", sum(df$vitimas), "\n")

populacao_area <- tribble(
  ~rmbh,               ~ano,  ~populacao,
  "1) Belo Horizonte", 2018,  2375151,
  "1) Belo Horizonte", 2019,  2372124,
  "1) Belo Horizonte", 2020,  2360284,
  "1) Belo Horizonte", 2021,  2337922,
  "1) Belo Horizonte", 2022,  2315560,
  "1) Belo Horizonte", 2023,  2350564,
  "2) RMBH (sem BH)",  2018,  2664676,
  "2) RMBH (sem BH)",  2019,  2695533,
  "2) RMBH (sem BH)",  2020,  2730000,
  "2) RMBH (sem BH)",  2021,  2771361,
  "2) RMBH (sem BH)",  2022,  2812722,
  "2) RMBH (sem BH)",  2023,  2850000,
  "3) Interior de MG", 2018,  15077572,
  "3) Interior de MG", 2019,  15164332,
  "3) Interior de MG", 2020,  15249716,
  "3) Interior de MG", 2021,  15330706,
  "3) Interior de MG", 2022,  15411707,
  "3) Interior de MG", 2023,  15479877
)

# ══════════════════════════════════════════════════════════════
# ── 4. TABELAS-RESUMO ─────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

# 4A ── Vítimas por ano (série histórica)
por_ano <- df |>
  group_by(ano) |>
  summarise(total_vitimas = sum(vitimas), ocorrencias = n(), .groups = "drop")

# 4B ── Vítimas por ano e mês (heatmap)
por_ano_mes <- df |>
  group_by(ano, mes, mes_label) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4C ── Tentado vs Consumado por ano
por_tipo_ano <- df |>
  group_by(ano, tipo) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# 4D ── Vítimas por RISP (todos os anos)
por_risp <- df |>
  group_by(cidade) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  arrange(desc(total_vitimas))

# 4E ── Top 10 municípios (todos os anos)
top_municipios <- df |>
  group_by(municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  slice_max(total_vitimas, n = 10)

# 4F ── Por área por ano (taxa por 100 mil hab.)
por_area_ano <- df |>
  group_by(ano, rmbh) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop") |>
  left_join(populacao_area, by = c("ano", "rmbh")) |>
  mutate(taxa_100k = (total_vitimas / populacao) * 100000)

# ══════════════════════════════════════════════════════════════
# ── 5. GRÁFICOS ───────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════

tema <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# GRÁFICO 1 ── Série histórica anual (linha)
ggplot(por_ano, aes(x = ano, y = total_vitimas)) +
  geom_line(color = "#c0392b", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 4) +
  geom_text(aes(label = total_vitimas), vjust = -1, size = 3.8) +
  scale_x_continuous(breaks = unique(df$ano)) +
  labs(title = "Evolução Anual de Vítimas de Feminicídio – MG 2018–2023",
       x = NULL, y = "Nº de Vítimas") +
  tema

# GRÁFICO 2 ── Heatmap: mês x ano
ggplot(por_ano_mes, aes(x = factor(ano), y = fct_rev(mes_label), fill = total_vitimas)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = total_vitimas), size = 3) +
  scale_fill_gradient(low = "#fde8e8", high = "#c0392b") +
  labs(title = "Vítimas por Mês e Ano – MG 2018–2023",
       x = NULL, y = NULL, fill = "Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 3 ── Tentado vs Consumado por ano (barras empilhadas)
ggplot(por_tipo_ano, aes(x = factor(ano), y = total_vitimas, fill = tipo)) +
  geom_col(position = "stack") +
  geom_text(aes(label = total_vitimas), position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_manual(values = c("CONSUMADO" = "#c0392b", "TENTADO" = "#e67e22")) +
  labs(title = "Feminicídios Tentados e Consumados por Ano – MG 2018–2023",
       x = NULL, y = "Nº de Vítimas", fill = NULL) +
  tema

# GRÁFICO 4 ── Vítimas por RISP – todos os anos
ggplot(por_risp, aes(x = reorder(cidade, -total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#2980b9") +
  geom_text(aes(label = total_vitimas), vjust = -0.4, size = 3.5) +
  labs(title = "Vítimas por Região de Segurança Pública – MG 2018–2023",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.text.y        = element_blank(),
    panel.grid.major.y = element_blank()
  )

# GRÁFICO 5 ── Top 10 municípios (barras horizontais)
ggplot(top_municipios, aes(x = reorder(municipio, total_vitimas), y = total_vitimas)) +
  geom_col(fill = "#8e44ad") +
  geom_text(aes(label = total_vitimas), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top 10 Municípios com Mais Vítimas – MG 2018–2023",
       x = NULL, y = "Nº de Vítimas") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 6 ── Taxa por 100 mil hab. por área e ano
ggplot(por_area_ano, aes(x = ano, y = taxa_100k, color = rmbh, group = rmbh)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(df$ano)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Taxa de Feminicídio por 100 mil Habitantes por Área – MG 2018–2023",
       x = NULL, y = "Vítimas por 100 mil hab.", color = NULL) +
  tema

#graficos do cloud:
# 1°:
top_municipios5 |>
  plot_ly(
    x = ~total_vitimas,
    y = ~reorder(municipio, total_vitimas),
    type = "bar",
    orientation = "h",
    marker = list(color = "#8e44ad"),
    hovertemplate = "<b>%{y}</b><br>Vítimas: %{x}<extra></extra>"
  ) |>
  layout(
    title = "Top 10 Municípios – MG 2018–2023",
    xaxis = list(title = "Nº de Vítimas"),
    yaxis = list(title = "")
  )

# 2°:
# Tabela necessária
por_risp_ano <- df |>
  group_by(ano, cidade) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

por_risp_ano |>
  plot_ly(
    x = ~ano, y = ~cidade,
    size = ~total_vitimas,
    color = ~cidade,
    type = "scatter", mode = "markers",
    marker = list(sizemode = "area", opacity = 0.7),
    hovertemplate = "<b>%{y}</b><br>Ano: %{x}<br>Vítimas: %{marker.size}<extra></extra>"
  ) |>
  layout(
    title = "Vítimas por RISP e Ano (bolhas)",
    showlegend = FALSE
  )
# 3°:
# Top 10 por ano (dinâmico)
top_por_ano <- df |>
  group_by(ano, municipio) |>
  summarise(total = sum(vitimas), .groups = "drop") |>
  group_by(ano) |>
  slice_max(total, n = 10) |>
  mutate(rank = rank(-total, ties.method = "first"))

p_race <- ggplot(top_por_ano, aes(x = rank, y = total, fill = municipio)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = municipio), hjust = 1.05, size = 4) +
  geom_text(aes(label = total), hjust = -0.1, size = 4) +
  coord_flip(clip = "off") +
  scale_x_reverse() +
  labs(title = "Ano: {closest_state}", x = NULL, y = "Nº de Vítimas") +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  transition_states(ano, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

animate(p_race, nframes = 120, fps = 10, renderer = gifski_renderer())

# 4°:
por_ano_mes_acc <- por_ano_mes |>
  arrange(ano, mes) |>
  mutate(ponto_id = row_number())

p_linha <- ggplot(por_ano_mes_acc,
                  aes(x = mes, y = total_vitimas, color = factor(ano), group = ano)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                                "Jul","Ago","Set","Out","Nov","Dez")) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Vítimas por Mês – até {frame_along}",
       color = "Ano", x = NULL, y = "Nº de Vítimas") +
  theme_minimal() +
  transition_reveal(ponto_id)

animate(p_linha, nframes = 80, fps = 8, renderer = gifski_renderer())

# 5°:

mapa_mg <- st_read("MG_Municipios_2020.shp", quiet = TRUE)

# Ver nomes das colunas para saber qual usar no join
names(mapa_mg)

# ── 1. Vítimas totais por município (todos os anos) ──
por_municipio_cod <- bind_rows(df) |>
  group_by(cod_municipio) |>
  summarise(total_vitimas = sum(vitimas), .groups = "drop")

# ── 2. Ajuste do código IBGE (6 → 7 dígitos) ──
por_municipio_cod <- por_municipio_cod |>
  mutate(cod_municipio = as.character(cod_municipio),
         cod_municipio = if_else(nchar(cod_municipio) == 6,
                                 paste0(cod_municipio, "0"),
                                 cod_municipio))

# ── 3. Join com o shapefile ──
mapa_dados <- mapa_mg |>
  left_join(por_municipio_cod, by = c("CD_MUN" = "cod_municipio")) |>
  mutate(total_vitimas = replace_na(total_vitimas, 0))

# ── 4. Verificar se o join funcionou ──
sum(mapa_dados$total_vitimas)  # deve ser > 0

gg <- ggplot(mapa_dados) +
  geom_sf(aes(fill = total_vitimas,text = paste0(name_muni, "\nVítimas: ", total_vitimas)),
          color = "white", linewidth = 0.1) +
  scale_fill_gradientn(
    colours = c("#fde8e8", "#f1948a", "#c0392b", "#7b241c"),
    name = "Vítimas",
    na.value = "grey90"
  ) +
  labs(
    title = "Feminicídios por Município – MG 2018–2022",
    caption = "Fonte: SSP-MG"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
ggplotly(gg, tooltip = "text")