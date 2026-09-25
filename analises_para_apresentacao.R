# ==============================================================
# Números para a apresentação do SIA
# Roda sozinho (não precisa executar o .qmd antes).
#
# COMO USAR
# 1. Coloque este arquivo na pasta "Julia" (onde está o dados_saude_mundo.csv).
# 2. Abra no RStudio e clique em "Source".
# 3. Copie TUDO o que aparecer no Console e me mande.
#    Com isso eu preencho os [confirme] do roteiro com os números reais.
# ==============================================================

suppressPackageStartupMessages(library(dplyr))

arq <- if (file.exists("dados_saude_mundo.csv")) {
  "dados_saude_mundo.csv"
} else {
  "Julia/dados_saude_mundo.csv"
}
if (!file.exists(arq)) {
  stop("Nao achei dados_saude_mundo.csv. Use Session > Set Working Directory > To Files Pane Location e rode de novo.")
}
dados <- read.csv(arq, stringsAsFactors = FALSE)

titulo <- function(x) cat("\n=====", x, "=====\n")
arred <- function(df) {
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) df[[i]] <- round(df[[i]], 1)
  }
  df
}

# --- Lista manual de sistemas (copiada do seu index.qmd) -----------
sistema_saude <- data.frame(
  iso3c = c(
    "BRA", "GBR", "CUB", "CAN", "ESP", "ITA", "NOR", "SWE", "DNK", "FIN",
    "PRT", "GRC", "HRV", "CZE", "EST", "LTU", "LVA", "SVK", "SVN", "HUN",
    "POL", "AUS", "NZL", "KOR", "THA", "MYS", "SRI", "CRI", "ARG", "MEX",
    "CHL", "COL", "URY",
    "DEU", "FRA", "AUT", "BEL", "NLD", "CHE", "LUX", "ISR", "JPN", "CHN",
    "IND", "IDN", "PHL", "VNM", "EGY", "IRN", "MAR", "TUN", "ZAF", "BWA",
    "TUR", "RUS",
    "USA", "SGP", "HKG", "NGA", "PAK", "BGD", "KEN", "ETH", "GHA", "PER",
    "GTM", "HND", "NIC", "SLV", "DOM", "JAM", "TTO"
  ),
  tipo_sistema = c(
    rep("Universal (Público)", 33),
    rep("Seguro Social (Misto)", 22),
    rep("Mercado (Privado)", 17)
  ),
  stringsAsFactors = FALSE
)

dados_ano <- dados |>
  filter(year == 2022, region != "Aggregates") |>
  left_join(sistema_saude, by = "iso3c") |>
  mutate(
    na_lista = !is.na(tipo_sistema),
    tipo_sistema = ifelse(is.na(tipo_sistema), "Não classificado", tipo_sistema)
  )

dados_g1 <- dados_ano |>
  filter(!is.na(expectativa_vida), !is.na(gasto_saude_percap), expectativa_vida >= 30)

# --- 0. Colunas ------------------------------------------------------
titulo("0. Colunas do CSV (procure a participacao do gasto no PIB)")
print(names(dados))

# --- 1. Reta de tendência -------------------------------------------
titulo("1. Reta de tendencia (expectativa de vida ~ log10 do gasto)")
m1 <- lm(expectativa_vida ~ log10(gasto_saude_percap), data = dados_g1)
cat("Paises usados na reta:", nrow(dados_g1), "\n")
cat("Anos ganhos a cada 10x de gasto:", round(coef(m1)[2], 1), "\n")
cat("R2 (fracao da variacao explicada):", round(summary(m1)$r.squared, 2), "\n")
prev <- predict(m1, data.frame(gasto_saude_percap = c(50, 500, 5000)))
cat("Previsto para US$ 50, 500 e 5000:", round(prev, 1), "\n")

# --- 2. Resíduos -----------------------------------------------------
titulo("2. Residuos (observado - previsto): positivo = ACIMA da linha")
dados_g1 <- dados_g1 |>
  mutate(
    previsto = predict(m1, newdata = dados_g1),
    residuo = expectativa_vida - previsto
  )

destaques <- dados_g1 |>
  filter(country %in% c("Brazil", "United States", "China", "Cuba", "Singapore") |
           grepl("Hong Kong", country)) |>
  select(country, gasto_saude_percap, expectativa_vida, previsto, residuo, tipo_sistema)
print(arred(as.data.frame(destaques)))

cat("\nMaiores residuos POSITIVOS (mais acima da linha):\n")
print(arred(as.data.frame(
  dados_g1 |> arrange(desc(residuo)) |>
    select(country, gasto_saude_percap, residuo, tipo_sistema) |> head(8)
)))
cat("\nMaiores residuos NEGATIVOS (mais abaixo da linha):\n")
print(arred(as.data.frame(
  dados_g1 |> arrange(residuo) |>
    select(country, gasto_saude_percap, residuo, tipo_sistema) |> head(8)
)))

# --- 3. Dispersão ----------------------------------------------------
titulo("3. Dispersao: amplitude da expectativa de vida por faixa de gasto")
faixas <- dados_g1 |>
  mutate(faixa = cut(gasto_saude_percap, breaks = c(0, 100, 300, 1000, 3000, Inf))) |>
  group_by(faixa) |>
  summarise(
    n = n(),
    minimo = min(expectativa_vida),
    maximo = max(expectativa_vida),
    amplitude = max(expectativa_vida) - min(expectativa_vida)
  )
print(arred(as.data.frame(faixas)))

cat("\nPaises com gasto parecido (entre 2/3 e 1,5x o gasto do pais):\n")
vizinhos <- function(pais, fator = 1.5) {
  g <- dados_g1$gasto_saude_percap[dados_g1$country == pais]
  if (length(g) != 1) {
    cat(pais, ": nao encontrado\n")
    return(invisible(NULL))
  }
  v <- dados_g1[dados_g1$gasto_saude_percap >= g / fator &
                  dados_g1$gasto_saude_percap <= g * fator, ]
  cat(pais, ": ", nrow(v), " paises; expectativa de vida de ",
      round(min(v$expectativa_vida), 1), " a ", round(max(v$expectativa_vida), 1),
      " (diferenca de ", round(max(v$expectativa_vida) - min(v$expectativa_vida), 1),
      " anos)\n", sep = "")
}
vizinhos("Brazil")
vizinhos("United States")
vizinhos("China")
vizinhos("Cuba")

# --- 4. Classificação ------------------------------------------------
titulo("4. Classificacao dos sistemas")
print(as.data.frame(dados_ano |> count(na_lista, tipo_sistema)))
cat("Paises da lista manual que NAO estao na base de 2022:",
    setdiff(sistema_saude$iso3c, dados_ano$iso3c), "\n")

# --- 5. Quantos países -------------------------------------------------
titulo("5. Quantos paises entram em cada analise")
cat("Linhas da base em 2022 (sem agregados):", nrow(dados_ano), "\n")
cat("Com gasto e expectativa de vida:",
    sum(!is.na(dados_ano$gasto_saude_percap) & !is.na(dados_ano$expectativa_vida)), "\n")
cat("Com gasto e mortalidade infantil:",
    sum(!is.na(dados_ano$gasto_saude_percap) & !is.na(dados_ano$mortalidade_infantil)), "\n")

# --- 6. Japão x Serra Leoa -------------------------------------------
titulo("6. Japao x Serra Leoa")
js <- dados_ano[dados_ano$country %in% c("Japan", "Sierra Leone"), c("country", "expectativa_vida")]
print(js)
if (nrow(js) == 2) cat("Diferenca:", round(abs(diff(js$expectativa_vida)), 1), "anos\n")

# --- 7. Gasto x mortalidade infantil ---------------------------------
titulo("7. Gasto x mortalidade infantil")
d3 <- dados_ano |>
  filter(!is.na(mortalidade_infantil), !is.na(gasto_saude_percap), mortalidade_infantil > 0)
cat("Paises:", nrow(d3), "\n")
cat("Correlacao (log-log):",
    round(cor(log10(d3$gasto_saude_percap), log10(d3$mortalidade_infantil)), 2), "\n")
cat("Correlacao de Spearman:",
    round(cor(d3$gasto_saude_percap, d3$mortalidade_infantil, method = "spearman"), 2), "\n")

cat("\n===== FIM: copie tudo acima e me mande =====\n")
