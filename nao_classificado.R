# ==============================================================
# Países fora da lista manual passam a ser "Não classificado"
# (círculo vazio nos gráficos), em vez de "Seguro Social (Misto)".
#
# COMO USAR
# 1. Rode antes o corrigir_site.R (ele cria a Nota metodológica).
# 2. Coloque este arquivo na pasta "Julia", abra no RStudio e clique em Source.
# 3. Abra o index.qmd, clique em Render e confira os gráficos.
#
# Faz cópia de segurança (index_backup_nc.qmd) antes de mexer.
# ==============================================================

pasta <- if (file.exists("index.qmd")) {
  "."
} else if (file.exists("Julia/index.qmd")) {
  "Julia"
} else {
  stop("Nao achei o index.qmd. Use Session > Set Working Directory > To Files Pane Location e rode de novo.")
}

arq <- file.path(pasta, "index.qmd")
file.copy(arq, file.path(pasta, "index_backup_nc.qmd"), overwrite = FALSE)
txt <- paste(readLines(arq, encoding = "UTF-8", warn = FALSE), collapse = "\n")

relatorio <- character()
trocar <- function(de, para, rotulo, todas = FALSE) {
  if (!grepl(de, txt, fixed = TRUE)) {
    relatorio <<- c(relatorio, paste0("NAO ENCONTRADO: ", rotulo))
  } else {
    if (todas) {
      txt <<- gsub(de, para, txt, fixed = TRUE)
    } else {
      txt <<- sub(de, para, txt, fixed = TRUE)
    }
    relatorio <<- c(relatorio, paste0("ok:              ", rotulo))
  }
}

# 1. Países fora da lista deixam de virar "misto"
trocar('is.na(tipo_sistema) ~ "Seguro Social (Misto)"',
       'is.na(tipo_sistema) ~ "Não classificado"',
       "padrão 'misto' -> 'Não classificado'")

# 2. Novo nível do fator
trocar('levels = c("Universal (Público)", "Seguro Social (Misto)", "Mercado (Privado)")',
       'levels = c("Universal (Público)", "Seguro Social (Misto)", "Mercado (Privado)", "Não classificado")',
       "novo nível no fator tipo_sistema")

# 3. Nova forma no gráfico: círculo vazio (shape 1)
padrao <- '"Mercado \\(Privado\\)"[ ]*=[ ]*15[ ]*#[^\n]*'
if (grepl(padrao, txt)) {
  txt <- sub(padrao,
             '"Mercado (Privado)"       = 15,   # quadrado sólido\n  "Não classificado"        = 1     # círculo vazio',
             txt)
  relatorio <- c(relatorio, "ok:              nova forma (círculo vazio) em shapes_sistema")
} else {
  relatorio <- c(relatorio, "NAO ENCONTRADO: nova forma em shapes_sistema (edite shapes_sistema à mão)")
}

# 4. Legendas das figuras
trocar("■ Mercado (Privado).",
       "■ Mercado (Privado), ○ Não classificado (fora da lista manual de 72 países).",
       "legendas das figuras", todas = TRUE)

# 5. Nota metodológica (só existe se o corrigir_site.R já foi rodado)
trocar('os demais aparecem como "seguro social (misto)" por padrão, e as formas dos pontos devem ser lidas como ilustração.',
       'os demais aparecem como "não classificado" (círculo vazio).',
       "Nota metodológica")

writeLines(enc2utf8(txt), arq, useBytes = TRUE)

cat("\n===== RELATORIO =====\n")
cat(paste(relatorio, collapse = "\n"), "\n")
cat("\nAgora: Render no index.qmd e confira se os gráficos mostram os círculos vazios.\n")
