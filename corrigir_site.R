# ==============================================================
# Correções de TEXTO do site (index.qmd e about.qmd)
# Não mexe em nenhum código R, só em frases, legendas e links.
#
# COMO USAR
# 1. Coloque este arquivo na mesma pasta do index.qmd (a pasta "Julia").
# 2. Abra no RStudio e clique em "Source" (canto superior direito do editor).
# 3. Leia o relatório que aparece no Console (ok / NÃO ENCONTRADO).
# 4. Abra o index.qmd, clique em Render e confira as figuras e o texto.
#
# SEGURANÇA
# - Antes de mexer, o script salva index_backup.qmd e about_backup.qmd.
# - Se não quiser uma correção, apague (ou comente com #) a linha dela.
# - Se der "NÃO ENCONTRADO", a frase no seu arquivo está um pouco
#   diferente; faça aquela troca à mão.
# ==============================================================

pasta <- if (file.exists("index.qmd")) {
  "."
} else if (file.exists("Julia/index.qmd")) {
  "Julia"
} else {
  stop("Nao achei o index.qmd. Use Session > Set Working Directory > To Files Pane Location e rode de novo.")
}

ler <- function(arq) {
  paste(readLines(file.path(pasta, arq), encoding = "UTF-8", warn = FALSE), collapse = "\n")
}
salvar <- function(texto, arq) {
  writeLines(enc2utf8(texto), file.path(pasta, arq), useBytes = TRUE)
}

# cópias de segurança (não sobrescreve se já existirem)
file.copy(file.path(pasta, "index.qmd"), file.path(pasta, "index_backup.qmd"), overwrite = FALSE)
file.copy(file.path(pasta, "about.qmd"), file.path(pasta, "about_backup.qmd"), overwrite = FALSE)

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

# ==============================================================
# index.qmd
# ==============================================================
txt <- ler("index.qmd")

# --- 0. Estrutura ------------------------------------------------
# Remove a linha em branco entre ```{r} e "#| label" (as opções do chunk
# devem ficar logo no início; isso é seguro mesmo se já funcionava).
trocar("```{r}\n\n#|", "```{r}\n#|", "linha em branco antes de #| label", todas = TRUE)

# Legenda errada do gráfico por região (estava copiada da de mortalidade)
if (grepl("#| label: fig-expectativa-regiao\n#| fig-cap: ", txt, fixed = TRUE)) {
  txt <- sub('(#\\| label: fig-expectativa-regiao\n#\\| fig-cap: )"[^\n]*"',
             '\\1"Expectativa de vida média ao nascer por região, 2022."', txt)
  relatorio <- c(relatorio, "ok:              legenda do gráfico por região")
} else {
  relatorio <- c(relatorio, "NAO ENCONTRADO: legenda do gráfico por região")
}

# geom_segment: "size" está obsoleto para linhas, o certo é "linewidth"
trocar("inherit.aes = FALSE, alpha = 0.5, size = 0.5, show.legend = FALSE",
       "inherit.aes = FALSE, alpha = 0.5, linewidth = 0.5, show.legend = FALSE",
       "size -> linewidth nas linhas de apontamento", todas = TRUE)

# --- 1. Introdução -----------------------------------------------
trocar("podia esperar viver em média 30 anos a menos do que uma criança nascida no Japão",
       "podia esperar viver em média cerca de 23 anos a menos do que uma criança nascida no Japão (61 contra 84 anos, segundo o Banco Mundial)",
       "Serra Leoa x Japão: 30 anos -> cerca de 23")

trocar("e por que os sistemas públicos e universais entregam retornos sociais muito superiores aos modelos privados?",
       "e por que países com gasto semelhante obtêm resultados tão diferentes, inclusive quando organizam o financiamento de formas distintas?",
       "pergunta final da introdução (alinhada ao resumo)")

# --- 2. Retornos decrescentes ------------------------------------
trocar("padrão amplamente documentado na literatura da OCDE: o aumento de 1% no gasto em saúde eleva a expectativa de vida em apenas 0,001%, enquanto fatores socioeconômicos e ambientais exercem influência comparativamente maior (Grima et al., 2022).",
       "padrão que também aparece em estudos sobre países da OCDE (Morina et al., 2022).",
       "Grima et al. -> Morina et al. e número 0,001%")

trocar("Esse padrão, conhecido na literatura econômica como a curva de Preston, foi documentado empiricamente por Cutler, Deaton e Lleras-Muney ([2006](https://www.aeaweb.org/articles?id=10.1257/jep.20.3.97)), que demonstram que, enquanto países pobres convertem investimentos básicos em salto de longevidade, nações ricas precisam gastar proporções muito maiores para obter ganhos incrementais mínimos.",
       "Esse padrão lembra a chamada curva de Preston (Preston, 1975), que relaciona longevidade e renda, e o debate sobre os determinantes da mortalidade em Cutler, Deaton e Lleras-Muney ([2006](https://www.aeaweb.org/articles?id=10.1257/jep.20.3.97)), que destacam o papel do progresso científico e tecnológico.",
       "curva de Preston (Preston 1975; Cutler et al. 2006)")

trocar("Isso não é uma falha, é um teto biológico. A vida humana tem limites naturais, e a partir de certo patamar financeiro o dinheiro garante apenas avanços incrementais cada vez menores.",
       "Isso é o que se chama de retornos decrescentes: a partir de certo patamar, o dinheiro extra compra ganhos cada vez menores.",
       "teto biológico -> retornos decrescentes")

trocar("Quando a saúde é estruturada como um bem público universal, cada dólar investido é otimizado para salvar vidas.",
       "Quando a saúde é estruturada como um bem público universal, o acesso tende a ser mais amplo e o dinheiro tende a render mais em anos de vida.",
       "frase sobre bem público universal (menos absoluta)")

trocar("Quando o sistema é guiado pelo lucro, somas astronômicas são consumidas por despesas administrativas e margens privadas, sem se traduzir em ganhos equivalentes de vida para a população.",
       "Em sistemas guiados pelo mercado, parte relevante dos recursos pode ser consumida por custos administrativos e margens privadas, o que pode reduzir o retorno em anos de vida.",
       "frase sobre sistemas guiados pelo lucro (hipótese, não fato)")

# --- 3. Quadros dos países ---------------------------------------
trocar("demonstrando eficiência social quando comparado a sistemas de custo muito superior",
       "sugerindo eficiência social relativa quando comparado a sistemas de custo muito superior",
       "Brasil: demonstrando -> sugerindo")

trocar("demonstrando que gastar muito não significa proteger a população ([Reid, 2010](https://en.wikipedia.org/wiki/The_Healing_of_America))",
       "sugerindo que gastar muito não basta para proteger a população (Reid, 2010)",
       "EUA: tira o link da Wikipédia")

trocar("Essa forte presença estatal impulsionou a expectativa de vida do país para patamares comparáveis aos de economias desenvolvidas ([Barata, 2009](https://www.scielo.br/j/csp)).",
       "Em paralelo, a expectativa de vida do país chegou a patamares comparáveis aos de economias desenvolvidas. A cobertura acima de 95% é dado oficial ([Commonwealth Fund, 2026](https://www.commonwealthfund.org/sites/default/files/2026-04/2026_Country-Profiles_China.pdf)).",
       "China: Barata -> Commonwealth Fund, com 'dado oficial'")

# --- 4. Duas faces / mortalidade infantil ------------------------
trocar("(Gráfico 1)", "(Figura 1)", "Gráfico 1 -> Figura 1")
trocar("(Gráfico 2)", "(Figura 2)", "Gráfico 2 -> Figura 2")

trocar("Nenhum país consegue oferecer uma vida longa à sua população adulta se, primeiro, não garantir que suas crianças sobrevivam ao primeiro ano de vida.",
       "Nenhum país consegue oferecer uma vida longa à sua população adulta se, primeiro, não garantir que suas crianças sobrevivam ao primeiro ano de vida. Vale lembrar que essa ligação é em parte aritmética, já que a mortalidade infantil entra no cálculo da expectativa de vida.",
       "ressalva aritmética (igual ao resumo)")

# --- 5. Geopolítica / mapas --------------------------------------
trocar("regiões submetidas a crises de financiamento e desmonte estatal sofrem com a perda prematura de vidas",
       "regiões com menor renda e menos investimento em saúde ainda sofrem com a perda prematura de vidas",
       "regiões: frase sem fonte")

trocar("transformam cuidados médicos em riscos de falência pessoal.Nos Estados Unidos, estima-se que dois terços das falências pessoais estejam associadas a despesas médicas, um fenômeno praticamente inexistente em outras nações desenvolvidas",
       "podem transformar cuidados médicos em riscos de falência pessoal. Nos Estados Unidos, uma pesquisa com pessoas que declararam falência entre 2013 e 2016 encontrou que 66,5% citaram problemas médicos, como contas ou perda de renda por doença, como contribuintes",
       "falências (Himmelstein): número e espaço depois do ponto")

trocar("([Himmelstein et al., 2019](https://doi.org/10.2105/AJPH.2018.304901); [Forbes, 2026](https://www.forbes.com/sites/joshuacohen/2026/04/05/increasing-burdens-of-medical-debt-and-bankruptcy-are-uniquely-american/))",
       "([Himmelstein et al., 2019](https://doi.org/10.2105/AJPH.2018.304901))",
       "tira o Forbes")

trocar("conseguem romper a barreira do subdesenvolvimento social para estender o tempo de vida de sua população",
       "parecem conseguir estender o tempo de vida de sua população, mesmo com renda limitada",
       "países em desenvolvimento (menos absoluto)")

trocar("O contraste não é acidental, é o resultado direto de décadas de investimento (ou abandono) em políticas públicas universais.",
       "O contraste não é acidental: ele acompanha décadas de diferenças de investimento em saúde e de acesso a políticas públicas.",
       "mapa de expectativa: 'resultado direto'")

# --- 6. Nota metodológica (callout, para não pegar a letra capitular) ---
trocar("## Conclusão: A saúde pública como direito inalienável",
       paste0('::: {.callout-note title="Nota metodológica"}\n',
              'Esta é uma análise descritiva e exploratória, sem modelagem inferencial: as linhas de tendência são ajustes lineares sobre o logaritmo do gasto e mostram associação, não causa e efeito. ',
              'A classificação dos sistemas em universal, seguro social e mercado foi feita manualmente para 72 países, adaptada da OMS (2010) e de Reid; os demais aparecem como "seguro social (misto)" por padrão, e as formas dos pontos devem ser lidas como ilustração. ',
              'Parte dos dados é reportada pelos governos, e a mortalidade infantil e a expectativa de vida não são medidas independentes.\n',
              ':::\n\n',
              '## Conclusão: A saúde pública como direito inalienável'),
       "novo quadro 'Nota metodológica' antes da Conclusão")

# --- 7. Conclusão -------------------------------------------------
trocar("apontam para uma conclusão inequívoca: o investimento em saúde pública e universal não é um gasto financeiro, mas o mecanismo mais poderoso de redistribuição de renda e justiça social que uma sociedade pode construir.",
       "sugerem que o investimento em saúde pública e universal está associado a melhores resultados de longevidade e merece ser visto como política de proteção social, e não apenas como gasto financeiro.",
       "conclusão inequívoca -> sugerem")

trocar("a sua orientação ideológica e operacional é determinante.",
       "a forma de organizá-los e distribuí-los também importa: em faixas parecidas de gasto por habitante, há países separados por mais de quinze anos de expectativa de vida.",
       "orientação ideológica -> dispersão (resumo)")

trocar("O mercado provou ser incapaz de distribuir cuidados médicos de forma justa ou eficiente.",
       "O modelo de financiamento pesa, mas não determina sozinho o resultado: há países de gasto alto com resultados aquém do esperado e países de gasto moderado acima do esperado.",
       "'o mercado provou ser incapaz'")

trocar("É a garantia estatal, representada no Brasil pelo SUS, que impede que a longevidade seja um privilégio dos mais abastados.",
       "No Brasil, a garantia estatal representada pelo SUS ajuda a impedir que a longevidade seja um privilégio dos mais abastados.",
       "SUS 'impede' -> 'ajuda a impedir'")

trocar("é o único caminho comprovado para que",
       "é um caminho central para que",
       "'único caminho comprovado'")

trocar('causas.Como bem observou Ariano Suassuna, "a injustiça brasileira secular dilacera o Brasil em dois países distintos: o país dos privilegiados e o país dos despossuídos". Essa frase, que parece literária, é um diagnóstico sociológico preciso: no Brasil,',
       'causas. A distinção entre um "Brasil oficial" e um "Brasil real", cara a Ariano Suassuna, ajuda a entender esse quadro: no Brasil,',
       "citação de Suassuna (sem fonte) -> paráfrase")

salvar(txt, "index.qmd")

# ==============================================================
# about.qmd
# ==============================================================
txt <- ler("about.qmd")

trocar("Está aberta a conversas sobre dados",
       "Estou aberta a conversas sobre dados",
       "about: 'Está aberta' -> 'Estou aberta' (mesma pessoa gramatical)")

trocar("R (`tidyverse`, `ggplot2`, `plotly`)",
       "R (`WDI`, `dplyr`, `ggplot2`, `plotly`)",
       "about: pacotes realmente usados")

trocar("sob orientação do Prof. Guaraci, na área",
       "sob orientação do Prof. Guaraci de Lima Requena, na área",
       "about: nome completo do orientador")

salvar(txt, "about.qmd")

# ==============================================================
# Relatório
# ==============================================================
cat("\n===== RELATORIO =====\n")
cat(paste(relatorio, collapse = "\n"), "\n")

cat("\n===== CONFERENCIA NOS SEUS DADOS =====\n")
csv <- file.path(pasta, "dados_saude_mundo.csv")
if (file.exists(csv)) {
  d <- read.csv(csv, stringsAsFactors = FALSE)
  x <- d[d$year == 2022 & d$country %in% c("Japan", "Sierra Leone"), c("country", "expectativa_vida")]
  print(x)
  if (nrow(x) == 2) {
    cat("Diferença Japão - Serra Leoa no SEU CSV:", round(abs(diff(x$expectativa_vida)), 1), "anos\n")
    cat("(o texto novo diz 'cerca de 23 anos', 61 contra 84; se der outro valor, ajuste a frase)\n")
  }
}

cat("\n===== FALTA FAZER A MAO =====\n")
cat("1. Links de Neri (2002), Ferreira Junior et al. (2013) e Barata (2009): hoje apontam para a\n   página inicial da revista. Troque pelo link de cada artigo e confira se sustenta a frase.\n")
cat("2. Reid: o comentário do código e a legenda do gráfico dizem 'Reid (2017)', o texto diz 2010.\n   Veja qual edição você usou e deixe igual em todos os lugares.\n")
cat("3. Cuba (Morais & Santos, 2015): confirme o ano do artigo.\n")
cat("4. Paim et al. (2011): confirme que o link abre o artigo certo.\n")
cat("5. Depois: Render no index.qmd (e no about.qmd), confira as figuras, o número de países no texto\n   (o resumo diz 192) e publique de novo.\n")
