# Atividade 6 - Aula web scraping
# Autor: Lorena Duarte Campos / Out_2026

library(tidyverse)
library(chromote)
library(rvest)


#-----------------------------------------------------
# Oscar Winning Films
#-----------------------------------------------------

site <- "https://www.scrapethissite.com/pages/ajax-javascript/"


# Cria uma pag no Chrome
b <- ChromoteSession$new()

# Navega para a pag
b$Page$navigate(site)

# Espera a pag carregar
Sys.sleep(5)


#-----------------------------------------------------
# Explorando a pag
#-----------------------------------------------------

# Captura o HTML da pag
html <- b$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

# Converte o HTML em um documento para o rvest
page <- read_html(html)

# Verifica os links dos anos
page %>%
  html_elements("a.year-link")

# Extrai os anos pela ID
anos <- page %>%
  html_elements("a.year-link") %>%
  html_attr("id")

anos


#-----------------------------------------------------
# Testando manualmente
#-----------------------------------------------------

# Clica no ano de 2015
b$Runtime$evaluate(
  "document.querySelector('[id=\"2015\"]').click();"
)

Sys.sleep(2)

# Captura o HTML atualizado
html_2015 <- b$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

# Converte o HTML atualizado
page_2015 <- read_html(html_2015)

# Verifica se a tabela foi carregada
page_2015 %>%
  html_elements("table")

# Extrai a tabela de 2015
tabela_2015 <- page_2015 %>%
  html_element("table") %>%
  html_table()

tabela_2015


#-----------------------------------------------------
# Extraindo todos os anos
#-----------------------------------------------------

dados_oscar <- map(anos, function(ano) {
  
  message("Baixando dados de: ", ano)
  
  # Cria o comando para clicar no ano
  js_code <- paste0(
    "document.querySelector('[id=\"", ano, "\"]').click();"
  )
  
  # Clica no ano
  b$Runtime$evaluate(js_code)
  
  Sys.sleep(2)
  
  # Captura o HTML atualizado
  html_pagina <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  # Extrai a tabela e adiciona o ano
  read_html(html_pagina) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(Year = ano)
})


#-----------------------------------------------------
# Tabela final
#-----------------------------------------------------

# Junta as tabelas de todos os anos
oscar_df <- dados_oscar %>%
  list_rbind()

# Resultado
oscar_df
