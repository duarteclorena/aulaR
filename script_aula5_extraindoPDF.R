## Atividade Aula 5 - extraindo informacoes do PDF
# Autor: Lorena Duarte Campos


library(pdftools)
library(stringr)
library(dplyr)

arquivo <- "./input/cadastro.pdf"

# ler o PDF
conteudo <- pdf_text(arquivo)

# quebrar em linhas
conteudo <- str_split(conteudo, "\\n")

# juntar todas pags
conteudo <- Reduce(c, conteudo)

# remover espaços extras
conteudo <- trimws(conteudo)

# check
conteudo

# como tem nomes diferentes, vou extrair uma pessoa por vez (nome vs. Nome)

# posição onde começa cada cadastro
pos_nome <- grep("^Nome:|^nome:", conteudo)

# adicionar posição final
pos_nome <- c(pos_nome, length(conteudo) + 1)

pos_nome

# Funcao para extrair tods as informacoes

extrair_cadastro <- function(i, conteudo, pos_nome){
  
  # pega apenas as linhas de determinada pessoa
  pessoa <- conteudo[pos_nome[i]:(pos_nome[i+1] - 1)]
  
  # junta as linhas para facilitar
  texto <- paste(pessoa, collapse = " ")
  
 
  # NOME E APELIDO

  
  linha_nome <- pessoa[grep("^Nome:|^nome:", pessoa)][1]
  
  # tira "Nome:"
  nome_completo <- str_remove(linha_nome, regex("^nome:\\s*", ignore_case = TRUE))
  
  # apelido = (aka...)
  apelido <- str_extract(nome_completo, "(?<=\\(aka ).*(?=\\))")
  
  # nome = tudo antes de (aka
  nome <- str_remove(nome_completo, "\\s*\\(aka.*")
  nome <- trimws(nome)
  

  # DATA DE NASCIMENTO

  
  linha_data <- pessoa[
    grep("^(Data de nascimento|Dt nasc):", pessoa, ignore.case = TRUE)
  ][1]
  
  data_nascimento <- str_remove(
    linha_data,
    regex("^(Data de nascimento|Dt nasc):\\s*", ignore_case = TRUE)
  )
  

  # CEP
  
  cep <- str_extract(
    texto,
    "\\d{2,5}\\.?(\\d{3})?-\\d{3}"
  )
  
  
  # ENDEREÇO
  
  linha_endereco <- pessoa[
    grep("^Endereço:", pessoa, ignore.case = TRUE)
  ][1]
  
  endereco <- str_remove(
    linha_endereco,
    regex("^Endereço:\\s*", ignore_case = TRUE)
  )
  
  # remove CEP caso esteja na mesma linha (pedi ajuda para o chatgpt nessa etapa)
  endereco <- str_remove(
    endereco,
    regex("\\s*CEP:\\s*\\d{2,5}\\.?(\\d{3})?-\\d{3}.*",
          ignore_case = TRUE)
  )
  
  endereco <- trimws(endereco)
  

  # TELEFONE

  
  linha_tel <- pessoa[
    grep("^(Tel|Telefone):", pessoa, ignore.case = TRUE)
  ][1]
  
  telefone <- str_remove(
    linha_tel,
    regex("^(Tel|Telefone):\\s*", ignore_case = TRUE)
  )
  
  
  # CPF

  
  linha_cpf <- pessoa[
    grep("^cpf:", pessoa, ignore.case = TRUE)
  ][1]
  
  cpf <- str_remove(
    linha_cpf,
    regex("^cpf:\\s*", ignore_case = TRUE)
  )
  
  # para evitar perder zeros iniciais ou perder algum dados numericos, mantive como texto as datas, telefone e CPF, 
  # ai seria necessario uma conversao posterior.
  
  
  # OUTPUT

  
  data.frame(
    nome = nome,
    apelido = apelido,
    endereco = endereco,
    cep = cep,
    data_nascimento = data_nascimento,
    cpf = cpf,
    telefone = telefone
  )
}

# aplicar a função para todos os cadastros (pedi ajuda para o chatgpt nessa etapa)
dados <- lapply(
  1:(length(pos_nome) - 1),
  extrair_cadastro,
  conteudo = conteudo,
  pos_nome = pos_nome
)

# juntar todos os cadastros
dados <- bind_rows(dados)

# check
dados


openxlsx::write.xlsx(
  dados,
  "./outputs/resultado_cadastro_exercicio_aula5.xlsx"
)

