library(DBI)
library(RJDBC)
library(rJava)
library(readr)

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-26.0.1")

IRIS_DRIVER <- "com.intersystems.jdbc.IRISDriver"
IRIS_JAR    <- "C:/Program Files/DBeaver/intersystems-jdbc-3.10.3.jar"
IRIS_URL    <- "jdbc:IRIS://172.17.3.41:51773/TJSE"

IRIS_USER <- "kleberson.pinto"
IRIS_PASSWORD <- "tjse@!2023"

drv <- JDBC(
  driverClass = IRIS_DRIVER,
  classPath = IRIS_JAR
)

con <- dbConnect(
  drv,
  IRIS_URL,
  IRIS_USER,
  IRIS_PASSWORD
)

# =====================================================
# Ler arquivo CSV
# =====================================================

arquivo <- "C:/Users/3894/Projetos R/eSDECODER/data/mpm_xlsx/diretores_foruns_sergipe_csv.csv"

dados <- read_csv(
  arquivo,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# =====================================================
# Verifica se existe a coluna Nome
# =====================================================

if (!"Nome" %in% names(dados)) {
  stop("O arquivo não possui uma coluna chamada 'Nome'.")
}

# =====================================================
# Função para buscar matrícula
# =====================================================

buscar_matricula <- function(nome) {
  
  nome <- gsub("'", "''", nome)
  
  sql <- paste0(
    "SELECT Matricula ",
    "FROM RHCadServidor ",
    "WHERE Financeiro->CalculaFolha = 1 ",
    "AND Nome = '", nome, "'"
  )
  
  resultado <- dbGetQuery(con, sql)
  
  if (nrow(resultado) == 0) {
    return(NA_character_)
  }
  
  return(as.character(resultado$Matricula[1]))
}

# =====================================================
# Buscar matrícula para cada nome
# =====================================================

dados$Matricula <- sapply(dados$Nome, buscar_matricula)

# =====================================================
# Salvar novo arquivo
# =====================================================

write_csv(
  dados,
  "C:/Users/3894/Projetos R/eSDECODER/data/mpm_xlsx/diretores_foruns_sergipe_csv_com_matricula.csv",
  na = ""
)

# =====================================================
# Encerrar conexão
# =====================================================

dbDisconnect(con)

cat("Arquivo criado com sucesso!\n")
