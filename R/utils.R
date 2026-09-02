# R/utils.R

obter_campo <- function(lista, nome){

  valor <- lista[[nome]]

  if(is.null(valor))
    return("")

  if(length(valor) == 0)
    return("")

  paste(as.character(valor), collapse = "; ")

}

extrair_manager <- function(manager){

  if(is.null(manager))
    return("")

  if(length(manager) == 0)
    return("")

  manager <- as.character(manager)[1]

  sub("^CN=([^,]+),.*$", "\\1", manager)

}

formatar_when_created <- function(whenCreated){

  if(is.null(whenCreated))
    return("")

  valor <- as.character(whenCreated)[1]

  dt <- tryCatch({

    as.POSIXct(
      valor,
      format="%Y%m%d%H%M%S.0Z",
      tz="UTC"
    )

  }, error=function(e){

    NA

  })

  if(is.na(dt))
    return(valor)

  format(
    dt,
    "%d/%m/%Y %H:%M:%S"
  )

}

formatar_whenCreated <- function(x) {
    if (is.null(x) || is.na(x) || x == "") return("")

    format(
        as.POSIXct(
            gsub("\\.0Z$", "", x),
            format = "%Y%m%d%H%M%S",
            tz = "UTC"
        ),
        "%d/%m/%Y %H:%M:%S"
    )
}

formatar_lastLogon <- function(x) {
    if (is.null(x) || is.na(x) || x == "" || x == "0") {
        return("Nunca acessou")
    }

    format(
        as.POSIXct(
            (as.numeric(x) / 10000000) - 11644473600,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        "%d/%m/%Y %H:%M:%S"
    )
}

carregar_rejeitados <- function(){

    arquivos <- list.files(
        CAMINHO_REJEITADOS,
        pattern = "\\.csv$",
        full.names = TRUE
    )

    if(length(arquivos) == 0){
        return(data.frame())
    }

    resultados <- map(
        arquivos,
        processar_rejeitado
    )

    bind_rows(
        resultados
    ) %>%
        distinct()

}

# =====================================================
# EXTRAI MATRÍCULA
# =====================================================

extrair_matricula <- function(texto){

    sapply(texto, function(x){

        partes <- str_split(
            x,
            " - ",
            n = 2
        )[[1]]

        trimws(partes[1])

    })

}

# =====================================================
# EXTRAI NOME
# =====================================================

extrair_nome <- function(texto){

    sapply(texto, function(x){

        partes <- str_split(
            x,
            " - ",
            n = 2
        )[[1]]

        if(length(partes) >= 2){

            trimws(partes[2])

        } else {

            NA_character_

        }

    })

}

# =====================================================
# EXTRAI PERIODO
# =====================================================

formatar_periodo <- function(periodo){

    paste0(
        str_extract(periodo, "\\d{4}$"),
        str_pad(
            str_extract(periodo, "^\\d{1,2}"),
            width = 2,
            pad = "0"
        )
    )

}
