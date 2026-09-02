# =====================================================
# R/auth.R
# =====================================================

library(reticulate)
library(jsonlite)

# =====================================================
# PYTHON
# =====================================================

use_python(
  "C://Program Files//Python314//python.exe",
  required = TRUE
)

ldap3 <- import("ldap3")

# # =====================================================
# # CONFIGURAÇÕES LDAP
# # =====================================================
# 
# LDAP_SERVER <- Sys.getenv(
#   "LDAP_SERVER"
# )
# 
# LDAP_PORT <- as.integer(
#   Sys.getenv("LDAP_PORT")
# )
# 
# LDAP_DOMAIN <- Sys.getenv(
#   "LDAP_DOMAIN"
# )
# 
# SEARCH_BASE <- Sys.getenv(
#   "LDAP_SEARCH_BASE"
# )
# 
# # =====================================================
# # VALIDA CONFIGURAÇÃO
# # =====================================================
# 
# validar_config_ldap <- function(){
# 
#   campos <- c(
#     LDAP_SERVER,
#     LDAP_PORT,
#     LDAP_DOMAIN,
#     SEARCH_BASE
#   )
# 
#   all(
#     !is.na(campos) &
#       campos != ""
#   )
# 
# }

# =====================================================
# CONFIGURAÇÕES LDAP E VALIDAÇÃO DINÂMICA
# =====================================================

validar_config_ldap <- function(){
  # Força a leitura do arquivo novamente para garantir que o Shiny localizou
  try(readRenviron("C:/Users/3894/Projetos Py/R2/conf/.Renviron"), silent = TRUE)
  
  # Atualiza as variáveis globais em tempo de execução
  LDAP_SERVER <<- Sys.getenv("LDAP_SERVER")
  LDAP_PORT   <<- as.integer(Sys.getenv("LDAP_PORT"))
  LDAP_DOMAIN <<- Sys.getenv("LDAP_DOMAIN")
  SEARCH_BASE <<- Sys.getenv("LDAP_SEARCH_BASE")
  
  campos <- c(LDAP_SERVER, LDAP_PORT, LDAP_DOMAIN, SEARCH_BASE)
  
  all(!is.na(campos) & campos != "")
}

# =====================================================
# AUTENTICAÇÃO ACTIVE DIRECTORY
# =====================================================

authenticate_ad <- function(
  usuario,
  senha
){

  if(!validar_config_ldap()){

    stop(
      "Configuração LDAP não encontrada."
    )

  }

  if(
    is.null(usuario) ||
    is.null(senha)
  ){

    return(NULL)

  }

  usuario <- trimws(usuario)
  senha   <- trimws(senha)

  if(
    usuario == "" ||
    senha == ""
  ){

    return(NULL)

  }

  servidor <- ldap3$Server(

    LDAP_SERVER,

    port = LDAP_PORT,

    use_ssl = TRUE,

    get_info = ldap3$NONE

  )

  usuario_ad <- paste0(
    usuario,
    "@",
    LDAP_DOMAIN
  )

  conexao <- ldap3$Connection(

    servidor,

    user = usuario_ad,

    password = senha,

    auto_bind = FALSE

  )

  autenticado <- tryCatch({

    conexao$bind()

  },

  error = function(e){

    FALSE

  })

  if(!autenticado){

    return(NULL)

  }

  resultado <- tryCatch({

    filtro <- paste0(
      "(sAMAccountName=",
      usuario,
      ")"
    )

    conexao$search(

      search_base = SEARCH_BASE,

      search_filter = filtro,

      attributes = ldap3$ALL_ATTRIBUTES

    )

    if(length(conexao$entries) == 0){

      conexao$unbind()

      return(NULL)

    }

    entry <- conexao$entries[[1]]

    atributos <- py_to_r(
      entry$entry_attributes_as_dict
    )

    conexao$unbind()

    atributos

  },

  error = function(e){

    try(
      conexao$unbind(),
      silent = TRUE
    )

    NULL

  })

  resultado

}

# =====================================================
# OBTÉM FOTO DO USUÁRIO
# =====================================================

obter_foto_usuario <- function(
  dados_usuario
){

  if(is.null(dados_usuario)){

    return(NULL)

  }

  if(
    is.null(
      dados_usuario$thumbnailPhoto
    )
  ){

    return(NULL)

  }

  tryCatch({

    bytes <- as.raw(

      unlist(
        dados_usuario$thumbnailPhoto
      )

    )

    paste0(

      "data:image/jpeg;base64,",

      jsonlite::base64_enc(
        bytes
      )

    )

  },

  error = function(e){

    NULL

  })

}

# =====================================================
# TESTE DE CONECTIVIDADE LDAP
# =====================================================

testar_ldap <- function(){

  tryCatch({

    servidor <- ldap3$Server(

      LDAP_SERVER,

      port = LDAP_PORT,

      use_ssl = TRUE,

      get_info = ldap3$NONE

    )

    TRUE

  },

  error = function(e){

    FALSE

  })

}

# =====================================================
# OBTÉM NOME COMPLETO
# =====================================================

obter_nome_usuario <- function(
  dados_usuario
){

  if(is.null(dados_usuario))
    return("")

  if(is.null(dados_usuario$displayName))
    return("")

  as.character(
    dados_usuario$displayName[[1]]
  )

}

# =====================================================
# OBTÉM EMAIL
# =====================================================

obter_email_usuario <- function(
  dados_usuario
){

  if(is.null(dados_usuario))
    return("")

  if(is.null(dados_usuario$mail))
    return("")

  as.character(
    dados_usuario$mail[[1]]
  )

}

# =====================================================
# OBTÉM LOGIN
# =====================================================

obter_login_usuario <- function(
  dados_usuario
){

  if(is.null(dados_usuario))
    return("")

  if(
    is.null(
      dados_usuario$sAMAccountName
    )
  )
    return("")

  as.character(
    dados_usuario$sAMAccountName[[1]]
  )

}

# =====================================================
# OBTÉM DEPARTAMENTO
# =====================================================

obter_departamento_usuario <- function(
  dados_usuario
){

  if(is.null(dados_usuario))
    return("")

  if(
    is.null(
      dados_usuario$department
    )
  )
    return("")

  as.character(
    dados_usuario$department[[1]]
  )

}