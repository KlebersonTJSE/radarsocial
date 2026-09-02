# =====================================================
# modules/mod_rejeitados.R
# =====================================================

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(DT)
library(ggplot2)

# =====================================================
# CONFIGURAÇÃO
# =====================================================

CAMINHO_REJEITADOS <- Sys.getenv("PASTA_REJEITADOS")

if (nzchar(CAMINHO_REJEITADOS) && !dir.exists(CAMINHO_REJEITADOS)) {
  dir.create(CAMINHO_REJEITADOS, recursive = TRUE, showWarnings = FALSE)
}

# =====================================================
# EXTRAI PERÍODO
# =====================================================

extrair_periodo_rejeitado <- function(nome_arquivo) {
  base <- basename(nome_arquivo)
  achado <- str_extract(base, "\\d{6}")
  achado
}

# =====================================================
# EXTRAI MATRÍCULA E NOME
# =====================================================

extrair_matricula <- function(texto){
  str_extract(
    texto,
    "\\d+"
  )
}

extrair_nome <- function(texto){
  texto %>%
    str_remove("^\\d+") %>%
    str_remove("^\\s*[-:]?\\s*") %>%
    str_trim()
}

processar_rejeitado <- function(arquivo){
  df <- read_csv(
    arquivo,
    col_types = cols(.default = "c")
  )
  df %>%
    mutate(
      Matricula =
        extrair_matricula(
          Detalhe
        ),
      Nome =
        extrair_nome(
          Detalhe
        ),
      Periodo = formatar_periodo(`Período`)
    ) %>%
    transmute(
      `ID Evento` =
        `ID Evento Fila`,
      `Código Evento` =
        `Código Evento`,
      Periodo,
      Matricula,
      Nome,
      `Ocorrência(s)` =
        Ocorrência
    )
}

# =====================================================
# UI
# =====================================================

mod_rejeitados_ui <- function(id) {
  ns <- NS(id)
  fluidPage(titlePanel("Eventos Rejeitados"),
            sidebarLayout(
              sidebarPanel(
                actionButton(ns("atualizar"), "Atualizar Dados"),
                br(),
                br(),
                tags$hr(),
                h4("Enviar Arquivos Rejeitados"),
                actionButton(
                  ns("apagar_pasta"),
                  "Apagar Arquivos da Pasta",
                  icon = icon("trash"),
                  class = "btn-danger"
                ),
                br(),
                br(),
                fileInput(
                  ns("upload_arquivos"),
                  "Selecionar arquivo(s) CSV",
                  multiple = TRUE,
                  accept = c(".csv", "text/csv")
                ),
                actionButton(
                  ns("enviar_arquivos"),
                  "Enviar para Pasta",
                  icon = icon("upload"),
                  class = "btn-primary"
                ),
                tags$hr(),
                selectInput(ns("periodo"), "Período", choices = c("Todos")),
                selectInput(ns("codigo_evento"), "Código Evento", choices = c("Todos")),
                textInput(ns("matricula"), "Matrícula"),
                textInput(ns("nome"), "Nome"),
                textInput(ns("ocorrencia"), "Ocorrência(s)")
              ),
              mainPanel(tabsetPanel(
                tabPanel("Tabela", DTOutput(ns("tabela"))),
                tabPanel("Gráfico", plotOutput(ns("grafico")))
              ))
            ))
}

# =====================================================
# SERVER
# =====================================================

mod_rejeitados_server <- function(id, ativo = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dados <- reactiveVal(carregar_rejeitados())
    
    # ----------------------------------------
    # COMBO Periodo — reage a dados() E a ativo()
    # ----------------------------------------
    
    observeEvent(list(dados(), ativo()), {
      req(ativo())
      req(dados())
      df <- dados()
      
      # -----------------------------------------
      # PRESERVA VALORES ATUAIS
      #
      # Evita que os combos voltem para "Todos"
      # toda vez que o usuário volta para esta aba
      # (ativo() vira TRUE de novo) sem que os dados
      # tenham mudado de fato.
      # -----------------------------------------
      
      periodo_atual <- input$periodo
      codigo_atual <- input$codigo_evento
      
      periodos <- if (nrow(df) > 0 &&
                      "Periodo" %in% names(df)) {
        sort(unique(df$Periodo))
      } else {
        character(0)
      }
      codigos_evento <- if (nrow(df) > 0 &&
                            "Código Evento" %in% names(df)) {
        sort(unique(df$`Código Evento`))
      } else {
        character(0)
      }
      
      periodo_selecionado <- if (!is.null(periodo_atual) &&
                                 periodo_atual %in% periodos) {
        periodo_atual
      } else {
        "Todos"
      }
      
      codigo_selecionado <- if (!is.null(codigo_atual) &&
                                codigo_atual %in% codigos_evento) {
        codigo_atual
      } else {
        "Todos"
      }
      
      updateSelectInput(session,
                        "periodo",
                        choices = c("Todos", periodos),
                        selected = periodo_selecionado)
      updateSelectInput(
        session,
        "codigo_evento",
        choices = c("Todos", codigos_evento),
        selected = codigo_selecionado
      )
    }, ignoreInit = FALSE)
    observeEvent(input$atualizar, {
      dados(carregar_rejeitados())
      showNotification("Dados atualizados", type = "message")
    })
    
    # ----------------------------------------
    # APAGAR ARQUIVOS DA PASTA (BOTÃO INDEPENDENTE)
    # ----------------------------------------
    
    apagar_pasta_rejeitados <- function() {
      if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
        showNotification(
          "PASTA_REJEITADOS não está configurada ou não existe.",
          type = "error"
        )
        return(invisible(NULL))
      }
      
      arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
      
      if (length(arquivos_atuais) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return(invisible(NULL))
      }
      
      resultado <- tryCatch({
        removidos <- file.remove(arquivos_atuais)
        list(ok = TRUE, removidos = removidos)
      }, error = function(e) {
        list(ok = FALSE, erro = conditionMessage(e))
      })
      
      if (!resultado$ok) {
        showNotification(
          sprintf("Erro ao apagar arquivos: %s", resultado$erro),
          type = "error"
        )
        return(invisible(NULL))
      }
      
      removidos <- resultado$removidos
      if (all(removidos)) {
        showNotification(
          sprintf("%d arquivo(s) apagado(s) da pasta.", length(arquivos_atuais)),
          type = "message"
        )
      } else {
        showNotification(
          sprintf(
            "%d de %d arquivo(s) não puderam ser apagados (verifique se estão abertos em outro programa).",
            sum(!removidos),
            length(removidos)
          ),
          type = "error"
        )
      }
      
      tryCatch({
        dados(carregar_rejeitados())
      }, error = function(e) {
        showNotification(
          sprintf("Arquivos apagados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
          type = "error"
        )
      })
    }
    
    observeEvent(input$apagar_pasta, {
      if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
        showNotification(
          "PASTA_REJEITADOS não está configurada ou não existe.",
          type = "error"
        )
        return()
      }
      
      arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
      
      if (length(arquivos_existentes) == 0) {
        showNotification("A pasta já está vazia.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Confirmar exclusão",
        sprintf(
          "Tem certeza que deseja apagar os %d arquivo(s) da pasta de rejeitados? Esta ação não pode ser desfeita.",
          length(arquivos_existentes)
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirmar_apagar_pasta"), "Apagar", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirmar_apagar_pasta, {
      removeModal()
      apagar_pasta_rejeitados()
    })
    
    # ----------------------------------------
    # UPLOAD DE ARQUIVOS PARA PASTA_REJEITADOS
    # ----------------------------------------
    
    # Copia os arquivos enviados para CAMINHO_REJEITADOS,
    # apagando os existentes antes se `apagar_existentes = TRUE`.
    processar_upload <- function(arquivos_upload, apagar_existentes = FALSE) {
      req(arquivos_upload)
      
      if (apagar_existentes) {
        arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
        if (length(arquivos_atuais) > 0) {
          file.remove(arquivos_atuais)
        }
      }
      
      destinos <- file.path(CAMINHO_REJEITADOS, arquivos_upload$name)
      copiados <- file.copy(arquivos_upload$datapath, destinos, overwrite = TRUE)
      
      if (all(copiados)) {
        showNotification(
          sprintf("%d arquivo(s) enviado(s) com sucesso.", nrow(arquivos_upload)),
          type = "message"
        )
      } else {
        showNotification(
          sprintf(
            "%d de %d arquivo(s) não puderam ser copiados.",
            sum(!copiados),
            length(copiados)
          ),
          type = "error"
        )
      }
      
      dados(carregar_rejeitados())
    }
    
    observeEvent(input$enviar_arquivos, {
      req(input$upload_arquivos)
      
      arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
      
      if (length(arquivos_existentes) > 0) {
        showModal(modalDialog(
          title = "Arquivos existentes na pasta",
          sprintf(
            "A pasta de rejeitados já contém %d arquivo(s). Deseja apagar os arquivos existentes antes de enviar os novos, ou manter os dois conjuntos?",
            length(arquivos_existentes)
          ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("manter_existentes"), "Manter Existentes"),
            actionButton(ns("apagar_existentes"), "Apagar e Enviar", class = "btn-danger")
          )
        ))
      } else {
        processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
      }
    })
    
    observeEvent(input$apagar_existentes, {
      removeModal()
      processar_upload(input$upload_arquivos, apagar_existentes = TRUE)
    })
    
    observeEvent(input$manter_existentes, {
      removeModal()
      processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
    })
    
    filtrado <- reactive({
      req(dados())
      df <- dados()
      if (!is.null(input$periodo) &&
          input$periodo != "Todos") {
        df <- df %>%
          filter(Periodo ==
                   input$periodo)
      }
      if (!is.null(input$codigo_evento) &&
          input$codigo_evento != "Todos") {
        df <- df %>%
          filter(`Código Evento` ==
                   input$codigo_evento)
      }
      if (input$matricula != "") {
        df <- df %>%
          filter(str_detect(
            str_to_upper(Matricula),
            str_to_upper(input$matricula)
          ))
      }
      if (input$nome != "") {
        df <- df %>%
          filter(str_detect(str_to_upper(Nome), str_to_upper(input$nome)))
      }
      if (input$ocorrencia != "") {
        df <- df %>%
          filter(str_detect(
            str_to_upper(`Ocorrência(s)`),
            str_to_upper(input$ocorrencia)
          ))
      }
      df
    })
    output$tabela <- renderDT({
      datatable(
        filtrado(),
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })
    output$grafico <- renderPlot({
      dados_grafico <- filtrado() %>%
        count(`Código Evento`, name = "Quantidade") %>%
        arrange(desc(Quantidade))
      ggplot(dados_grafico, aes(x = reorder(`Código Evento`, Quantidade), y = Quantidade)) +
        geom_col(fill = "#E74C3C") +
        geom_text(aes(label = Quantidade), hjust = -0.2) +
        coord_flip() +
        labs(title =
               "Quantidade de Rejeições por Código de Evento", x =
               "Código Evento", y =
               "Quantidade") +
        theme_minimal()
    })
  })
}
# # =====================================================
# # modules/mod_rejeitados.R
# # =====================================================
# 
# library(shiny)
# library(dplyr)
# library(readr)
# library(stringr)
# library(purrr)
# library(DT)
# library(ggplot2)
# 
# # =====================================================
# # CONFIGURAÇÃO
# # =====================================================
# 
# CAMINHO_REJEITADOS <- Sys.getenv("PASTA_REJEITADOS")
# 
# if (nzchar(CAMINHO_REJEITADOS) && !dir.exists(CAMINHO_REJEITADOS)) {
#   dir.create(CAMINHO_REJEITADOS, recursive = TRUE, showWarnings = FALSE)
# }
# 
# # =====================================================
# # EXTRAI PERÍODO
# # =====================================================
# 
# extrair_periodo_rejeitado <- function(nome_arquivo) {
#   base <- basename(nome_arquivo)
#   achado <- str_extract(base, "\\d{6}")
#   achado
# }
# 
# # =====================================================
# # EXTRAI MATRÍCULA E NOME
# # =====================================================
# 
# extrair_matricula <- function(texto){
#   str_extract(
#     texto,
#     "\\d+"
#   )
# }
# 
# extrair_nome <- function(texto){
#   texto %>%
#     str_remove("^\\d+") %>%
#     str_remove("^\\s*[-:]?\\s*") %>%
#     str_trim()
# }
# 
# processar_rejeitado <- function(arquivo){
#   df <- read_csv(
#     arquivo,
#     col_types = cols(.default = "c")
#   )
#   df %>%
#     mutate(
#       Matricula =
#         extrair_matricula(
#           Detalhe
#         ),
#       Nome =
#         extrair_nome(
#           Detalhe
#         ),
#       Periodo = formatar_periodo(`Período`)
#     ) %>%
#     transmute(
#       `ID Evento` =
#         `ID Evento Fila`,
#       `Código Evento` =
#         `Código Evento`,
#       Periodo,
#       Matricula,
#       Nome,
#       `Ocorrência(s)` =
#         Ocorrência
#     )
# }
# 
# # =====================================================
# # UI
# # =====================================================
# 
# mod_rejeitados_ui <- function(id) {
#   ns <- NS(id)
#   fluidPage(titlePanel("Eventos Rejeitados"),
#             sidebarLayout(
#               sidebarPanel(
#                 actionButton(ns("atualizar"), "Atualizar Dados"),
#                 br(),
#                 br(),
#                 tags$hr(),
#                 h4("Enviar Arquivos Rejeitados"),
#                 actionButton(
#                   ns("apagar_pasta"),
#                   "Apagar Arquivos da Pasta",
#                   icon = icon("trash"),
#                   class = "btn-danger"
#                 ),
#                 br(),
#                 br(),
#                 fileInput(
#                   ns("upload_arquivos"),
#                   "Selecionar arquivo(s) CSV",
#                   multiple = TRUE,
#                   accept = c(".csv", "text/csv")
#                 ),
#                 actionButton(
#                   ns("enviar_arquivos"),
#                   "Enviar para Pasta",
#                   icon = icon("upload"),
#                   class = "btn-primary"
#                 ),
#                 tags$hr(),
#                 selectInput(ns("periodo"), "Período", choices = c("Todos")),
#                 selectInput(ns("codigo_evento"), "Código Evento", choices = c("Todos")),
#                 textInput(ns("matricula"), "Matrícula"),
#                 textInput(ns("nome"), "Nome"),
#                 textInput(ns("ocorrencia"), "Ocorrência(s)")
#               ),
#               mainPanel(tabsetPanel(
#                 tabPanel("Tabela", DTOutput(ns("tabela"))),
#                 tabPanel("Gráfico", plotOutput(ns("grafico")))
#               ))
#             ))
# }
# 
# # =====================================================
# # SERVER
# # =====================================================
# 
# mod_rejeitados_server <- function(id, ativo = reactive(TRUE)) {
#   moduleServer(id, function(input, output, session) {
#     dados <- reactiveVal(carregar_rejeitados())
#     
#     # ----------------------------------------
#     # COMBO Periodo — reage a dados() E a ativo()
#     # ----------------------------------------
#     
#     observeEvent(list(dados(), ativo()), {
#       req(ativo())
#       req(dados())
#       df <- dados()
#       
#       # -----------------------------------------
#       # PRESERVA VALORES ATUAIS
#       #
#       # Evita que os combos voltem para "Todos"
#       # toda vez que o usuário volta para esta aba
#       # (ativo() vira TRUE de novo) sem que os dados
#       # tenham mudado de fato.
#       # -----------------------------------------
#       
#       periodo_atual <- input$periodo
#       codigo_atual <- input$codigo_evento
#       
#       periodos <- if (nrow(df) > 0 &&
#                       "Periodo" %in% names(df)) {
#         sort(unique(df$Periodo))
#       } else {
#         character(0)
#       }
#       codigos_evento <- if (nrow(df) > 0 &&
#                             "Código Evento" %in% names(df)) {
#         sort(unique(df$`Código Evento`))
#       } else {
#         character(0)
#       }
#       
#       periodo_selecionado <- if (!is.null(periodo_atual) &&
#                                  periodo_atual %in% periodos) {
#         periodo_atual
#       } else {
#         "Todos"
#       }
#       
#       codigo_selecionado <- if (!is.null(codigo_atual) &&
#                                 codigo_atual %in% codigos_evento) {
#         codigo_atual
#       } else {
#         "Todos"
#       }
#       
#       updateSelectInput(session,
#                         "periodo",
#                         choices = c("Todos", periodos),
#                         selected = periodo_selecionado)
#       updateSelectInput(
#         session,
#         "codigo_evento",
#         choices = c("Todos", codigos_evento),
#         selected = codigo_selecionado
#       )
#     }, ignoreInit = FALSE)
#     observeEvent(input$atualizar, {
#       dados(carregar_rejeitados())
#       showNotification("Dados atualizados", type = "message")
#     })
#     
#     # ----------------------------------------
#     # APAGAR ARQUIVOS DA PASTA (BOTÃO INDEPENDENTE)
#     # ----------------------------------------
#     
#     apagar_pasta_rejeitados <- function() {
#       if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
#         showNotification(
#           "PASTA_REJEITADOS não está configurada ou não existe.",
#           type = "error"
#         )
#         return(invisible(NULL))
#       }
#       
#       arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
#       
#       if (length(arquivos_atuais) == 0) {
#         showNotification("A pasta já está vazia.", type = "warning")
#         return(invisible(NULL))
#       }
#       
#       resultado <- tryCatch({
#         removidos <- file.remove(arquivos_atuais)
#         list(ok = TRUE, removidos = removidos)
#       }, error = function(e) {
#         list(ok = FALSE, erro = conditionMessage(e))
#       })
#       
#       if (!resultado$ok) {
#         showNotification(
#           sprintf("Erro ao apagar arquivos: %s", resultado$erro),
#           type = "error"
#         )
#         return(invisible(NULL))
#       }
#       
#       removidos <- resultado$removidos
#       if (all(removidos)) {
#         showNotification(
#           sprintf("%d arquivo(s) apagado(s) da pasta.", length(arquivos_atuais)),
#           type = "message"
#         )
#       } else {
#         showNotification(
#           sprintf(
#             "%d de %d arquivo(s) não puderam ser apagados (verifique se estão abertos em outro programa).",
#             sum(!removidos),
#             length(removidos)
#           ),
#           type = "error"
#         )
#       }
#       
#       tryCatch({
#         dados(carregar_rejeitados())
#       }, error = function(e) {
#         showNotification(
#           sprintf("Arquivos apagados, mas houve erro ao recarregar a tabela: %s", conditionMessage(e)),
#           type = "error"
#         )
#       })
#     }
#     
#     observeEvent(input$apagar_pasta, {
#       if (!nzchar(CAMINHO_REJEITADOS) || !dir.exists(CAMINHO_REJEITADOS)) {
#         showNotification(
#           "PASTA_REJEITADOS não está configurada ou não existe.",
#           type = "error"
#         )
#         return()
#       }
#       
#       arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
#       
#       if (length(arquivos_existentes) == 0) {
#         showNotification("A pasta já está vazia.", type = "warning")
#         return()
#       }
#       
#       showModal(modalDialog(
#         title = "Confirmar exclusão",
#         sprintf(
#           "Tem certeza que deseja apagar os %d arquivo(s) da pasta de rejeitados? Esta ação não pode ser desfeita.",
#           length(arquivos_existentes)
#         ),
#         footer = tagList(
#           modalButton("Cancelar"),
#           actionButton(ns("confirmar_apagar_pasta"), "Apagar", class = "btn-danger")
#         )
#       ))
#     })
#     
#     observeEvent(input$confirmar_apagar_pasta, {
#       removeModal()
#       apagar_pasta_rejeitados()
#     })
#     
#     # ----------------------------------------
#     # UPLOAD DE ARQUIVOS PARA PASTA_REJEITADOS
#     # ----------------------------------------
#     
#     # Copia os arquivos enviados para CAMINHO_REJEITADOS,
#     # apagando os existentes antes se `apagar_existentes = TRUE`.
#     processar_upload <- function(arquivos_upload, apagar_existentes = FALSE) {
#       req(arquivos_upload)
#       
#       if (apagar_existentes) {
#         arquivos_atuais <- list.files(CAMINHO_REJEITADOS, full.names = TRUE)
#         if (length(arquivos_atuais) > 0) {
#           file.remove(arquivos_atuais)
#         }
#       }
#       
#       destinos <- file.path(CAMINHO_REJEITADOS, arquivos_upload$name)
#       copiados <- file.copy(arquivos_upload$datapath, destinos, overwrite = TRUE)
#       
#       if (all(copiados)) {
#         showNotification(
#           sprintf("%d arquivo(s) enviado(s) com sucesso.", nrow(arquivos_upload)),
#           type = "message"
#         )
#       } else {
#         showNotification(
#           sprintf(
#             "%d de %d arquivo(s) não puderam ser copiados.",
#             sum(!copiados),
#             length(copiados)
#           ),
#           type = "error"
#         )
#       }
#       
#       dados(carregar_rejeitados())
#     }
#     
#     observeEvent(input$enviar_arquivos, {
#       req(input$upload_arquivos)
#       
#       arquivos_existentes <- list.files(CAMINHO_REJEITADOS)
#       
#       if (length(arquivos_existentes) > 0) {
#         showModal(modalDialog(
#           title = "Arquivos existentes na pasta",
#           sprintf(
#             "A pasta de rejeitados já contém %d arquivo(s). Deseja apagar os arquivos existentes antes de enviar os novos, ou manter os dois conjuntos?",
#             length(arquivos_existentes)
#           ),
#           footer = tagList(
#             modalButton("Cancelar"),
#             actionButton(ns("manter_existentes"), "Manter Existentes"),
#             actionButton(ns("apagar_existentes"), "Apagar e Enviar", class = "btn-danger")
#           )
#         ))
#       } else {
#         processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
#       }
#     })
#     
#     observeEvent(input$apagar_existentes, {
#       removeModal()
#       processar_upload(input$upload_arquivos, apagar_existentes = TRUE)
#     })
#     
#     observeEvent(input$manter_existentes, {
#       removeModal()
#       processar_upload(input$upload_arquivos, apagar_existentes = FALSE)
#     })
#     
#     filtrado <- reactive({
#       req(dados())
#       df <- dados()
#       if (!is.null(input$periodo) &&
#           input$periodo != "Todos") {
#         df <- df %>%
#           filter(Periodo ==
#                    input$periodo)
#       }
#       if (!is.null(input$codigo_evento) &&
#           input$codigo_evento != "Todos") {
#         df <- df %>%
#           filter(`Código Evento` ==
#                    input$codigo_evento)
#       }
#       if (input$matricula != "") {
#         df <- df %>%
#           filter(str_detect(
#             str_to_upper(Matricula),
#             str_to_upper(input$matricula)
#           ))
#       }
#       if (input$nome != "") {
#         df <- df %>%
#           filter(str_detect(str_to_upper(Nome), str_to_upper(input$nome)))
#       }
#       if (input$ocorrencia != "") {
#         df <- df %>%
#           filter(str_detect(
#             str_to_upper(`Ocorrência(s)`),
#             str_to_upper(input$ocorrencia)
#           ))
#       }
#       df
#     })
#     output$tabela <- renderDT({
#       datatable(
#         filtrado(),
#         filter = "top",
#         rownames = FALSE,
#         options = list(pageLength = 25, scrollX = TRUE)
#       )
#     })
#     output$grafico <- renderPlot({
#       dados_grafico <- filtrado() %>%
#         count(`Código Evento`, name = "Quantidade") %>%
#         arrange(desc(Quantidade))
#       ggplot(dados_grafico, aes(x = reorder(`Código Evento`, Quantidade), y = Quantidade)) +
#         geom_col(fill = "#E74C3C") +
#         geom_text(aes(label = Quantidade), hjust = -0.2) +
#         coord_flip() +
#         labs(title =
#                "Quantidade de Rejeições por Código de Evento", x =
#                "Código Evento", y =
#                "Quantidade") +
#         theme_minimal()
#     })
#   })
# }