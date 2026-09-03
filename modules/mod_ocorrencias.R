# =====================================================
# modules/mod_ocorrencias.R
# =====================================================

library(shiny)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(DT)

# =====================================================
# CONFIGURAÇÃO
# =====================================================

CAMINHO_OCORRENCIAS <- Sys.getenv("PASTA_OCORRENCIAS")

# =====================================================
# LEITURA E TRATAMENTO
# =====================================================

ler_tratar_csv <- function(arquivo) {
  
  df <- read_csv(
    
    arquivo,
    
    show_col_types =
      FALSE
    
  )
  
  arquivo_identificado <-
    FALSE
  
  arquivo_lc <-
    
    tolower(
      
      basename(
        arquivo
      )
      
    )
  
  if (
    
    str_detect(
      arquivo_lc,
      "inconsistentes"
    )
    
  ) {
    
    arquivo_identificado <-
      TRUE
    
    df <- df %>%
      
      mutate(
        
        `ID Evento` =
          `ID`,
        
        `Código Evento` =
          `Cod. Evento`,
        
        `Ocorrência(s)` =
          `Inconsistência(s)`,
        
        `Situação` =
          `Situação`,
        
        `Período` =
          NA_character_
        
      )
    
  } else if (
    
    str_detect(
      arquivo_lc,
      "rejeitados"
    )
    
  ) {
    
    arquivo_identificado <-
      TRUE
    
    df <- df %>%
      
      mutate(
        
        `ID Evento` =
          `ID Evento Fila`,
        
        `Código Evento` =
          `Código Evento`,
        
        `Ocorrência(s)` =
          `Ocorrência`,
        
        `Situação` =
          NA_character_,
        
        `Período` =
          `Período`
        
      )
    
  }
  
  if (
    
    !arquivo_identificado
    
  ) {
    
    warning(
      
      paste(
        
        "Arquivo não identificado:",
        arquivo
        
      )
      
    )
    
    return(
      data.frame()
    )
    
  }
  
  df %>%
    
    mutate(
      
      Matricula = str_extract(
        
        `Detalhe`,
        
        "^[^ ]+"
        
      ),
      
      Nome = str_replace(
        
        `Detalhe`,
        
        "^[^ ]+ [^ ]+\\s*",
        
        ""
        
      ),
      
      `Código Evento` =
        as.character(
          `Código Evento`
        )
      
    ) %>%
    
    select(
      
      `ID Evento`,
      
      `Código Evento`,
      
      Matricula,
      
      Nome,
      
      `Ocorrência(s)`,
      
      `Situação`,
      
      `Período`
      
    )
  
}

# =====================================================
# CARREGAMENTO
# =====================================================

carregar_ocorrencias <- function() {
  
  arquivos <- list.files(
    
    path =
      CAMINHO_OCORRENCIAS,
    
    pattern =
      "^relat_.*\\.csv$",
    
    full.names =
      TRUE
    
  )
  
  if (
    
    length(arquivos) == 0
    
  ) {
    
    warning(
      
      "Nenhum arquivo encontrado em: ",
      
      CAMINHO_OCORRENCIAS
      
    )
    
    return(
      data.frame()
    )
    
  }
  
  map_dfr(
    
    arquivos,
    
    ler_tratar_csv
    
  )
  
}

# =====================================================
# UI
# =====================================================

mod_ocorrencias_ui <- function(id) {
  ns <- NS(id)
  fluidPage(titlePanel("Consolidação - Inconsistencias"),
            sidebarLayout(
            sidebarPanel(
            actionButton(ns("atualizar"),"Atualizar Dados",
          
          class =
            "btn btn-secondary"
          
        ),
        
        br(),
        br(),
        
        selectInput(
          
          ns("codigo"),
          
          "Código Evento",
          
          choices =
            c("Todos")
          
        ),
        
        textInput(
          
          ns("matricula"),
          
          "Matrícula"
          
        ),
        
        textInput(
          
          ns("nome"),
          
          "Nome"
          
        ),
        
        textInput(
          
          ns("ocorrencia"),
          
          "Ocorrência"
          
        )
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel(
            
            "Tabela",
            
            DTOutput(
              ns("tabela")
            )
            
          ),
          
          tabPanel(
            
            "Gráfico",
            
            plotOutput(
              ns("grafico")
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
}

# =====================================================
# SERVER
# =====================================================

mod_ocorrencias_server <- function(
    
  id,
  
  ativo =
    reactive(TRUE)
  
) {
  
  moduleServer(
    
    id,
    
    function(
    
      input,
      
      output,
      
      session
      
    ) {
      
      # ==============================================
      # DADOS
      # ==============================================
      
      dados <- reactiveVal(
        
        carregar_ocorrencias()
        
      )
      
      # ==============================================
      # ATUALIZA O COMBO
      # ==============================================
      
      atualizar_combo_codigo <- function() {
        
        df <- dados()
        
        if (
          
          is.null(df) ||
          
          nrow(df) == 0 ||
          
          !"Código Evento" %in%
          names(df)
          
        ) {
          
          updateSelectInput(
            
            session,
            
            "codigo",
            
            choices =
              "Todos",
            
            selected =
              "Todos"
            
          )
          
          return()
          
        }
        
        codigos <- sort(
          
          unique(
            
            df[[
              "Código Evento"
            ]]
            
          )
          
        )
        
        codigos <-
          
          codigos[
            
            !is.na(codigos) &
              
              codigos != ""
            
          ]
        
        updateSelectInput(
          
          session,
          
          "codigo",
          
          choices = c(
            
            "Todos",
            
            codigos
            
          ),
          
          selected =
            "Todos"
          
        )
        
      }
      
      # ==============================================
      # PRIMEIRA ATIVAÇÃO DA ABA
      # ==============================================
      
      observeEvent(
        
        ativo(),
        
        {
          
          if (
            
            isTRUE(
              ativo()
            )
            
          ) {
            
            atualizar_combo_codigo()
            
          }
          
        },
        
        ignoreInit =
          FALSE
        
      )
      
      # ==============================================
      # ATUALIZAÇÃO MANUAL
      # ==============================================
      
      observeEvent(
        
        input$atualizar,
        
        {
          
          dados(
            
            carregar_ocorrencias()
            
          )
          
          atualizar_combo_codigo()
          
          showNotification(
            
            "Dados atualizados.",
            
            type =
              "message"
            
          )
          
        },
        
        ignoreInit =
          TRUE
        
      )
      
      # ==============================================
      # FILTROS
      # ==============================================
      
      dados_filtrados <- reactive({
        
        req(
          dados()
        )
        
        df <- dados()
        
        if (
          
          !is.null(
            input$matricula
          ) &&
          
          input$matricula != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  Matricula
                ),
                
                str_to_upper(
                  input$matricula
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$nome
          ) &&
          
          input$nome != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  Nome
                ),
                
                str_to_upper(
                  input$nome
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$ocorrencia
          ) &&
          
          input$ocorrencia != ""
          
        ) {
          
          df <- df %>%
            
            filter(
              
              str_detect(
                
                str_to_upper(
                  `Ocorrência(s)`
                ),
                
                str_to_upper(
                  input$ocorrencia
                )
                
              )
              
            )
          
        }
        
        if (
          
          !is.null(
            input$codigo
          ) &&
          
          input$codigo != "Todos"
          
        ) {
          
          df <- df %>%
            
            filter(
              
              `Código Evento` ==
                input$codigo
              
            )
          
        }
        
        df
        
      })
      
      # ==============================================
      # TABELA
      # ==============================================
      
      output$tabela <- renderDT({
        
        datatable(
          
          dados_filtrados(),
          
          filter =
            "top",
          
          rownames =
            FALSE,
          
          options =
            list(
              
              pageLength =
                20,
              
              scrollX =
                TRUE,
              
              autoWidth =
                TRUE
              
            )
          
        )
        
      })
      
      # ==============================================
      # GRÁFICO
      # ==============================================
      
      output$grafico <- renderPlot({
        
        df <-
          dados_filtrados()
        
        req(
          nrow(df) > 0
        )
        
        df %>%
          
          count(
            
            `Código Evento`
            
          ) %>%
          
          ggplot(
            
            aes(
              
              x =
                as.factor(
                  `Código Evento`
                ),
              
              y =
                n
              
            )
            
          ) +
          
          geom_col(
            
            fill =
              "#2C7FB8"
            
          ) +
          
          geom_text(
            
            aes(
              label =
                n
            ),
            
            vjust =
              -0.3
            
          ) +
          
          labs(
            
            title =
              "Quantidade por Código Evento",
            
            x =
              "Código Evento",
            
            y =
              "Quantidade"
            
          ) +
          
          theme_minimal()
        
      })
      
    }
    
  )
  
}