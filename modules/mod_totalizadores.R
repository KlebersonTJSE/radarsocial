# =====================================================
# modules/mod_totalizadores.R
# =====================================================

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)
library(DT)

# =====================================================
# CONFIGURAÇÃO
# =====================================================

CAMINHO_TOTALIZADORES <- Sys.getenv("PASTA_TOTALIZADORES")

# =====================================================
# EXTRAI PERÍODO
# =====================================================

extrair_periodo <- function(nome_arquivo) {
  base <- basename(nome_arquivo)
  partes <- str_split(base, "_")[[1]]
  mes <- partes[3]
  ano <- partes[4]
  mes <- str_pad(mes, width = 2, pad = "0")
  paste0(ano, mes)
}

# =====================================================
# FORMATAÇÃO MONETÁRIA
# =====================================================

formatar_moeda <- function(x) {
  ifelse(is.na(x), NA_character_, paste0(
    "R$ ",
    formatC(
      x,
      format = "f",
      digits = 2,
      big.mark = ".",
      decimal.mark = ","
    )
  ))
  
}

# =====================================================
# PROCESSA UM PERÍODO
# =====================================================

processar_periodo <- function(arq5001, arq5002, periodo) {
  # -------------------------------------------------
  # LEITURA DOS ARQUIVOS
  # -------------------------------------------------
  
  df1 <- read_csv(arq5001, col_types = cols(.default = "c"))
  
  df2 <- read_csv(arq5002, col_types = cols(.default = "c"))
  
  # -------------------------------------------------
  # CONVERSÃO DOS VALORES
  # -------------------------------------------------
  
  df1$valor <- parse_number(df1$valor, locale = locale(decimal_mark = ",", grouping_mark = "."))
  
  df2$Valor <- parse_number(df2$Valor, locale = locale(decimal_mark = ",", grouping_mark = "."))
  
  # -------------------------------------------------
  # BASE DE CÁLCULO
  # -------------------------------------------------
  
  base <- df1 %>%
    filter(str_starts(tpValor, "11")) %>%
    group_by(`Matrícula`, CPF, Nome) %>%
    summarise(Base.Calculo = sum(valor, na.rm = TRUE),
              .groups = "drop")
  
  # -------------------------------------------------
  # INSS
  # -------------------------------------------------
  
  inss <- df1 %>%
    filter(str_starts(tpValor, "21")) %>%
    group_by(`Matrícula`, CPF) %>%
    summarise(INSS = sum(valor, na.rm = TRUE), .groups = "drop")
  
  # -------------------------------------------------
  # IR
  # -------------------------------------------------
  
  ir <- df2 %>%
    filter(str_starts(tpInfoIR, "31")) %>%
    group_by(`Matrícula`, CPF) %>%
    summarise(IR = sum(Valor, na.rm = TRUE), .groups = "drop")
  
  # -------------------------------------------------
  # PSO
  # -------------------------------------------------
  
  pso <- df2 %>%
    filter(str_starts(tpInfoIR, "41")) %>%
    group_by(`Matrícula`, CPF) %>%
    summarise(PSO = sum(Valor, na.rm = TRUE), .groups = "drop")
  
  # -------------------------------------------------
  # ISS
  # -------------------------------------------------
  
  iss <- df2 %>%
    filter(str_detect(tpInfoIR, "AJUSTAR_ISS")) %>%
    group_by(`Matrícula`, CPF) %>%
    summarise(ISS = sum(Valor, na.rm = TRUE), .groups = "drop")
  
  # -------------------------------------------------
  # CADASTRO DO ARQUIVO 5001
  # -------------------------------------------------
  
  cadastro_5001 <- df1 %>%
    select(`Matrícula`, CPF, Nome) %>%
    distinct() %>%
    rename(Nome_5001 = Nome)
  
  # -------------------------------------------------
  # CADASTRO DO ARQUIVO 5002
  # -------------------------------------------------
  
  cadastro_5002 <- df2 %>%
    select(`Matrícula`, CPF, Nome) %>%
    distinct() %>%
    rename(Nome_5002 = Nome)
  
  # -------------------------------------------------
  # CONSOLIDA CADASTRO
  # -------------------------------------------------
  
  cadastro <- full_join(cadastro_5001, cadastro_5002, by = c("Matrícula", "CPF")) %>%
    
    mutate(Nome = coalesce(Nome_5001, Nome_5002)) %>%
    
    select(`Matrícula`, CPF, Nome)
  
  # -------------------------------------------------
  # CONSOLIDA RESULTADO
  # -------------------------------------------------
  
  resultado <- cadastro %>%
    full_join(base,
              by = c("Matrícula", "CPF"),
              suffix = c("", ".base")) %>%
    mutate(Nome = coalesce(Nome, Nome.base)) %>%
    select(-Nome.base) %>%
    full_join(inss, by = c("Matrícula", "CPF")) %>%
    full_join(ir, by = c("Matrícula", "CPF")) %>%
    full_join(pso, by = c("Matrícula", "CPF")) %>%
    full_join(iss, by = c("Matrícula", "CPF"))
  
  # -------------------------------------------------
  # FORMATAÇÃO FINAL
  # -------------------------------------------------
  
  resultado <- resultado %>%
    mutate(
      `Base de Calculo` = formatar_moeda(Base.Calculo),
      INSS              = formatar_moeda(INSS),
      IR                = formatar_moeda(IR),
      PSO               = formatar_moeda(PSO),
      ISS               = formatar_moeda(ISS)
    ) %>%
    
    mutate(
      Tipo_Contribuinte =
        ifelse(nchar(as.character(`Matrícula`)) == 11, "PRESTADOR", "SERVIDOR"),
      Periodo = periodo
    ) %>%
    select(
      Tipo_Contribuinte,
      Periodo,
      CPF,
      Nome,
      `Matrícula`,
      `Base de Calculo`,
      INSS,
      IR,
      PSO,
      ISS
    )
  
  resultado
  
}

# =====================================================
# CARREGAMENTO
# =====================================================

carregar_totalizadores <- function() {
  # -------------------------------------------------
  # LOCALIZA ARQUIVOS 5001
  # -------------------------------------------------
  
  arquivos_5001 <- list.files(CAMINHO_TOTALIZADORES,
                              pattern = "^5001_.*\\.csv$",
                              full.names = TRUE)
  
  # -------------------------------------------------
  # LOCALIZA ARQUIVOS 5002
  # -------------------------------------------------
  
  arquivos_5002 <- list.files(CAMINHO_TOTALIZADORES,
                              pattern = "^5002_.*\\.csv$",
                              full.names = TRUE)
  
  # -------------------------------------------------
  # VERIFICA EXISTÊNCIA
  # -------------------------------------------------
  
  if (length(arquivos_5001) == 0 ||
      length(arquivos_5002) == 0) {
    return(data.frame())
  }
  
  # -------------------------------------------------
  # IDENTIFICA PERÍODOS
  # -------------------------------------------------
  
  df_5001 <- data.frame(
    arquivo = arquivos_5001,
    periodo = sapply(arquivos_5001, extrair_periodo),
    stringsAsFactors = FALSE
  )
  
  df_5002 <- data.frame(
    arquivo = arquivos_5002,
    periodo = sapply(arquivos_5002, extrair_periodo),
    stringsAsFactors = FALSE
  )
  
  # -------------------------------------------------
  # CRIA PARES 5001 / 5002
  # -------------------------------------------------
  
  pares <- inner_join(df_5001,
                      df_5002,
                      by = "periodo",
                      suffix = c("_5001", "_5002"))
  
  # -------------------------------------------------
  # PROCESSA OS PERÍODOS
  # -------------------------------------------------
  
  resultados <- map2(
    pares$arquivo_5001,
    pares$arquivo_5002,
    ~ processar_periodo(.x, .y, extrair_periodo(.x))
  )
  
  # -------------------------------------------------
  # CONSOLIDA RESULTADOS
  # -------------------------------------------------
  
  bind_rows(resultados) %>%
    distinct()
}

# =====================================================
# UI
# =====================================================

mod_totalizadores_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Consolidado - Servidores x Prestadores"),
    
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("atualizar"), "Atualizar Dados"),
        
        br(),
        
        br(),
        
        selectInput(
          ns("tipo"),
          
          "Tipo Contribuinte",
          
          choices = c("Todos"),
          
          selected = "Todos"
          
        ),
        
        selectInput(
          ns("periodo"),
          
          "Período",
          
          choices = c("Todos"),
          
          selected = "Todos"
          
        ),
        
        textInput(ns("matricula"), "Matrícula"),
        
        textInput(ns("cpf"), "CPF"),
        
        textInput(ns("nome"), "Nome")
        
      ),
      
      mainPanel(tabsetPanel(
        tabPanel("Tabela", DTOutput(ns("tabela"))),
        
        tabPanel("Gráfico", plotOutput(ns("grafico")))
        
      ))
      
    )
    
  )
  
}

# =====================================================
# SERVER
# =====================================================

mod_totalizadores_server <- function(id, ativo = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    # =================================================
    # DADOS
    # =================================================
    
    dados <- reactiveVal(carregar_totalizadores())
    
    # =================================================
    # ATUALIZAÇÃO DOS COMBOBOX
    #
    # IMPORTANTE:
    #
    # Este observeEvent reage a dados() E a ativo().
    #
    # Precisa reagir a ativo() porque o moduleServer é
    # instanciado (e dados() é inicializado) ANTES do
    # login/UI existir no navegador. Sem depender de
    # ativo(), o updateSelectInput roda antes dos
    # selectInput existirem no DOM e é perdido -- os
    # combos nunca populam.
    #
    # A lógica abaixo preserva a seleção atual (tipo_atual
    # / periodo_atual) quando ainda é válida, então
    # alternar para esta aba não reseta os filtros já
    # escolhidos.
    # =================================================
    
    observeEvent(list(dados(), ativo()), {
      req(ativo())
      
      df <- dados()
      
      # -----------------------------------------
      # PRESERVA VALORES ATUAIS
      # -----------------------------------------
      
      tipo_atual <- input$tipo
      
      periodo_atual <- input$periodo
      
      # -----------------------------------------
      # TIPOS DISPONÍVEIS
      # -----------------------------------------
      
      tipos <- if (nrow(df) > 0 &&
                   
                   "Tipo_Contribuinte" %in%
                   
                   names(df)) {
        sort(unique(na.omit(
          as.character(df$Tipo_Contribuinte)
          
        )))
        
      } else {
        character(0)
        
      }
      
      # -----------------------------------------
      # PERÍODOS DISPONÍVEIS
      # -----------------------------------------
      
      periodos <- if (nrow(df) > 0 &&
                      
                      "Periodo" %in%
                      
                      names(df)) {
        sort(unique(na.omit(as.character(df$Periodo))))
        
      } else {
        character(0)
        
      }
      
      # -----------------------------------------
      # SELEÇÃO DO TIPO
      # -----------------------------------------
      
      tipo_selecionado <- if (!is.null(tipo_atual) &&
                              
                              tipo_atual %in%
                              
                              tipos) {
        tipo_atual
        
      } else {
        "Todos"
        
      }
      
      # -----------------------------------------
      # SELEÇÃO DO PERÍODO
      # -----------------------------------------
      
      periodo_selecionado <- if (!is.null(periodo_atual) &&
                                 
                                 periodo_atual %in%
                                 
                                 periodos) {
        periodo_atual
        
      } else {
        "Todos"
        
      }
      
      # -----------------------------------------
      # ATUALIZA TIPO
      # -----------------------------------------
      
      updateSelectInput(session,
                        
                        "tipo",
                        
                        choices = c("Todos", tipos),
                        
                        selected = tipo_selecionado)
      
      # -----------------------------------------
      # ATUALIZA PERÍODO
      # -----------------------------------------
      
      updateSelectInput(
        session,
        
        "periodo",
        
        choices = c("Todos", periodos),
        
        selected = periodo_selecionado
        
      )
      
    }, ignoreInit = FALSE)
    
    # =================================================
    # ATUALIZAÇÃO MANUAL DOS DADOS
    # =================================================
    
    observeEvent(input$atualizar, {
      dados(carregar_totalizadores())
      
      showNotification("Dados atualizados.", type = "message")
      
    })
    
    # =================================================
    # FILTROS
    # =================================================
    
    filtrado <- reactive({
      req(dados())
      
      df <- dados()
      
      # ---------------------------------------------
      # TIPO DE CONTRIBUINTE
      # ---------------------------------------------
      
      if (!is.null(input$tipo) &&
          
          length(input$tipo) > 0 &&
          
          input$tipo != "Todos") {
        df <- df %>%
          
          filter(!is.na(Tipo_Contribuinte),
                 
                 Tipo_Contribuinte ==
                   
                   input$tipo)
        
      }
      
      # ---------------------------------------------
      # PERÍODO
      # ---------------------------------------------
      
      if (!is.null(input$periodo) &&
          
          length(input$periodo) > 0 &&
          
          input$periodo != "Todos") {
        df <- df %>%
          
          filter(!is.na(Periodo), Periodo ==
                   
                   input$periodo)
        
      }
      
      # ---------------------------------------------
      # MATRÍCULA
      # ---------------------------------------------
      
      if (!is.null(input$matricula) &&
          
          nzchar(trimws(input$matricula))) {
        df <- df %>%
          
          filter(!is.na(`Matrícula`),
                 
                 str_detect(str_to_upper(as.character(
                   `Matrícula`
                   
                 )), fixed(str_to_upper(
                   trimws(input$matricula)
                   
                 ))))
        
      }
      
      # ---------------------------------------------
      # CPF
      # ---------------------------------------------
      
      if (!is.null(input$cpf) &&
          
          nzchar(trimws(input$cpf))) {
        df <- df %>%
          
          filter(!is.na(CPF), str_detect(as.character(CPF), fixed(trimws(input$cpf))))
        
      }
      
      # ---------------------------------------------
      # NOME
      # ---------------------------------------------
      
      if (!is.null(input$nome) &&
          
          nzchar(trimws(input$nome))) {
        df <- df %>%
          
          filter(!is.na(Nome), str_detect(str_to_upper(as.character(Nome)), fixed(str_to_upper(
            trimws(input$nome)
            
          ))))
        
      }
      
      df
      
    })
    
    # =================================================
    # TABELA
    # =================================================
    
    output$tabela <- renderDT({
      df <- filtrado()
      
      # ---------------------------------------------
      # RECONVERTE OS VALORES FORMATADOS (TEXTO)
      # PARA NUMÉRICO, PARA PODER SOMAR
      # ---------------------------------------------
      
      parse_valor <- function(x) {
        readr::parse_number(
          x,
          locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
        )
      }
      
      total_base <- sum(parse_valor(df$`Base de Calculo`), na.rm = TRUE)
      total_inss <- sum(parse_valor(df$INSS), na.rm = TRUE)
      total_ir   <- sum(parse_valor(df$IR), na.rm = TRUE)
      total_pso  <- sum(parse_valor(df$PSO), na.rm = TRUE)
      total_iss  <- sum(parse_valor(df$ISS), na.rm = TRUE)
      
      # ---------------------------------------------
      # MONTA O CABEÇALHO + RODAPÉ COM OS TOTAIS
      #
      # Usar tfoot (em vez de acrescentar uma linha
      # "TOTAL" aos dados) garante que o total sempre
      # apareça na última linha da tabela, mesmo com
      # paginação, ordenação por coluna ou filtros.
      # ---------------------------------------------
      
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(lapply(names(df), th))
        ),
        tfoot(
          tr(
            th("Total", colspan = 5),
            th(formatar_moeda(total_base)),
            th(formatar_moeda(total_inss)),
            th(formatar_moeda(total_ir)),
            th(formatar_moeda(total_pso)),
            th(formatar_moeda(total_iss))
          )
        )
      ))
      
      datatable(
        df,
        
        container = sketch,
        
        filter = "top",
        
        rownames = FALSE,
        
        options = list(pageLength = 20, scrollX = TRUE)
        
      )
      
    })
    
    # =================================================
    # GRÁFICO
    # =================================================
    
    output$grafico <- renderPlot({
      dados_grafico <- filtrado() %>%
        
        count(Periodo, Tipo_Contribuinte, name = "Quantidade")
      
      ggplot(dados_grafico,
             
             aes(x = Periodo, y = Quantidade, fill = Tipo_Contribuinte)) +
        
        geom_col(position = "dodge") +
        
        geom_text(aes(label = Quantidade),
                  
                  position = position_dodge(width = 0.9),
                  
                  vjust = -0.3) +
        
        labs(
          title =
            
            "Quantidade por Período e Tipo de Contribuinte",
          
          x = "Período",
          
          y = "Quantidade",
          
          fill = "Tipo"
          
        ) +
        
        theme_minimal()
      
    })
    
  })
  
}

# # =====================================================
# # modules/mod_totalizadores.R
# # =====================================================
# 
# library(shiny)
# library(dplyr)
# library(readr)
# library(stringr)
# library(purrr)
# library(ggplot2)
# library(DT)
# 
# # =====================================================
# # CONFIGURAÇÃO
# # =====================================================
# 
# CAMINHO_TOTALIZADORES <- Sys.getenv(
#   "PASTA_TOTALIZADORES",
#   "C:/Users/3894/Projetos R/eSDECODER/data/totalizadores_csv"
# )
# 
# # =====================================================
# # EXTRAI PERÍODO
# # =====================================================
# 
# extrair_periodo <- function(nome_arquivo) {
#   base <- basename(nome_arquivo)
#   partes <- str_split(base, "_")[[1]]
#   mes <- partes[3]
#   ano <- partes[4]
#   mes <- str_pad(mes, width = 2, pad = "0")
#   paste0(ano, mes)
# }
# 
# # =====================================================
# # FORMATAÇÃO MONETÁRIA
# # =====================================================
# 
# formatar_moeda <- function(x) {
#   ifelse(is.na(x), NA_character_, paste0(
#     "R$ ",
#     formatC(
#       x,
#       format = "f",
#       digits = 2,
#       big.mark = ".",
#       decimal.mark = ","
#     )
#   ))
#   
# }
# 
# # =====================================================
# # PROCESSA UM PERÍODO
# # =====================================================
# 
# processar_periodo <- function(arq5001, arq5002, periodo) {
#   # -------------------------------------------------
#   # LEITURA DOS ARQUIVOS
#   # -------------------------------------------------
#   
#   df1 <- read_csv(arq5001, col_types = cols(.default = "c"))
#   
#   df2 <- read_csv(arq5002, col_types = cols(.default = "c"))
#   
#   # -------------------------------------------------
#   # CONVERSÃO DOS VALORES
#   # -------------------------------------------------
#   
#   df1$valor <- parse_number(df1$valor, locale = locale(decimal_mark = ",", grouping_mark = "."))
#   
#   df2$Valor <- parse_number(df2$Valor, locale = locale(decimal_mark = ",", grouping_mark = "."))
#   
#   # -------------------------------------------------
#   # BASE DE CÁLCULO
#   # -------------------------------------------------
#   
#   base <- df1 %>%
#     filter(str_starts(tpValor, "11")) %>%
#     group_by(`Matrícula`, CPF, Nome) %>%
#     summarise(Base.Calculo = sum(valor, na.rm = TRUE),
#               .groups = "drop")
#   
#   # -------------------------------------------------
#   # INSS
#   # -------------------------------------------------
#   
#   inss <- df1 %>%
#     filter(str_starts(tpValor, "21")) %>%
#     group_by(`Matrícula`, CPF) %>%
#     summarise(INSS = sum(valor, na.rm = TRUE), .groups = "drop")
#   
#   # -------------------------------------------------
#   # IR
#   # -------------------------------------------------
#   
#   ir <- df2 %>%
#     filter(str_starts(tpInfoIR, "31")) %>%
#     group_by(`Matrícula`, CPF) %>%
#     summarise(IR = sum(Valor, na.rm = TRUE), .groups = "drop")
#   
#   # -------------------------------------------------
#   # PSO
#   # -------------------------------------------------
#   
#   pso <- df2 %>%
#     filter(str_starts(tpInfoIR, "41")) %>%
#     group_by(`Matrícula`, CPF) %>%
#     summarise(PSO = sum(Valor, na.rm = TRUE), .groups = "drop")
#   
#   # -------------------------------------------------
#   # ISS
#   # -------------------------------------------------
#   
#   iss <- df2 %>%
#     filter(str_detect(tpInfoIR, "AJUSTAR_ISS")) %>%
#     group_by(`Matrícula`, CPF) %>%
#     summarise(ISS = sum(Valor, na.rm = TRUE), .groups = "drop")
#   
#   # -------------------------------------------------
#   # CADASTRO DO ARQUIVO 5001
#   # -------------------------------------------------
#   
#   cadastro_5001 <- df1 %>%
#     select(`Matrícula`, CPF, Nome) %>%
#     distinct() %>%
#     rename(Nome_5001 = Nome)
#   
#   # -------------------------------------------------
#   # CADASTRO DO ARQUIVO 5002
#   # -------------------------------------------------
#   
#   cadastro_5002 <- df2 %>%
#     select(`Matrícula`, CPF, Nome) %>%
#     distinct() %>%
#     rename(Nome_5002 = Nome)
#   
#   # -------------------------------------------------
#   # CONSOLIDA CADASTRO
#   # -------------------------------------------------
#   
#   cadastro <- full_join(cadastro_5001, cadastro_5002, by = c("Matrícula", "CPF")) %>%
#     
#     mutate(Nome = coalesce(Nome_5001, Nome_5002)) %>%
#     
#     select(`Matrícula`, CPF, Nome)
#   
#   # -------------------------------------------------
#   # CONSOLIDA RESULTADO
#   # -------------------------------------------------
#   
#   resultado <- cadastro %>%
#     full_join(base,
#               by = c("Matrícula", "CPF"),
#               suffix = c("", ".base")) %>%
#     mutate(Nome = coalesce(Nome, Nome.base)) %>%
#     select(-Nome.base) %>%
#     full_join(inss, by = c("Matrícula", "CPF")) %>%
#     full_join(ir, by = c("Matrícula", "CPF")) %>%
#     full_join(pso, by = c("Matrícula", "CPF")) %>%
#     full_join(iss, by = c("Matrícula", "CPF"))
#   
#   # -------------------------------------------------
#   # FORMATAÇÃO FINAL
#   # -------------------------------------------------
#   
#   resultado <- resultado %>%
#     mutate(
#       `Base de Calculo` = formatar_moeda(Base.Calculo),
#       INSS              = formatar_moeda(INSS),
#       IR                = formatar_moeda(IR),
#       PSO               = formatar_moeda(PSO),
#       ISS               = formatar_moeda(ISS)
#     ) %>%
#     
#     mutate(
#       Tipo_Contribuinte =
#         ifelse(nchar(as.character(`Matrícula`)) == 11, "PRESTADOR", "SERVIDOR"),
#       Periodo = periodo
#     ) %>%
#     select(
#       Tipo_Contribuinte,
#       Periodo,
#       CPF,
#       Nome,
#       `Matrícula`,
#       `Base de Calculo`,
#       INSS,
#       IR,
#       PSO,
#       ISS
#     )
#   
#   resultado
#   
# }
# 
# # =====================================================
# # CARREGAMENTO
# # =====================================================
# 
# carregar_totalizadores <- function() {
#   # -------------------------------------------------
#   # LOCALIZA ARQUIVOS 5001
#   # -------------------------------------------------
#   
#   arquivos_5001 <- list.files(CAMINHO_TOTALIZADORES,
#                               pattern = "^5001_.*\\.csv$",
#                               full.names = TRUE)
#   
#   # -------------------------------------------------
#   # LOCALIZA ARQUIVOS 5002
#   # -------------------------------------------------
#   
#   arquivos_5002 <- list.files(CAMINHO_TOTALIZADORES,
#                               pattern = "^5002_.*\\.csv$",
#                               full.names = TRUE)
#   
#   # -------------------------------------------------
#   # VERIFICA EXISTÊNCIA
#   # -------------------------------------------------
#   
#   if (length(arquivos_5001) == 0 ||
#       length(arquivos_5002) == 0) {
#     return(data.frame())
#   }
#   
#   # -------------------------------------------------
#   # IDENTIFICA PERÍODOS
#   # -------------------------------------------------
#   
#   df_5001 <- data.frame(
#     arquivo = arquivos_5001,
#     periodo = sapply(arquivos_5001, extrair_periodo),
#     stringsAsFactors = FALSE
#   )
#   
#   df_5002 <- data.frame(
#     arquivo = arquivos_5002,
#     periodo = sapply(arquivos_5002, extrair_periodo),
#     stringsAsFactors = FALSE
#   )
#   
#   # -------------------------------------------------
#   # CRIA PARES 5001 / 5002
#   # -------------------------------------------------
#   
#   pares <- inner_join(df_5001,
#                       df_5002,
#                       by = "periodo",
#                       suffix = c("_5001", "_5002"))
#   
#   # -------------------------------------------------
#   # PROCESSA OS PERÍODOS
#   # -------------------------------------------------
#   
#   resultados <- map2(
#     pares$arquivo_5001,
#     pares$arquivo_5002,
#     ~ processar_periodo(.x, .y, extrair_periodo(.x))
#   )
#   
#   # -------------------------------------------------
#   # CONSOLIDA RESULTADOS
#   # -------------------------------------------------
#   
#   bind_rows(resultados) %>%
#     distinct()
# }
# 
# # =====================================================
# # UI
# # =====================================================
# 
# mod_totalizadores_ui <- function(id) {
#   ns <- NS(id)
#   
#   fluidPage(
#     titlePanel("Consolidado - Servidores x Prestadores"),
#     
#     sidebarLayout(
#       sidebarPanel(
#         actionButton(ns("atualizar"), "Atualizar Dados"),
#         
#         br(),
#         
#         br(),
#         
#         selectInput(
#           ns("tipo"),
#           
#           "Tipo Contribuinte",
#           
#           choices = c("Todos"),
#           
#           selected = "Todos"
#           
#         ),
#         
#         selectInput(
#           ns("periodo"),
#           
#           "Período",
#           
#           choices = c("Todos"),
#           
#           selected = "Todos"
#           
#         ),
#         
#         textInput(ns("matricula"), "Matrícula"),
#         
#         textInput(ns("cpf"), "CPF"),
#         
#         textInput(ns("nome"), "Nome")
#         
#       ),
#       
#       mainPanel(tabsetPanel(
#         tabPanel("Tabela", DTOutput(ns("tabela"))),
#         
#         tabPanel("Gráfico", plotOutput(ns("grafico")))
#         
#       ))
#       
#     )
#     
#   )
#   
# }
# 
# # =====================================================
# # SERVER
# # =====================================================
# 
# mod_totalizadores_server <- function(id, ativo = reactive(TRUE)) {
#   moduleServer(id, function(input, output, session) {
#     # =================================================
#     # DADOS
#     # =================================================
#     
#     dados <- reactiveVal(carregar_totalizadores())
#     
#     # =================================================
#     # ATUALIZAÇÃO DOS COMBOBOX
#     #
#     # IMPORTANTE:
#     #
#     # Este observeEvent reage a dados() E a ativo().
#     #
#     # Precisa reagir a ativo() porque o moduleServer é
#     # instanciado (e dados() é inicializado) ANTES do
#     # login/UI existir no navegador. Sem depender de
#     # ativo(), o updateSelectInput roda antes dos
#     # selectInput existirem no DOM e é perdido -- os
#     # combos nunca populam.
#     #
#     # A lógica abaixo preserva a seleção atual (tipo_atual
#     # / periodo_atual) quando ainda é válida, então
#     # alternar para esta aba não reseta os filtros já
#     # escolhidos.
#     # =================================================
#     
#     observeEvent(list(dados(), ativo()), {
#       req(ativo())
#       
#       df <- dados()
#       
#       # -----------------------------------------
#       # PRESERVA VALORES ATUAIS
#       # -----------------------------------------
#       
#       tipo_atual <- input$tipo
#       
#       periodo_atual <- input$periodo
#       
#       # -----------------------------------------
#       # TIPOS DISPONÍVEIS
#       # -----------------------------------------
#       
#       tipos <- if (nrow(df) > 0 &&
#                    
#                    "Tipo_Contribuinte" %in%
#                    
#                    names(df)) {
#         sort(unique(na.omit(
#           as.character(df$Tipo_Contribuinte)
#           
#         )))
#         
#       } else {
#         character(0)
#         
#       }
#       
#       # -----------------------------------------
#       # PERÍODOS DISPONÍVEIS
#       # -----------------------------------------
#       
#       periodos <- if (nrow(df) > 0 &&
#                       
#                       "Periodo" %in%
#                       
#                       names(df)) {
#         sort(unique(na.omit(as.character(df$Periodo))))
#         
#       } else {
#         character(0)
#         
#       }
#       
#       # -----------------------------------------
#       # SELEÇÃO DO TIPO
#       # -----------------------------------------
#       
#       tipo_selecionado <- if (!is.null(tipo_atual) &&
#                               
#                               tipo_atual %in%
#                               
#                               tipos) {
#         tipo_atual
#         
#       } else {
#         "Todos"
#         
#       }
#       
#       # -----------------------------------------
#       # SELEÇÃO DO PERÍODO
#       # -----------------------------------------
#       
#       periodo_selecionado <- if (!is.null(periodo_atual) &&
#                                  
#                                  periodo_atual %in%
#                                  
#                                  periodos) {
#         periodo_atual
#         
#       } else {
#         "Todos"
#         
#       }
#       
#       # -----------------------------------------
#       # ATUALIZA TIPO
#       # -----------------------------------------
#       
#       updateSelectInput(session,
#                         
#                         "tipo",
#                         
#                         choices = c("Todos", tipos),
#                         
#                         selected = tipo_selecionado)
#       
#       # -----------------------------------------
#       # ATUALIZA PERÍODO
#       # -----------------------------------------
#       
#       updateSelectInput(
#         session,
#         
#         "periodo",
#         
#         choices = c("Todos", periodos),
#         
#         selected = periodo_selecionado
#         
#       )
#       
#     }, ignoreInit = FALSE)
#     
#     # =================================================
#     # ATUALIZAÇÃO MANUAL DOS DADOS
#     # =================================================
#     
#     observeEvent(input$atualizar, {
#       dados(carregar_totalizadores())
#       
#       showNotification("Dados atualizados.", type = "message")
#       
#     })
#     
#     # =================================================
#     # FILTROS
#     # =================================================
#     
#     filtrado <- reactive({
#       req(dados())
#       
#       df <- dados()
#       
#       # ---------------------------------------------
#       # TIPO DE CONTRIBUINTE
#       # ---------------------------------------------
#       
#       if (!is.null(input$tipo) &&
#           
#           length(input$tipo) > 0 &&
#           
#           input$tipo != "Todos") {
#         df <- df %>%
#           
#           filter(!is.na(Tipo_Contribuinte),
#                  
#                  Tipo_Contribuinte ==
#                    
#                    input$tipo)
#         
#       }
#       
#       # ---------------------------------------------
#       # PERÍODO
#       # ---------------------------------------------
#       
#       if (!is.null(input$periodo) &&
#           
#           length(input$periodo) > 0 &&
#           
#           input$periodo != "Todos") {
#         df <- df %>%
#           
#           filter(!is.na(Periodo), Periodo ==
#                    
#                    input$periodo)
#         
#       }
#       
#       # ---------------------------------------------
#       # MATRÍCULA
#       # ---------------------------------------------
#       
#       if (!is.null(input$matricula) &&
#           
#           nzchar(trimws(input$matricula))) {
#         df <- df %>%
#           
#           filter(!is.na(`Matrícula`),
#                  
#                  str_detect(str_to_upper(as.character(
#                    `Matrícula`
#                    
#                  )), fixed(str_to_upper(
#                    trimws(input$matricula)
#                    
#                  ))))
#         
#       }
#       
#       # ---------------------------------------------
#       # CPF
#       # ---------------------------------------------
#       
#       if (!is.null(input$cpf) &&
#           
#           nzchar(trimws(input$cpf))) {
#         df <- df %>%
#           
#           filter(!is.na(CPF), str_detect(as.character(CPF), fixed(trimws(input$cpf))))
#         
#       }
#       
#       # ---------------------------------------------
#       # NOME
#       # ---------------------------------------------
#       
#       if (!is.null(input$nome) &&
#           
#           nzchar(trimws(input$nome))) {
#         df <- df %>%
#           
#           filter(!is.na(Nome), str_detect(str_to_upper(as.character(Nome)), fixed(str_to_upper(
#             trimws(input$nome)
#             
#           ))))
#         
#       }
#       
#       df
#       
#     })
#     
#     # =================================================
#     # TABELA
#     # =================================================
#     
#     output$tabela <- renderDT({
#       datatable(
#         filtrado(),
#         
#         filter = "top",
#         
#         rownames = FALSE,
#         
#         options = list(pageLength = 20, scrollX = TRUE)
#         
#       )
#       
#     })
#     
#     # =================================================
#     # GRÁFICO
#     # =================================================
#     
#     output$grafico <- renderPlot({
#       dados_grafico <- filtrado() %>%
#         
#         count(Periodo, Tipo_Contribuinte, name = "Quantidade")
#       
#       ggplot(dados_grafico,
#              
#              aes(x = Periodo, y = Quantidade, fill = Tipo_Contribuinte)) +
#         
#         geom_col(position = "dodge") +
#         
#         geom_text(aes(label = Quantidade),
#                   
#                   position = position_dodge(width = 0.9),
#                   
#                   vjust = -0.3) +
#         
#         labs(
#           title =
#             
#             "Quantidade por Período e Tipo de Contribuinte",
#           
#           x = "Período",
#           
#           y = "Quantidade",
#           
#           fill = "Tipo"
#           
#         ) +
#         
#         theme_minimal()
#       
#     })
#     
#   })
#   
# }