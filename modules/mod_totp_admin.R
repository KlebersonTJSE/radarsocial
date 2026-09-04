# =====================================================
# modules/mod_totp_admin.R
# Cadastro e gerenciamento de usuários TOTP (Authenticator)
# Recomendado disponibilizar apenas para quem entrou via AD.
# =====================================================

mod_totp_admin_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        4,
        
        wellPanel(
          
          h4("Cadastrar usuário TOTP"),
          
          textInput(ns("novo_login"), "Login"),
          textInput(ns("novo_nome"), "Nome"),
          
          actionButton(
            ns("cadastrar"),
            "Gerar chave",
            class = "btn btn-primary w-100"
          ),
          
          uiOutput(ns("resultado_cadastro"))
          
        )
        
      ),
      
      column(
        8,
        
        h4("Usuários TOTP cadastrados"),
        
        DTOutput(ns("tabela_usuarios")),
        
        br(),
        
        actionButton(
          ns("desativar"),
          "Desativar selecionado",
          class = "btn btn-outline-danger"
        )
        
      )
      
    )
    
  )
  
}

mod_totp_admin_server <- function(id, con, ativo = reactive(TRUE)) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    atualizar <- reactiveVal(0)
    
    # ===================================================
    # CADASTRO
    # ===================================================
    
    observeEvent(input$cadastrar, {
      
      req(input$novo_login, input$novo_nome)
      
      resultado <- tryCatch(
        cadastrar_usuario_totp(con, input$novo_login, input$novo_nome),
        error = function(e) {
          showNotification(paste("Erro:", e$message), type = "error")
          NULL
        }
      )
      
      req(resultado)
      
      qr <- gerar_qrcode_base64(resultado$uri)
      
      output$resultado_cadastro <- renderUI({
        
        tagList(
          
          hr(),
          
          p(
            strong("Chave secreta (entrada manual): "),
            code(resultado$secret)
          ),
          
          if (!is.null(qr)) {
            
            tags$img(
              src = qr,
              style = "width:220px;height:220px;display:block;margin:0 auto;"
            )
            
          } else {
            
            tagList(
              p(
                class = "text-muted",
                "Pacote 'qrcode' não instalado — use a chave acima em ",
                "'Inserir código manualmente' no Microsoft/Google Authenticator."
              ),
              p(code(resultado$uri))
            )
            
          },
          
          p(
            class = "text-muted mt-2",
            "Peça ao usuário para adicionar a chave no aplicativo autenticador ",
            "antes de sair desta tela — a chave não fica visível novamente depois."
          )
          
        )
        
      })
      
      updateTextInput(session, "novo_login", value = "")
      updateTextInput(session, "novo_nome", value = "")
      
      atualizar(atualizar() + 1)
      
    })
    
    # ===================================================
    # LISTAGEM
    # ===================================================
    
    output$tabela_usuarios <- renderDT({
      
      atualizar()
      
      listar_usuarios_totp(con)
      
    }, selection = "single", options = list(pageLength = 8), rownames = FALSE)
    
    # ===================================================
    # DESATIVAÇÃO
    # ===================================================
    
    observeEvent(input$desativar, {
      
      linha <- input$tabela_usuarios_rows_selected
      
      req(linha)
      
      dados <- listar_usuarios_totp(con)
      
      desativar_usuario_totp(con, dados$login[linha])
      
      showNotification("Usuário TOTP desativado", type = "message")
      
      atualizar(atualizar() + 1)
      
    }, ignoreInit = TRUE)
    
  })
  
}