# =====================================================
# APP.R
# =====================================================

# Corrige o diretório de trabalho caso o projeto não
# tenha sido aberto pelo .Rproj
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
}

library(here)
here::i_am("app.R")

readRenviron(here::here(".Renviron"))

library(shiny)
library(bslib)
library(DT)
library(jsonlite)
library(digest)

library(DBI)
library(RSQLite)

library(reticulate)

python_path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.which("python"))
if (!nzchar(python_path) || !file.exists(python_path)) {
    stop(
        "Python não encontrado em '", python_path, "'. ",
        "Verifique a instalação do Python ou defina RETICULATE_PYTHON no .Renviron ",
        "apontando para o python.exe correto."
    )
}
use_python(python_path, required = TRUE)

# =====================================================
# CARREGA CONEXAO DB PARA LOGIN COM AUTH
# =====================================================
con <- dbConnect(
  SQLite(),
  "data/radarsocial.db"
)

# =====================================================
# SOURCES
# =====================================================

source("R/utils.R")
source("R/auth.R")
source("R/auth_totp.R")
source("R/database.R")

source("modules/mod_usuario.R")
source("modules/mod_rejeitados.R")
source("modules/mod_inconsistencias.R")
source("modules/mod_totalizadores.R")
source("modules/mod_totp_admin.R")

# =====================================================
# RECURSOS ESTÁTICOS
# =====================================================

addResourcePath("img", "img")

# =====================================================
# UI
# =====================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  tags$head(
    
    tags$style(HTML("

      body {
        background: #f4f6f9;
      }

      /* =============================================
         LOGIN - CARD ENTERPRISE
         ============================================= */

      .login-wrapper {
        min-height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 40px 20px;
      }

      .login-card {
        width: 100%;
        max-width: 460px;
        background: #fff;
        border: 1px solid rgba(0,0,0,.05);
        border-radius: 1rem;
      }

      .login-icon-badge {
        width: 60px;
        height: 60px;
        border-radius: 50%;
        margin: 0 auto;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #003366, #0d6efd);
        color: #fff;
        font-size: 1.4rem;
        box-shadow: 0 .4rem 1rem rgba(13,110,253,.25);
      }

      .login-title {
        text-align: center;
        letter-spacing: -.01em;
      }

      .login-subtitle {
        text-align: center;
        font-size: .9rem;
      }

      /* Cartões de método de acesso (radioButtons estilizado) */

      .metodo-opcoes .radio {
        margin-bottom: .85rem;
      }

      .metodo-opcoes .radio label {
        display: flex;
        align-items: flex-start;
        gap: .85rem;
        width: 100%;
        margin: 0;
        border: 1.5px solid #e2e6ea;
        border-radius: .85rem;
        padding: 1rem 1.1rem;
        cursor: pointer;
        transition: border-color .15s ease,
                    background-color .15s ease,
                    box-shadow .15s ease,
                    transform .1s ease;
      }

      .metodo-opcoes .radio label:hover {
        border-color: #8fb8ff;
        background: #f5f9ff;
        box-shadow: 0 .25rem .75rem rgba(13,110,253,.08);
        transform: translateY(-1px);
      }

      .metodo-opcoes .radio input[type=radio] {
        margin-top: .3rem;
        accent-color: #0d6efd;
        width: 1.05rem;
        height: 1.05rem;
        flex-shrink: 0;
      }

      .metodo-opcoes .radio:has(input:checked) label {
        border-color: #0d6efd;
        background: #eef4ff;
        box-shadow: 0 .3rem .9rem rgba(13,110,253,.15);
      }

      .metodo-opcao-icone {
        color: #0d6efd;
        font-size: 1.15rem;
        margin-top: .1rem;
      }

      .metodo-opcao-titulo {
        font-weight: 600;
        color: #212529;
      }

      .metodo-opcao-desc {
        font-weight: 400;
        font-size: .8rem;
        color: #6c757d;
      }

      .btn-acesso {
        height: 48px;
        font-weight: 600;
        font-size: 1rem;
        border-radius: .6rem;
      }

      .voltar-link {
        font-size: .85rem;
        color: #6c757d !important;
      }

      .voltar-link:hover {
        color: #0d6efd !important;
      }

      .logo {
        text-align: center;
        font-size: 30px;
        font-weight: bold;
        color: #003366;
        margin-bottom: 25px;
      }

      .logo-login {
        display: block;
        width: 100%;
        max-width: 320px;
        height: auto;
        margin: 0 auto 30px auto;
        filter: drop-shadow(0 3px 8px rgba(0,0,0,.10));
      }

      .foto {
        width: 180px;
        border-radius: 50%;
        border: 4px solid #ddd;
      }

      .shiny-notification {
        position: fixed !important;
        top: 20px !important;
        right: 20px !important;
        left: auto !important;
        bottom: auto !important;
        transform: none !important;
      }

      #capslock_warning {
        display: none;
        margin-top: 8px;
        padding: 6px 10px;
        font-size: 13px;
        color: #842029;
        background: #f8d7da;
        border: 1px solid #f5c2c7;
        border-radius: 6px;
      }

      .icon-bar {
        position: fixed;
        top: 0;
        left: 0;
        width: 52px;
        height: 100vh;
        background: #003366;
        display: flex;
        flex-direction: column;
        align-items: center;
        padding-top: 14px;
        gap: 12px;
        z-index: 1050;
      }

      .icon-bar .icon-btn {
        width: 36px;
        height: 36px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: rgba(255,255,255,.75);
        font-size: 16px;
        border-radius: 8px;
        cursor: pointer;
        text-decoration: none !important;
        transition: background .15s ease,
                    color .15s ease;
      }

      .icon-bar .icon-btn:hover {
        background: rgba(255,255,255,.12);
        color: #fff;
      }

      .icon-bar .icon-btn.sair {
        margin-top: auto;
        margin-bottom: 14px;
        color: #ff9d9d;
      }

      .icon-bar .icon-btn.sair:hover {
        background: rgba(220,53,69,.25);
        color: #fff;
      }

      #app-content {
        margin-left: 52px;
        padding: 20px 25px;
      }

      .header-container {
        overflow: hidden;
        max-height: 220px;
        opacity: 1;
        transition: max-height .28s ease,
                    opacity .2s ease,
                    margin .28s ease;
        margin-bottom: 15px;
      }

      .header-container.collapsed {
        max-height: 0;
        opacity: 0;
        margin-bottom: 0;
      }

    ")),
    
    # =================================================
    # AVISO DE CAPS LOCK
    # =================================================
    
    tags$script(HTML("

      $(document).on(
        'keydown keyup focus',
        '#senha',
        function(event) {

          var aviso =
            document.getElementById(
              'capslock_warning'
            );

          if (!aviso) return;

          if (
            event.originalEvent &&
            typeof event.originalEvent
              .getModifierState === 'function'
          ) {

            if (
              event.originalEvent
                .getModifierState('CapsLock')
            ) {

              $(aviso).show();

            } else {

              $(aviso).hide();

            }

          }

        }
      );

      $(document).on(
        'blur',
        '#senha',
        function() {

          $('#capslock_warning').hide();

        }
      );

    ")),
    
    # =================================================
    # TOGGLE DO CABEÇALHO (client-side, preserva estado dos módulos)
    # =================================================
    
    tags$script(HTML("

      Shiny.addCustomMessageHandler(

        'toggle-header',

        function(message) {

          var header =
            document.querySelector(
              '.header-container'
            );

          if (!header) return;

          if (message.oculto) {

            header.classList.add(
              'collapsed'
            );

          } else {

            header.classList.remove(
              'collapsed'
            );

          }

        }

      );

    "))
    
  ),
  
  # ===================================================
  # LOGIN
  # ===================================================
  
  uiOutput("tela_login"),
  
  # ===================================================
  # SISTEMA PRINCIPAL
  # ===================================================
  
  uiOutput("tela_principal")
  
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ===================================================
  # ESTADO DA SESSÃO
  # ===================================================
  
  autenticado <- reactiveVal(FALSE)
  usuarioLogado <- reactiveVal(NULL)
  dadosUsuario <- reactiveVal(NULL)
  fotoUsuario <- reactiveVal(NULL)
  
  # Método escolhido na tela de seleção ("ad" | "totp" | NULL = seletor)
  metodoAcesso <- reactiveVal(NULL)
  
  # Método efetivamente usado no login bem-sucedido ("AD" | "TOTP")
  metodoAutenticado <- reactiveVal(NULL)
  
  # ===================================================
  # ESTADO DO CABEÇALHO
  # ===================================================
  
  header_oculto <- reactiveVal(FALSE)
  
  # ===================================================
  # MÓDULO SELECIONADO
  # ===================================================
  
  menuSelecionado <- reactiveVal("Usuário")
  
  # ===================================================
  # SELEÇÃO DO MÉTODO DE ACESSO
  # ===================================================
  
  observeEvent(
    
    input$continuar,
    
    {
      
      req(input$metodo_acesso)
      
      metodoAcesso(input$metodo_acesso)
      
    },
    
    ignoreInit = TRUE
    
  )
  
  observeEvent(
    
    input$voltar_metodo,
    
    {
      
      metodoAcesso(NULL)
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # LOGIN - AD
  # ===================================================
  
  observeEvent(
    
    input$entrar,
    
    {
      
      req(
        input$usuario,
        input$senha
      )
      
      dados <- authenticate_ad(
        input$usuario,
        input$senha
      )
      
      registrar_auditoria(
        con,
        input$usuario,
        "AD",
        !is.null(dados)
      )
      
      if (!is.null(dados)) {
        
        autenticado(TRUE)
        
        usuarioLogado(input$usuario)
        
        dadosUsuario(dados)
        
        fotoUsuario(
          obter_foto_usuario(dados)
        )
        
        metodoAutenticado("AD")
        
        menuSelecionado("Usuário")
        
        # Garante que a aba volte para "Usuário" sem recriar a UI toda
        updateTabsetPanel(
          session,
          "menu",
          selected = "Usuário"
        )
        
        showNotification(
          paste(
            "Bem-vindo",
            obter_campo(dados, "displayName")
          ),
          type = "message"
        )
        
      } else {
        
        showNotification(
          "Usuário ou senha inválidos",
          type = "error"
        )
        
      }
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # LOGIN - TOTP (Authenticator)
  # ===================================================
  
  observeEvent(
    
    input$entrar_totp,
    
    {
      
      req(
        input$usuario_totp,
        input$codigo_totp
      )
      
      dados <- autenticar_totp(
        con,
        input$usuario_totp,
        input$codigo_totp
      )
      
      if (!is.null(dados)) {
        
        autenticado(TRUE)
        
        usuarioLogado(dados$login)
        
        dadosUsuario(dados)
        
        fotoUsuario(NULL)
        
        metodoAutenticado("TOTP")
        
        menuSelecionado("Usuário")
        
        updateTabsetPanel(
          session,
          "menu",
          selected = "Usuário"
        )
        
        showNotification(
          paste(
            "Bem-vindo",
            dados$displayName
          ),
          type = "message"
        )
        
      } else {
        
        showNotification(
          "Usuário ou código inválido",
          type = "error"
        )
        
      }
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # LOGOUT
  # ===================================================
  
  observeEvent(
    
    input$sair,
    
    {
      
      autenticado(FALSE)
      usuarioLogado(NULL)
      dadosUsuario(NULL)
      fotoUsuario(NULL)
      metodoAutenticado(NULL)
      metodoAcesso(NULL)
      menuSelecionado("Usuário")
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # ALTERNÂNCIA DO CABEÇALHO (client-side, não recria a UI)
  # ===================================================
  
  observeEvent(
    
    input$toggle_header,
    
    {
      
      header_oculto(!header_oculto())
      
      session$sendCustomMessage(
        "toggle-header",
        list(oculto = header_oculto())
      )
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # CAPTURA DA ABA SELECIONADA
  # ===================================================
  
  observeEvent(
    
    input$menu,
    
    {
      
      req(input$menu)
      
      menuSelecionado(input$menu)
      
    },
    
    ignoreInit = TRUE
    
  )
  
  # ===================================================
  # TELA DE LOGIN
  # ===================================================
  
  output$tela_login <- renderUI({
    
    if (autenticado()) {
      return(NULL)
    }
    
    div(
      class = "login-wrapper",
      
      div(
        class = "login-card shadow-sm p-4",
        
        div(
          class = "logo-container mb-4",
          
          tags$img(
            src = "img/radarSocial_logo_horizontal_fundo_claro.png",
            class = "logo-login",
            alt = "RadarSocial"
          )
        ),
        
        div(
          class = "login-icon-badge mb-3",
          icon("shield-halved")
        ),
        
        tags$h4(
          "Acesso ao sistema",
          class = "login-title fw-bold mb-1"
        ),
        
        tags$p(
          "Escolha como deseja entrar no RadarSocial",
          class = "login-subtitle text-muted mb-4"
        ),
        
        if (is.null(metodoAcesso())) {
          
          # =============================================
          # PASSO 1 - SELETOR DE MÉTODO
          # =============================================
          
          tagList(
            
            div(
              class = "metodo-opcoes",
              
              radioButtons(
                "metodo_acesso",
                NULL,
                choiceNames = list(
                  
                  tagList(
                    icon("building-shield", class = "metodo-opcao-icone"),
                    div(
                      div("Login Corporativo (AD)", class = "metodo-opcao-titulo"),
                      div("Entrar com seu usuário e senha de domínio", class = "metodo-opcao-desc")
                    )
                  ),
                  
                  tagList(
                    icon("mobile-screen-button", class = "metodo-opcao-icone"),
                    div(
                      div("Código Authenticator", class = "metodo-opcao-titulo"),
                      div("Entrar com um código gerado no seu celular", class = "metodo-opcao-desc")
                    )
                  )
                  
                ),
                choiceValues = list("ad", "totp"),
                selected = character(0)
              )
              
            ),
            
            actionButton(
              "continuar",
              tagList("Continuar", icon("arrow-right", class = "ms-2")),
              class = "btn btn-primary w-100 btn-acesso mt-2"
            )
            
          )
          
        } else if (metodoAcesso() == "ad") {
          
          # =============================================
          # PASSO 2A - LOGIN CORPORATIVO (AD)
          # =============================================
          
          tagList(
            
            actionLink(
              "voltar_metodo",
              tagList(icon("arrow-left"), " Voltar"),
              class = "voltar-link mb-4 d-inline-block"
            ),
            
            div(
              class = "mb-3",
              textInput("usuario", "Usuário", width = "100%")
            ),
            
            div(
              class = "mb-2",
              passwordInput("senha", "Senha", width = "100%")
            ),
            
            div(
              id = "capslock_warning",
              icon("triangle-exclamation"),
              " Caps Lock está ativado"
            ),
            
            actionButton(
              "entrar",
              tagList(icon("right-to-bracket", class = "me-2"), "Entrar"),
              class = "btn btn-primary w-100 btn-acesso mt-4"
            )
            
          )
          
        } else if (metodoAcesso() == "totp") {
          
          # =============================================
          # PASSO 2B - CÓDIGO AUTHENTICATOR (TOTP)
          # =============================================
          
          tagList(
            
            actionLink(
              "voltar_metodo",
              tagList(icon("arrow-left"), " Voltar"),
              class = "voltar-link mb-4 d-inline-block"
            ),
            
            div(
              class = "mb-3",
              textInput("usuario_totp", "Usuário", width = "100%")
            ),
            
            div(
              class = "mb-2",
              textInput(
                "codigo_totp",
                "Código do Authenticator",
                placeholder = "000000",
                width = "100%"
              )
            ),
            
            actionButton(
              "entrar_totp",
              tagList(icon("key", class = "me-2"), "Entrar"),
              class = "btn btn-primary w-100 btn-acesso mt-4"
            )
            
          )
          
        }
        
      )
      
    )
  })
  
  # ===================================================
  # TELA PRINCIPAL
  # ===================================================
  
  output$tela_principal <- renderUI({
    
    req(autenticado())
    
    tagList(
      
      # =================================================
      # BARRA DE ÍCONES
      # =================================================
      
      div(
        
        class = "icon-bar",
        
        actionLink(
          "toggle_header",
          icon("id-badge"),
          class = "icon-btn",
          title = "Mostrar/ocultar informações do usuário"
        ),
        
        actionLink(
          "sair",
          icon("power-off"),
          class = "icon-btn sair",
          title = "Sair"
        )
        
      ),
      
      # =================================================
      # CONTEÚDO
      # =================================================
      
      div(
        
        id = "app-content",
        
        # ===============================================
        # CABEÇALHO
        # ===============================================
        
        div(
          
          class = "header-container",
          
          h2("Radar Social"),
          
          if (identical(metodoAutenticado(), "AD")) {
            
            tags$div(
              
              style = "color:#555;",
              
              tags$b("Usuário: "),
              obter_campo(dadosUsuario(), "displayName"),
              br(),
              
              tags$b("Departamento: "),
              obter_campo(dadosUsuario(), "department"),
              br(),
              
              tags$b("Criado em: "),
              formatar_whenCreated(
                obter_campo(dadosUsuario(), "whenCreated")
              ),
              br(),
              
              tags$b("Último acesso: "),
              formatar_lastLogon(
                obter_campo(dadosUsuario(), "lastLogonTimestamp")
              ),
              br(),
              
              tags$b("Gestor: "),
              extrair_manager(dadosUsuario()$manager),
              br(),
              
              tags$b("Método de acesso: "),
              "Login Corporativo (AD)"
              
            )
            
          } else {
            
            tags$div(
              
              style = "color:#555;",
              
              tags$b("Usuário: "),
              dadosUsuario()$displayName,
              br(),
              
              tags$b("Login: "),
              dadosUsuario()$login,
              br(),
              
              tags$b("Método de acesso: "),
              "Código Authenticator (TOTP)"
              
            )
            
          }
          
        ),
        
        hr(),
        
        # ===============================================
        # ABAS
        # ===============================================
        
        do.call(
          navset_tab,
          c(
            list(
              id = "menu",
              selected = isolate(menuSelecionado())
            ),
            list(
              nav_panel("Inconsistências", mod_inconsistencias_ui("inconsistencias")),
              nav_panel("Rejeitados", mod_rejeitados_ui("rejeitados")),
              nav_panel("Totalizadores", mod_totalizadores_ui("totalizadores"))
            ),
            if (identical(metodoAutenticado(), "AD")) {
              list(
                nav_panel("Administração TOTP", mod_totp_admin_ui("totp_admin"))
              )
            }
          )
        )
        
      )
      
    )
    
  })
  
  # ===================================================
  # SERVIDORES DOS MÓDULOS
  # ===================================================
  
  mod_usuario_server(
    "usuario",
    dados_usuario = dadosUsuario,
    foto_usuario = fotoUsuario
  )
  
  mod_rejeitados_server(
    "rejeitados",
    ativo = reactive(menuSelecionado() == "Rejeitados")
  )
  
  mod_inconsistencias_server(
      "inconsistencias",
      ativo = reactive(menuSelecionado() == "Inconsistências")
  )
  
  mod_totalizadores_server(
    "totalizadores",
    ativo = reactive(menuSelecionado() == "Totalizadores")
  )
  
  # Cadastro TOTP fica disponível apenas para quem entrou via AD
  # (a UI da aba só é renderizada nesse caso, mas o módulo em si
  # não depende disso para funcionar caso a regra mude no futuro).
  mod_totp_admin_server(
    "totp_admin",
    con = con,
    ativo = reactive(menuSelecionado() == "Administração TOTP")
  )
  
}

# =====================================================
# EXECUÇÃO
# =====================================================

shinyApp(
  ui = ui,
  server = server
)
