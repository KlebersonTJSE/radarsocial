# =====================================================
# APP.R
# =====================================================

library(shiny)
library(bslib)
library(DT)
library(jsonlite)

# =====================================================
# CARREGA VARIÁVEIS DE AMBIENTE
# =====================================================

readRenviron(
  "C:/Users/3894/OneDrive - Tribunal de Justiça do Estado de Sergipe/Documentos/GitHub/radarsocial/conf/.Renviron"
)

# =====================================================
# SOURCES
# =====================================================

source("R/utils.R")
source("R/auth.R")
source("R/database.R")

source("modules/mod_usuario.R")
source("modules/mod_rejeitados.R")
source("modules/mod_ocorrencias.R")
source("modules/mod_totalizadores.R")
# source("modules/mod_consulta_sql.R")
# source("modules/mod_declaracao.R")
# source("modules/mod_alertas.R")

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

      .login {
        width: 420px;
        margin: auto;
        margin-top: 120px;
        background: white;
        padding: 40px;
        border-radius: 20px;
        box-shadow: 0 3px 20px rgba(0,0,0,.15);
      }

      .logo {
        text-align: center;
        font-size: 30px;
        font-weight: bold;
        color: #003366;
        margin-bottom: 25px;
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

  # ===================================================
  # ESTADO DO CABEÇALHO
  # ===================================================

  header_oculto <- reactiveVal(FALSE)

  # ===================================================
  # MÓDULO SELECIONADO
  # ===================================================

  menuSelecionado <- reactiveVal("Usuário")

  # ===================================================
  # LOGIN
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

      if (!is.null(dados)) {

        autenticado(TRUE)

        usuarioLogado(input$usuario)

        dadosUsuario(dados)

        fotoUsuario(
          obter_foto_usuario(dados)
        )

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
  # LOGOUT
  # ===================================================

  observeEvent(

    input$sair,

    {

      autenticado(FALSE)

      usuarioLogado(NULL)

      dadosUsuario(NULL)

      fotoUsuario(NULL)

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

      class = "login",

      div(
        class = "logo",
        "RadarSocial"
      ),

      textInput("usuario", "Usuário"),

      passwordInput("senha", "Senha"),

      div(
        id = "capslock_warning",
        icon("triangle-exclamation"),
        " Caps Lock está ativado"
      ),

      br(),

      actionButton(
        "entrar",
        "Entrar",
        class = "btn btn-primary w-100"
      )

    )

  })

  # ===================================================
  # TELA PRINCIPAL
  # ===================================================
  # OBS: usamos isolate() em menuSelecionado() aqui porque este bloco
  # só deve rodar quando `autenticado()` muda (login/logout).
  # Se a aba selecionada (input$menu) disparasse este renderUI, toda a
  # navset_tab -- e os módulos dentro dela -- seriam recriados a cada
  # troca de aba, perdendo o estado interno dos módulos (o mesmo bug
  # de combos que não populavam, já visto em outros módulos).
  # A troca de aba em si é sincronizada via updateTabsetPanel() acima.
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
            extrair_manager(dadosUsuario()$manager)

          )

        ),

        hr(),

        # ===============================================
        # ABAS
        # ===============================================

        navset_tab(

          id = "menu",

          selected = isolate(menuSelecionado()),

          # nav_panel("Usuário", mod_usuario_ui("usuario")),
          # nav_panel("Alertas", mod_alertas_ui("alertas")),
          nav_panel("Ocorrências", mod_ocorrencias_ui("ocorrencias")),
          nav_panel("Rejeitados", mod_rejeitados_ui("rejeitados")),
          nav_panel("Totalizadores", mod_totalizadores_ui("totalizadores"))#,
          # nav_panel("Consulta SQL", mod_consulta_sql_ui("sql")),
          # nav_panel("Declaração", mod_declaracao_ui("declaracao"))

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

  mod_ocorrencias_server(
    "ocorrencias",
    ativo = reactive(menuSelecionado() == "Ocorrências")
  )

  mod_totalizadores_server(
    "totalizadores",
    ativo = reactive(menuSelecionado() == "Totalizadores")
  )

  # mod_consulta_sql_server("sql")

  # mod_declaracao_server("declaracao")

  # mod_alertas_server(
  #   "alertas",
  #   ativo = reactive(menuSelecionado() == "Alertas")
  # )

}

# =====================================================
# EXECUÇÃO
# =====================================================

shinyApp(
  ui = ui,
  server = server
)