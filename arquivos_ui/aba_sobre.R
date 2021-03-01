aba_sobre <- tabItem(
  "sobre",
  fluidPage(
    fluidRow(
      tags$img(src = "logo_projeto_anomalias_git.png", height = 107 * 0.75),
      tags$img(
        src = "logo_parceiros_projeto.png",
        height = 107 * 0.75,
        width = 1275 * .75
      )
    ),
    fluidRow(
      tags$img(src = "ufrgs_logo.png", height = 107 * 0.75),
      tags$img(src = "logos_hcpa_ibc.png", height = 107 * 0.75),
      tags$img(src = "logo_ime.png", height = 107 * 0.75)
    ),
    br(),
    fluidRow(
      widgetUserBox(
        title = "Projeto Anomalias Congênitas",
        h4(str_c(
          "Esse Aplicativo foi desenvolvido pelo grupo de epidemiologia do Projeto Anomalias Congênitas e tem como objetivo
                                                                                    mostrar a distribuição das Anomalias Congênitas por município de  residência da mãe do Estado ",preposicao_sigla_uf," ", uf_extenso,"."
        )),
        h4(
          "Fonte de dados: Sistema de Informação sobre Nascidos Vivos (SINASC)"
        ),
        type = 2,
        collapsible = TRUE,
        color = "primary",
        width = 12,
        tags$div(class = "box box-widget widget-user-2", style =
                   "left: 407px;bottom: 207px;")
      ),
      h2("Equipe de desenvolvimento do Aplicativo")
    ),
    fluidRow(
      widgetUserBox(
        title = "Márcia Helena Barbian",
        subtitle = "Professora do Departamento de Estatística da UFRGS",
        type = 2,
        width = 4,
        #height = 180,
        src = 'marcia.png',
        color = "blue",
        "Contato: mhbarbian@ufrgs.br",
        footer_padding = F,
        collapsible = FALSE
      ),
      widgetUserBox(
        title = "Bruno Alano da Silva",
        subtitle = HTML("Estudante de Estatística da UFRGS"),
        type = 2,
        width = 4,
        #height = 180,
        src = 'bruno.jpg',
        color = "blue",
        "Contato: alano.bruno31@gmail.com",
        footer_padding = F,
        collapsible = FALSE
      ),
      widgetUserBox(
        title = "Guilherme Rodrigues Boff",
        subtitle = HTML("Estudante de Estatística da UFRGS"),
        type = 2,
        width = 4,
        #height = 180,
        src = 'guilherme.jpeg',
        color = "blue",
        "Contato: guilherme_rboff@hotmail.com",
        footer_padding = F,
        collapsible = FALSE
      )
    ),
    fluidRow(
      widgetUserBox(
        title = "Fernanda Sales Luiz Vianna",
        subtitle = "Professora do Departamento de Genética da UFRGS",
        type = 2,
        width = 4,
        #height = 180,
        src = 'fernanda.gif',
        color = "blue",
        "Contato: fslvianna@gmail.com",
        footer_padding = F,
        collapsible = FALSE
      ),
      widgetUserBox(
        title = "Luiza Monteavaro Mariath",
        subtitle = HTML(
          "Doutora em Ciências (Genética e Biologia Molecular) pelo PPGBM da UFRGS"
        ),
        type = 2,
        width = 4,
        #height = 180,
        src = 'luiza.jpg',
        color = "blue",
        "Contato: luiza_mariath@hotmail.com",
        footer_padding = F,
        collapsible = FALSE
      ),
      widgetUserBox(
        title = "Thayne Woycinck Kowalski",
        subtitle = HTML(
          "Professora do Centro Universitário CESUCA, pesquisadora do Núcleo de Bioinformática do HCPA"
        ),
        type = 2,
        width = 4,
        #height = 180,
        src = 'thayne.jpg',
        color = "blue",
        "Contato: thaynewk@gmail.com",
        footer_padding = F,
        collapsible = FALSE
      )
    ),
    fluidRow(
      h2("Coordenadora do Projeto"),
      widgetUserBox(
        title = "Lavinia Schüler Faccini",
        subtitle = "Professora do Departamento de Genética da UFRGS",
        type = 2,
        width = 4,
        #height = 180,
        src = 'lavinia.jpg',
        color = "blue",
        "Contato: lschuler@hcpa.edu.br",
        footer_padding = F,
        collapsible = FALSE
      )
    ),
    fluidRow(
      h2("Código Fonte"),
      valueBox(
        "Repositório Github",
        subtitle = div(
          "Confira aqui nosso repositório no GitHub!",
          br(),
          "Contato: projeto.anomalias.congenitas@gmail.com"
        ),
        icon = icon("github"),
        color = "green",
        width = 12,
        href = "https://github.com/anomaliascongenitas/App_Anomalias_Congenitas_RS"
      )
    ),
    br(),
    fluidRow(column(
      width = 6,
      #tags$img(src="ufrgs_logo.png", height = 100, width = 127),
      tags$img(
        src = "pos_estatistica_logo.png",
        height = 100,
        width = 220
      ),
      tags$img(
        src = "ppg_genetica.png",
        height = 100,
        width = 124
      )
    ))
  )
) 
