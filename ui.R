##########################################################################################
##                                                                                      ##
##                                Análise Fatorial                                      ## 
##                                   Ismael Dias                                        ##
##                                                                                      ##
##                                                                                      ##
##                                                                                      ##
##########################################################################################



## LENDO OS DADOS
dados = read.table("Beer.txt", header = T)

# SUBSTITUINDO OS VALORES VAZIOS PELA MÉDIA GERAL
dados$COST[which(is.na(dados$COST))] = mean(dados$COST, na.rm = T)
dados$SIZE[which(is.na(dados$SIZE))] = mean(dados$SIZE, na.rm = T)
dados$ALCOHOL[which(is.na(dados$ALCOHOL))] = mean(dados$ALCOHOL, na.rm = T)
dados$REPUTAT[which(is.na(dados$REPUTAT))] = mean(dados$REPUTAT, na.rm = T)
dados$COLOR[which(is.na(dados$COLOR))] = mean(dados$COLOR, na.rm = T)
dados$AROMA[which(is.na(dados$AROMA))] = mean(dados$AROMA, na.rm = T)
dados$TASTE[which(is.na(dados$TASTE))] = mean(dados$TASTE, na.rm = T)
dados$SES[which(is.na(dados$SES))] = mean(dados$SES, na.rm = T)
dados$GROUP[which(is.na(dados$GROUP))] = mean(dados$GROUP, na.rm = T)

#REMOVENDO VARIÁVEL NA QUAL EM ANÁLISE PRÉVIA MOSTROU NAO FAZER PARTE DE NENHUM GRUPO
dados = dados[,-c(4)]

# INTERFACE
ui = dashboardPage(
  dashboardHeader(title = "Análise Fatorial - Ismael Dias", titleWidth = "300px"),
  dashboardSidebar(width = "300px",
    sidebarMenu(
      menuItem("Dados", tabName = "dados_tab", icon = icon("database")),
      menuItem("Análise descritiva", tabName = "descritiva_tab", icon = icon("chart-bar")),
      menuItem("Análise Multivariada", tabName = "multivariada_tab", icon = icon("project-diagram"),
               menuItem("Correlação",tabName = "correlacao_tab", icon = icon("chart-line")),
               menuItem("Normalidade M. e Bartlett", tabName = "normalidade_tab", icon = icon("database")),
               menuItem("Análise Fatorial", tabName = "fatorial_tab", icon = icon("object-group"))
               ),
      menuItem("Resultados", tabName = "resultados_tab", icon = icon("tasks"),
               box( width = NULL,
                    tags$p("Os dados apresentam divisão em 2",br(),"fatores, sendo o primeiro",br(),"relacionado aos aspectos",br()," comerciais da bebida, já",br(),"
o segundo fator estaria",br()," relacionado aos aspectos",br()," físicos da cerveja, o",br()," que faz total sentido.", style = "font-size: 100%;color:black;")      
               )
      )
    )
    )
  ,
  dashboardBody(
    tabItems(tabItem(tabName = "correlacao_tab",
                     fluidRow(
                       
                       column(width = 4,
                              box(width = NULL, title = "Correlações aparentes", status = "warning",
                                  h4("Observando o gráfico de dispersão, percebe-se correlação forte positiva entre algumas
variáveis. Estas correlações indicam a presença de dois grupos ou até mesmo 3 grupos.")
                              ),
                              box(title = "Gráfico de correlação", status = "warning",width = NULL,
                              plotOutput("correlacao_plot")
                              )
                       ),
                       column(width = 8,
                              box(width = NULL, status = "warning",
                              fluidRow(
                                valueBox(value = tags$p("COLOR X AROMA", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                valueBox(value = tags$p("COLOR X TASTE", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                valueBox(value = tags$p("TASTE X AROMA", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                ),
                              box(title = "Gráfico de dispersão para todas as variáveis", status = "warning", width = NULL,
                              plotOutput("dispersao_plot")
                              ),
                              fluidRow(
                                box(status = "warning", width = NULL,
                                valueBox(value = tags$p("COST X SIZE", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                valueBox(value = tags$p("COST X ALCOHOL", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                valueBox(value = tags$p("ALCOHOL X SIZE", style = "font-size: 50%;"), subtitle = "Forte",width = 4),
                                )
                                ),
                              )
                       )
                     )
    ),
      
      tabItem(tabName = "dados_tab",
              fluidRow( setBackgroundImage( shinydashboard = TRUE,
                src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRwHUcZdHUx7QfWjzw7-gK6pRhRost1GDtBOg&usqp=CAU"
                ),
                column(width = 4,
                       box(height = "600px", width = NULL,status = "warning",title = "Descrição dos dados",
                           h4("Para esta análise foram utilizados dados coletados com base na opinião de consumidores
em relação a importância de sete diferentes qualidades na decisão pela compra de um pack com
seis garrafas de cerveja. As variáveis são:"),
                           br(),
                           tags$li("COST: Custo do pack;"), 
                           tags$li("SIZE: Volume de cerveja no pack;"), 
                           tags$li("ALCOHOL: porcentagem de ́alcool na bebida;"),
                           tags$li("COLOR: cor da bebida;"), 
                           tags$li("AROMA: aroma da bebida;"),
                           tags$li("TASTE: sabor da bebida.")

                           )
                       ),
                column(width = 8,
                       box(width = NULL, status = "warning",title = "Base de dados",
                       dataTableOutput("basededados_output")
                       )
                       )
              )
      ),
    tabItem(tabName = "descritiva_tab",
            fluidRow(
              column(width = 4,
                     box(width = NULL,status = "warning",title = "Boxplot",
                         h4("Os dados parecem estar bem comportados, sem a presença de outliers em nenhuma das
variáveis, o que é bom para a análise.")),
                     box(width = NULL, status = "warning", title = "Histograma",
                         h4("Visualmente, os histogramas não apontam normalidade para os dados, porém a normalidade multivariada é testada na aba 'Normalidade Multivariada' ")),
                     box(width = NULL, status = "warning",
                         h4("Este é o summary obtido:"),
                         verbatimTextOutput("summary_output")
                     )
                         ),
              column(width = 8,
                     box(status = "warning",width = NULL,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Boxplot",
                                          plotOutput("boxplot_output")
                                          ),
                                 tabPanel("Dispersão",
                                   plotOutput("histograma_output")
                                 ),
                                 tabPanel("Histograma",
                                          selectInput(inputId = "histograma_input", "Selecione a variável:", choices = colnames(dados)[-8]),
                                          plotOutput("histogramavariavel_output")
                                          )
                                 ))
                         )
            
            )
            ),
            tabItem(tabName = "normalidade_tab",
                    fluidRow(
                      column(width = 6,
                             box(width = NULL, title = "Teste de normalidade multivariada", status = "warning",
                                 h4("O teste de shapiro indicou não haver normalidade multivariada nos dados. Porém, como as variáveis
                                    possuem uma boa correlação entre si, o teste de espericidade de Bartlett fez-se necessário para
                                    verificar se existe correlação suficiente para aplicação da técnica multivariada."),
                                 verbatimTextOutput("shapiro_output")
                                 )
                        
                      ),
                      column(width = 6,
                             box(width = NULL, title = "Teste de espericidade de Bartlett", status = "warning",
                                 h4("O p-valor de aproximadamente 0 foi obtido, rejeitando
a hipótese nula de que nao existe correlação suficiente para aplicação da técnica multivariada. Sendo assim, a base de dados pode ser utilizada para técnicas multivariadas."),
                                 verbatimTextOutput("bartlett_output")
                             )
                      )
                    )
                    ),
              tabItem(tabName = "fatorial_tab",
                      fluidRow(
                        column(width = 4,
                               box(width = NULL, title = "Critério de Kaiser", status = "warning",
                                   h4("Através do critério de Kaiser, o número de componentes obtidos foi 2, como já era possível observar
através do gráfico de correlação."),
                                   plotOutput("kaiser_output"),
                                   h4("É importante observar que ambos os componentes juntos(1 e 2) explicam 89,1% da variabilidade dos dados.")
                                   )
                               ),
                        column(width = 3,
                               box(width = NULL, title = "Extração dos fatores utilizando o metodo das componentes principais", status = "warning",
                                   h4("Aplicando a rotação Verimax, tem-se:"),
                                   verbatimTextOutput("analise_plot"),
                                   h4("Qualidade do ajuste de 0,9962 obtida."),
                                   verbatimTextOutput("qualidadeajuste_plot")
                                   )
                      ),
                        column(width = 5,
                               box(title = "Fator 1", status = "warning",width = NULL,
                                   valueBox("COST",subtitle = "0.90"),
                                   valueBox("ALCOHOL",subtitle = "0.95"),
                                   valueBox("SIZE", subtitle = "0.97"),
                                   ),
                               box(title = "Fator 2", status = "warning",width = NULL,
                                   valueBox("AROMA", subtitle = "0.97"),
                                   valueBox("TASTE",subtitle = "0.95"),
                                   valueBox("SES", subtitle = "0.65"),
                                   valueBox("GROUP", subtitle = "0.5")
                               )
                               )
                      )
                      )
            )
            ),
  skin = "black"
      )
shinyUI(ui)




