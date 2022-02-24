
navbarPage("Dashboard",theme = shinytheme("slate"),
           
                           tabPanel("Diferença",theme = shinytheme("slate"),
                titlePanel("Diferença de ações entre Ativo - Ibovespa"),   
                mainPanel(p("Atualizado em:",yyy[1,1]),
                          
                ),
                column(
                  12,fluidRow(column(12, highchartOutput('chart22')))
                ),
                downloadButton("downloadData5", "Download Ativo-Ibovespa"),
                h2("Ativo - Ibovespa"),
                DT::dataTableOutput("table_AI"),

),

                        tabPanel("Ativo",  
                         column(12,h2("Ativo"),p("Fonte:",a("https://br.investing.com/equities/imc-holdings-on-historical-data",   href="https://br.investing.com/equities/imc-holdings-on-historical-data")),
                                fluidRow(column(12, highchartOutput('chart_1'))),
                                h2("Download dados Ativo"), 
                                downloadButton("downloadData2", "Download Ativo"),
                                h2("Download dados Ativo Histograma"), 
                                downloadButton("downloadData3", "Download Ativo_H"),
                                h2("Ativo - Tabela"),
                                DT::dataTableOutput("table_A"))
                             ),
           
                          tabPanel("Ibovespa",          
                         column(12,h2("Ibovespa"),p("Fonte:",a("https://br.investing.com/indices/bovespa-historical-data",   href="https://br.investing.com/indices/bovespa-historical-data")),
                                fluidRow(column(12, highchartOutput('chart_2'))),h2("Download dados Ibovespa"), 
                                downloadButton("downloadData1", "Download Ibovespa"),
                                h2("Download dados Ibovespa Histograma"), 
                                downloadButton("downloadData4", "Download Ibovespa_H"),
                                h2("Ibovespa - Tabela"),
                                DT::dataTableOutput("table_I"),),
                            ),

                
                tabPanel("Criadores",
                         fluidRow(
                           tags$style(HTML("
                    img {
                      border-radius: 50%;
                    }")),
                           column(width = 6,uiOutput("img"),h2("Alessandro Pereira"),p("Graduando em Estatística da Universidade Federal do Rio Grande do Norte. Possui experiência em ciência de dados, principalmente na utilização da linguagem R. Usuário avançado do framework shiny, utilizado para a construção de dashboards.") ,p("GitHub:",a("https://github.com/AlessandroPTSN",   href="https://github.com/AlessandroPTSN"))),
                           column(width = 6, uiOutput("img2"),h2("Felipe Sergio"),p(" Analista de TI focado em desenvolvimento de software, linguagens de programação e infraestrutura como código. Profissional com experiência comprovada nas áreas de administração de sistemas Linux,Gerenciamento de datacenter, técnico, consultoria e implantação de sistemas corporativos."),p("GitHub:",a("https://github.com/felipesergios",   href="https://github.com/felipesergios")))
                         
                )
                         
               
                )
           
           
           
)
