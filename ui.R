options(encoding = "utf-8")

library(shiny)

shinyUI(fluidPage(theme = shinytheme("united"),

    
    
    
    navbarPage("Jeux Olympiques",
               
               
               
               
               tabPanel("Home", icon = icon("home"),
                        h1("Les Jeux Olympiques", style = "color: #E95421; text-align: center; margin-bottom: 50px;"),
                        div(img(src = "img/Sports_JO.jpg", width = 400, heigth=250), style = "text-align: center;"),
                        
                        div("Les Jeux Olympiques, aussi appelÃ©s Jeux Olympiques modernes, puisqu'ils prolongent la tradition des jeux olympiques de la GrÃ¨ce 
                        antique, sont des Ã©vÃ¨nements sportifs internationaux majeurs. Ils regroupent des sports d'Ã©tÃ© ou d'hiver auxquels plusieurs milliers d'athlÃ¨tes 
                        participent Ã  travers diffÃ©rentes compÃ©titions tous les quatre ans, pour chaque olympiade moderne.", style = "text-align: justify; margin-top:50px; font-size: x-large; font-family: Avantgarde, TeX Gyre Adventor, URW Gothic L, sans-serif;"),
                        div("Avant de rentrer dans le vif du sujet, nous vous prÃ©sentons une petite vidÃ©o qui retrace l'Histoire des jeux Olympiques modernes.", style = "margin-top:10px; font-size: x-large; font-family: Avantgarde, TeX Gyre Adventor, URW Gothic L, sans-serif;"),
                        div(HTML("<iframe width=60% height=420px src='https://www.youtube.com/embed/VCRv6mZ4AfU' frameborder='0' allowfullscreen></iframe>"), style = "text-align: center; margin-top:50px;")
                        
                        
                        ),
               
               
               
               
               
              navbarMenu("JO", icon = icon("volleyball-ball"),

                        
                                     tabPanel("Participants", icon = icon("bar-chart-o"),

                                              fluidRow(
                                                  
                                                  
                                                  
                                                  column(width = 3,
                                                         wellPanel(
                                                             selectizeInput('JO_sex','Sexe', choices = c("M","F"), multiple = TRUE, selected = c("M","F")),
                                                             sliderInput("JO_year", label = "Slider Year", min = 1896, max = 2016, value = c(1896, 2016), sep="")
                                                             ),
                                                         
                                                         
                                                         wellPanel( "Map",
                                                             selectizeInput('JO_season','Season', choices = c("Summer","Winter"), multiple = TRUE, selected = "Summer")
                                                             )
                                                        ),
                                                  
                                                  
                                                  
                                                  column(width=9,
                                                         tabsetPanel(
                                                             tabPanel("Line + Map ",plotlyOutput("jo_dist"), div(plotOutput("jo_carte"), style = 'margin-top: 40px;') ),
                                                             tabPanel("Barplot", plotlyOutput("jo_bar"))
                                                            )
                                                         )
                                              
                                                    )
                                             ),
                         
                         
                         
                                     tabPanel("Villes", div(leafletOutput("jo_ville"),style = 'align: center; margin-left: 15px; margin-right: 15px;'), div( dataTableOutput("table_ville"), style = 'margin-top: 40px; align: center; margin-left: 15px; margin-right: 15px;') ),
                                     
                         
                         
                         
                                    tabPanel("Sports", 
                                              fluidRow(
                                                  
                                                  
                                                  
                                                  column(width=3,
                                                         wellPanel(
                                                         selectInput("JO_games", "Selectioner JO", choices = sort(unique(data$Games), decreasing = TRUE), selected = 1)
                                                                    )
                                                         ),
                                                  
                                                  
                                                  
                                                  column(width=9,
                                                         tabsetPanel(
                                                            tabPanel("Carre", plotOutput("jo_carre",  width = "100%", height = "750px"), div(h5("Nombre de sports :"), verbatimTextOutput("nb_sport"))),
                                                            tabPanel("Sports Anciens", dataTableOutput("table_sport_ancien"))
                                                         )
                                                         )
                                                    )
                                            )
                        ),
              
              
              
              
              
              
                        
               navbarMenu("Athlete", icon = icon("running"),
                          
                                    tabPanel("Par Sport",
                                             fluidRow(
                                                 
                                                 
                                                 column(width = 3,
                                                        wellPanel(
                                                        
                                                            selectizeInput('by_sport_sex','Sex', choices = unique(data$Sex), multiple = TRUE, selected = c("M","F")),
                                                            selectizeInput('by_sport_sport', 'Sport', choices = c(sort(unique(data$Sport)),"ALL"), multiple=TRUE, selected = c("Basketball", "Gymnastics", "Athletics", "Volleyball")),
                                                            sliderInput("by_sport_year", label = h3("Slider Year"), min = 1986, max = 2016, value = c(1986, 2016), sep = ""),
                                                            checkboxGroupInput('by_sport_medal', 'Medal', choices = c("Gold", "Silver", "Bronze", "Other"), selected = c("Gold", "Silver", "Bronze", "Other")),
                                                            ),
                                                        
                                                        wellPanel(
                                                            selectizeInput('by_sport_sport_s', 'Sport Summer', choices = c(sort(sport_summer),"ALL"), multiple=TRUE, selected = c("Basketball", "Gymnastics", "Athletics", "Volleyball")),
                                                            selectizeInput('by_sport_sport_w', 'Sport Winter', choices = c(sort(sport_winter),"ALL"), multiple=TRUE, selected = c("Curling", "Ski Jumping", "Biathlon", "Luge"))
                                                            )
                                                 ),
                                                 
                                                 
                                                 
                                                 column(width=9,
                                                        tabsetPanel( 
                                                            tabPanel("Taille & Poids", plotlyOutput("bulles")),
                                                            tabPanel("Age", plotlyOutput("density_Age"), div( amChartsOutput("box_Age"),style = "margin-top: 40px;")),
                                                            tabPanel("Evolution", div(plotlyOutput("bulles_summer")), div(plotlyOutput("bulles_winter"), style = "margin-top: 40px;"))
                                                            
                                                                )
                                                 )
                                             )
                                             ),
                          
                          
                          
                          
                          tabPanel("Par Pays",
                                   fluidRow(
                                       
                                       
                                       
                                       column(width = 3,
                                              wellPanel(
                                                  selectizeInput('by_pays_variable', 'Caracteristiques', choices = c("Age", "Height", "Weight"), selected="Age"),
                                                  selectizeInput('by_pays_sex','Sex', choices = unique(data$Sex), multiple = TRUE, selected = c("M","F")),
                                                  selectizeInput('by_pays_sport', 'Sport', choices = sort(unique(data$Sport)), selected = "Athletics"),
                                                  selectizeInput('by_pays_pays', 'Pays', choices = sort(unique(data$region)), multiple=TRUE, selected = c("France", "UK", "Spain")),
                                                  sliderInput("by_pays_year", label = h3("Slider Year"), min = 1986, max = 2016, value = c(2014, 2016), sep = ""),
                                                  checkboxGroupInput('by_pays_medal', 'Medal', choices = c("Gold", "Silver", "Bronze", "Other"), selected = c("Gold", "Silver", "Bronze", "Other"))
                                                  
                                              )
                                       ),
                                       
                                       
                                       
                                       column(width=9,
                                              tabsetPanel(
                                                    tabPanel("Distribution", plotlyOutput("by_pays_density")),
                                                    tabPanel("Hist", plotOutput("dist_plot2"), div(plotOutput("dist_plot_general"), style = "margin-top: 40px;"))
                                              )
                                       )
                                   )
                          )
                        ),

    
              
    
    
    
                tabPanel("Carte", icon = icon("map"),

                         
                         
                         fluidRow(
                             
                             
                             
                             column(width=3,
                                  wellPanel(
                                      checkboxGroupInput('map_medal', 'Medal', choices = c("Gold", "Silver", "Bronze"), selected = c("Gold", "Silver", "Bronze")),
                                      selectizeInput('map_season','Season', choices = c("Summer","Winter"), multiple = TRUE, selected = c("Summer")),
                                      actionButton("button_map", "Resultats !")
                                        )
                                  ),
                             
                             
                             
                             column(width=9,
                                    tabPanel("Map", leafletOutput("Map", width = "100%", height = "750px"))
                                    )
                         
                        )),
    
    
    
    
    
                navbarMenu("Medals", icon = icon("user-friends"),

                           
                           
                         tabPanel("Pays",
                                  fluidRow(
                                      
                                      
                                      
                                      column(width = 3,
                                             wellPanel(
                                                 selectizeInput('medal_pays', 'Pays', choices = sort(unique(data$region)), selected = "France"),
                                                 selectizeInput('medal_sex','Sexe', choices = unique(data$Sex), multiple = TRUE, selected = c("M","F")),
                                                 sliderInput("medal_year", label = h3("Slider Year"), min = 1896, max = 2016, value = c(1896, 2016), sep=""),
                                                 checkboxGroupInput('medal_medal', 'Medal', choices = c("Gold", "Silver", "Bronze"), selected = c("Gold", "Silver", "Bronze")),
                                                 numericInput("medal_top", "Numeric input", value = 10)
                                             )
                                      ),
                                      
                                      
                                      column(width=9,
                                             amChartsOutput("medals_pie"), div(dataTableOutput("table_pays_medals"), style = "margin-top: 40px;")
                                      )
                                      
                                  )
                         ),
                         
                         
                         
                         tabPanel("Versus", 
                                  fluidRow(
                                      
                                      
                                      column(width=3,
                                             wellPanel(
                                                 h4("Variables general"),
                                                 selectizeInput('versus_pays1', 'Pays1', choices = sort(unique(data$region)), selected = "France"),
                                                 selectizeInput('versus_pays2', 'Pays2', choices = sort(unique(data$region)), selected = "UK"),
                                                 selectizeInput('versus_sex','Sexe', choices = c("M", "F"), multiple = TRUE, selected = c("M","F")),
                                                 checkboxGroupInput('versus_medal', 'Medal', choices = c("Gold", "Silver", "Bronze"), selected = c("Gold", "Silver", "Bronze"))
                                                 ),
                                             
                                             wellPanel(selectizeInput('versus_season','Season', choices = c("Summer", "Winter"), selected = "Summer")),
                                             wellPanel(h4("Onglet Sport"),selectizeInput('versus_sport', 'Sport', choices = sort(unique(data$Sport)), selected = "Judo"))
                                      ),
                                      
                                      
                                      
                                      column(width=9,
                                             tabsetPanel(
                                             tabPanel("General", plotlyOutput("line_versus"), plotlyOutput("prop_versus")),
                                             tabPanel("Sport", amChartsOutput("hist_versus"))
                                             )
                                      )
                                  )
                         )
                        ),
    
    
    
    
    
            tabPanel("Données brutes", icon = icon("address-book"),

                     
                        navlistPanel("Data athlete",
                                        tabPanel("Table", dataTableOutput("table_athlete")),
                                        tabPanel("Summary & Str", verbatimTextOutput("summary_athlete"), verbatimTextOutput("str_athlete"))
                                     )
                     )
            
            )
    

   )
)
