
shinyServer(function(input, output) {
    
    
    
    ###################################################### onglet JO
    ########### onglet Participant
    # carte
    output$jo_carte <- renderPlot({
        
        p <- data %>% filter(Sex %in% input$JO_sex, Year>=input$JO_year[1], Year<=input$JO_year[2], Season %in% input$JO_season) %>% 
                group_by(NOC) %>% summarise("Nb_participant"=n())
        
        p <- left_join(p, world, by="NOC")
        st_geometry(p) <- p$geometry
        p <- st_transform(p, crs = 4326)
        
        ggplot(p)+geom_sf(aes(fill=Nb_participant)) + scale_fill_gradient(low="yellow", high="red") + labs(title = "Nombre de participants par pays") +
            theme(plot.title = element_text(color = "#E95421", size = 17, face = "bold", hjust = 0.5))
    })
    
    # plot
    JO_p <- reactive({ 
        
        data %>% filter(Year>=input$JO_year[1], Year<=input$JO_year[2], Sex %in% input$JO_sex) %>% 
                group_by(Year,Season) %>%  summarise("Nombre d'athletes"=n_distinct(ID)) 
        })
    
    
    output$jo_dist <- renderPlotly({
        
        p <- JO_p() %>% ggplot()+ aes(x=Year,y=`Nombre d'athletes`, color=Season) + geom_point(shape=20) + geom_line() + 
            labs(title = "Evolution du nombre de participants") + theme(plot.title = element_text(color = "#E95421", size = 12, face = "bold", hjust = 0.5))
        ggplotly(p)
    })
    
    
    output$jo_bar <- renderPlotly({
        
        p <- ggplot(JO_p())+aes(x=Year, y=`Nombre d'athletes`, fill=Season) + geom_bar(stat = "identity", position = "stack")  + 
            labs(title = "Evolution du nombre de participants") + theme(plot.title = element_text(color = "#E95421", size = 12, face = "bold", hjust = 0.5))
        ggplotly(p)
    })
    
    
    
    ########### onglet Villes
    # Map 
    output$jo_ville <- renderLeaflet({
        
        ville_geo %>%  leaflet() %>% setView(lat=45, lng=0,zoom = 1) %>% addProviderTiles("Esri.WorldGrayCanvas") %>% 
            addMarkers(~lon, ~lat, popup =~paste(City,"<br>","AnnÃ©es :",Year,"<br>",lon,lat, sep = " "), label = ~City, icon = leafIcons)
    })
    
    # Table
    output$table_ville <- renderDataTable({ 
        
        datatable(ville_geo[,-1], caption = htmltools::tags$caption( style = 'caption-side: bottom; text-align: center; color: black','Table :', 
                                                                     htmltools::em('Table des villes des Jeux olympiques')), 
                  rownames = FALSE)
    })
    
    
    
    ########### onglet Sport
    # Treemap
    output$jo_carre <- renderPlot ({
        
        var <- unlist(str_split(input$JO_games," "))
        p <- data %>% filter(Season==var[2], Year==as.numeric(var[1])) %>% group_by(Sport) %>%  summarise("Nombre d'atheltes"=n())
        p %>% ggplot( aes(area = `Nombre d'atheltes`, fill=`Nombre d'atheltes`, label = Sport))+ scale_fill_gradient(low="lightgrey", high="red") + 
             geom_treemap() + geom_treemap_text(fontface = "italic", colour = "black", place = "centre") + 
            labs(title = paste("Repartiton des participants par Sport au jeux",var[2], var[1], sep=" ")) +
            theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5), legend.position="left")
        
    })
    
    
    output$nb_sport <- renderPrint({
        
        var <- unlist(str_split(input$JO_games," "))
        p <- data %>% filter(Season==var[2], Year==as.numeric(var[1])) %>% summarise("nb"=n_distinct(Sport)) %>%  as.numeric()
        p
    })
    
    # Table 
    output$table_sport_ancien <- renderDataTable({
        
        l <- data %>% filter(Year %in% c(2016,2014)) %>% select(Sport) %>%  unique()
        d <- data %>%  filter(!Sport %in% l$Sport) %>% group_by(Sport, Season) %>% summarise("last_date" = max(Year))
        
        datatable(d, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center; color: black',
                                                        'Table :', htmltools::em('Table des Sports disparus aux Jeux olympiques')
        ), rownames = FALSE)
        
    })
    
    
    
    
    
    
    ###################################################### onglet athlete
    ########### onglet Par sport
    # variable
    var_by_sport <- reactive({ 
        
        p <- data %>% filter(Year>=input$by_sport_year[1], Year<=input$by_sport_year[2], Medal  %in% input$by_sport_medal, Sex  %in% input$by_sport_sex)
        if(!"ALL" %in% input$by_sport_sport){
            p <- p %>% filter(Sport %in% input$by_sport_sport)
        }
        p
    })
    

    
    # Bulle poid et taille
    output$bulles <- renderPlotly({
        
        p <- var_by_sport() %>% group_by(Sport) %>% summarise(nb = n(), moy_taille=mean(Height, na.rm = TRUE), moy_poid= mean(Weight, na.rm = TRUE))
        mu.p <-  data %>%  summarise(grp.mean=mean(Weight, na.rm=TRUE))
        mu.h <-  data %>%  summarise(grp.mean=mean(Height, na.rm=TRUE))
        
        p <- ggplot(p)+aes(x=moy_taille, y=moy_poid) + geom_point(aes(size=nb, color=Sport, alpha=1/10))+ 
            geom_vline(data=mu.h, aes(xintercept=grp.mean, alpha=1/10), linetype="dashed") + geom_hline(data=mu.p, aes(yintercept=grp.mean, alpha=1/10), linetype="dashed")+
            guides(color=FALSE,size=FALSE, alpha=FALSE) + 
            scale_size_continuous(range = c(5, 35)) + scale_x_continuous(limits = c(150, 200)) + scale_y_continuous(limits = c(50, 100)) + 
            labs(title = "Poids et tailles moyennes par Sport") + 
            theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5), legend.position = "none")
        
        ggplotly(p)
        
    })
    
    
    
    # distribution Age 
    output$density_Age <- renderPlotly({
        
        if("ALL" %in% input$by_sport_sport){
            p <- var_by_sport() %>% ggplot() + aes(x=Age) + geom_density()
        }else{
            mu <-  var_by_sport() %>%  group_by(Sport) %>%  summarise(grp.mean=mean(Age, na.rm=TRUE))
            p <- var_by_sport() %>% ggplot() + aes(x=Age, color=Sport, frame = c()) + geom_density() + geom_vline(data=mu, aes(xintercept=grp.mean, color=Sport), linetype="dashed") + labs(title = "Distribution de l'Age par Sport") +
                theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5))
        }
        ggplotly(p)
    })
    
    
    output$box_Age <- renderAmCharts({
        
        if("ALL" %in% input$by_sport_sport){
            amBoxplot(var_by_sport()$Age, horiz = TRUE, theme='dark', ylab="Age", main = "Boxplot de l'Age en fonction des Sport", col = "#CD1076",
                      mainColor = "#E95421", mainSize = 20, creditsPosition = "top-right")
        }else{
            amBoxplot(Age~Sport, data=var_by_sport(), horiz = TRUE, theme='dark', ylab="Age", main = "Boxplot de l'Age en fonction des Sport", col = "#CD1076",
                      mainColor = "#E95421", mainSize = 20, creditsPosition = "top-right")
        }
        
    })
    
    
    
    # Bulle animée
    output$bulles_summer <- renderPlotly({
        
        if(!"ALL" %in% input$by_sport_sport_s){
            p <- data %>% filter(Sport %in% input$by_sport_sport_s)
            t1 <- as_tibble(data.frame(Year=1895, Sport=input$by_sport_sport_s, nb=NaN, moy_taille=NaN,moy_poid=NaN))
        }else{ 
            p <- data %>% filter(Season=="Summer")
            t1 <- as_tibble(data.frame(Year=1895, Sport=sport_summer, nb=NaN, moy_taille=NaN,moy_poid=NaN))
            }
        
        p <- p %>% group_by(Sport, Year) %>% summarise(nb = n(), moy_taille=mean(Height, na.rm = TRUE), moy_poid= mean(Weight, na.rm = TRUE))
        p <- bind_rows(t1,p)
        
        gg <- ggplot(p)+ aes(x=moy_taille, y=moy_poid) + geom_point(aes(size = nb, color=Sport, alpha=1/10, frame = Year)) +
            scale_size_continuous(range = c(5, 35)) + scale_x_continuous(limits = c(150, 200)) + scale_y_continuous(limits = c(50, 100)) + 
            labs(title ="Poids et tailles moyennes par Sport d'Ã©tÃ©") + 
            theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5), legend.position = "none")
        
        ggplotly(gg)
    })
    
    
    output$bulles_winter <- renderPlotly({
        
        if(!"ALL" %in% input$by_sport_sport_w){
            p <- data %>% filter(Sport %in% input$by_sport_sport_w)
            t1 <- as_tibble(data.frame(Year=1923, Sport=input$by_sport_sport_w, nb=NaN, moy_taille=NaN,moy_poid=NaN))
        }else{ 
            p <- data %>% filter(Season=="Winter")
            t1 <- as_tibble(data.frame(Year=1923, Sport=sport_winter, nb=NaN, moy_taille=NaN,moy_poid=NaN))
        }
        
        p <- p %>% group_by(Sport, Year) %>% summarise(nb = n(), moy_taille=mean(Height, na.rm = TRUE), moy_poid= mean(Weight, na.rm = TRUE))
        p <- bind_rows(t1,p)
        
        gg <- ggplot(p)+ aes(x=moy_taille, y=moy_poid) + geom_point(aes(size = nb, color=Sport, alpha=1/10, frame = Year)) +
            scale_size_continuous(range = c(5, 35)) + scale_x_continuous(limits = c(150, 200)) + scale_y_continuous(limits = c(50, 100)) + 
            labs(title = "Poids et tailles moyennes par Sport d'hiver") + 
            theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5), legend.position = "none")
        
        ggplotly(gg)
    })
    
    
    
    ########### onglet Par pays 
    # variable 
    var_by_pays <- reactive({
        
        var <- data %>% filter(Year>=input$by_pays_year[1], Year<=input$by_pays_year[2], Medal  %in% input$by_pays_medal, 
                               Sex  %in% input$by_pays_sex, Sport  %in% input$by_pays_sport, region %in% input$by_pays_pays)
        var <- var %>% mutate("Target"=var[[input$by_pays_variable]])
        
        var
    })
    
    # distribution
    output$by_pays_density <- renderPlotly({
        
        g <-  var_by_pays() %>% ggplot() + aes(x=Target, color=region) + geom_density() + geom_density(data = var_by_pays(), aes(x=Target, color="General")) + 
                labs(title = paste("Distribution de la variable",input$by_pays_variable,"des athletes", sep=" ")) +
                theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5)) 
        
        ggplotly(g)
    })
    
    # Histogramme
    output$dist_plot2 <- renderPlot({

        var_by_pays() %>% ggplot()+ aes(x=Target, fill=Sex) + geom_histogram(position="identity", alpha=0.5) + facet_wrap(~region, ncol = 2) + 
            labs(title = paste("Histograme de la variable",input$by_pays_variable,"des athletes", sep=" ")) +
            theme(plot.title = element_text(color = "#E95421", size = 15, face = "bold", hjust = 0.5))
        
    })
    
    
    output$dist_plot_general <- renderPlot({
        
        var_by_pays() %>% ggplot()+ aes(x=Target, fill=Sex) + geom_histogram(position="identity", alpha=0.5) +
            labs(title = "Histograme de l'ensemble des pays") +
            theme(plot.title = element_text(color = "#E95421", size = 13, face = "bold", hjust = 0.5))
    })
    
    
    
    
    
    
    
    
    ####################################################### onglet medals
    # onglet 
    # Camenbert
    output$medals_pie <- renderAmCharts({
        
        p <- data %>% filter(Year>=input$medal_year[1], Year<=input$medal_year[2], Medal  %in% input$medal_medal, Sex  %in% input$medal_sex, 
                             region == input$medal_pays) %>% 
                group_by(Sport) %>% summarise("Nb medals"= n()) %>% arrange(desc(`Nb medals`))
        
        colnames(p) <- c("label","value")
        p <- as.data.frame(p)
        
        if(input$medal_top < nrow(p)){
            p <- as.data.frame(p[1:input$medal_top,])
        }
        
        amPie(data = p, export = TRUE, show_values = TRUE, legend = TRUE, legendPosition = "bottom", legendAlign = "center", 
                main= paste("Top",input$medal_top,"des Sports en ",input$medal_pays, sep=" "), mainColor = "#E95421", mainSize = 20)
        
    })
    
    output$table_pays_medals <- renderDataTable({
        
        p <- data %>% filter(Medal  %in% input$medal_medal, Sex  %in% input$medal_sex, region == input$medal_pays) %>% 
                group_by(Sport) %>% summarise("Nb medals"= n()) %>% arrange(desc(`Nb medals`))
        
        datatable(p, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center; color: black',
                                                       'Table :', htmltools::em(paste('Table des mÃ©dailles par sports du pays',input$medal_pays, sep = " "))
            ), rownames = FALSE)
        
    })
    
    
    
    # onglet Versus 
    # histograme vs
    output$hist_versus <- renderAmCharts({
        
        p <- data %>% filter(Medal  %in% input$versus_medal, Sex  %in% input$versus_sex, Sport  %in% input$versus_sport, 
                             region %in% c(input$versus_pays1, input$versus_pays2))
        p <- p %>% group_by(region, Sex) %>% summarise("Nb medals"= n()) %>% pivot_wider(names_from = "Sex", values_from = "Nb medals") %>% as.data.frame()
        
        if(length(rownames(p))==1){
            rownames(p)=p$region
        }else{
            rownames(p) <- c(input$versus_pays1, input$versus_pays2)
        }
        amBarplot ( y = c("M","F"), data = p, stack_type = "regular", main = paste("Nombre de médailles : ",input$versus_sport), col = "#CD1076", mainColor = "#E95421", mainSize = 20)
    })
    
    
    # times series
    output$line_versus <- renderPlotly({
        
        p <- data %>% filter(Medal  %in% input$versus_medal, Sex  %in% input$versus_sex, region %in% c(input$versus_pays1, input$versus_pays2), 
                             Season == input$versus_season) %>% group_by(region, Year) %>% summarise("Nb medals"= n())
        p <- p %>% ggplot()+ aes(x=Year,y=`Nb medals`, color=region) + geom_line(size=1) + labs(title = paste("Nombre de médailles :", input$versus_pays1," vs ",input$versus_pays2)) +
            theme(plot.title = element_text(color = "#E95421", size = 13, face = "bold", hjust = 0.5))
        
        ggplotly(p)
    })
    
    
    # histogramme proportion
    output$prop_versus <- renderPlotly({
        
        p1 <- data %>% filter(Sex  %in% input$versus_sex, region %in% c(input$versus_pays1, input$versus_pays2), Season == input$versus_season) %>% 
                group_by(region, Year) %>% summarise("Nb athlete"= n())
        
        p2 <- data %>% filter(Medal  %in% input$versus_medal, Sex  %in% input$versus_sex, region %in% c(input$versus_pays1, input$versus_pays2),
                              Season == input$versus_season) %>% group_by(region, Year) %>% summarise("Nb medals"= n())
        
        prop <- left_join(p2,p1, by = c("region","Year"))
        prop <- prop %>% mutate("% de medals"= `Nb medals`/`Nb athlete`)
        
        p <- prop %>% ggplot()+aes(x=Year, y = `% de medals`, fill=region)+ geom_bar(position= "dodge", stat="identity") + 
            labs(title = "Proportion de médailles gagnées par rapport au nombre de participants") +
            theme(plot.title = element_text(color = "#E95421", size = 12, face = "bold", hjust = 0.5))
        ggplotly(p)
        
    })
    
    
    
    
    
    ####################################################### onglet cartes
    
    output$Map <- renderLeaflet({
        
        input$button_map
        isolate({
            
            p <- data %>% filter(Medal %in% input$map_medal, Season %in% input$map_season) %>% group_by(NOC, region) %>% summarise("Nb Medals"=n())
            p <- left_join(p, world, by="NOC")
            st_geometry(p) <- p$geometry
            p <- st_transform(p, crs = 4326)

            bins <- c(0,100,200,400,600,1000,1500,2000,Inf)
            pal <- colorBin("viridis", domain = p$`Nb Medals`, bins = bins)

            leaflet(p) %>% setView(lat=45, lng=0,zoom = 2) %>% addProviderTiles("CartoDB.VoyagerNoLabels") %>%
                addPolygons(data=p, fillColor = ~pal(`Nb Medals`), weight = 0.8, opacity = 0.7,
                            color = "black", fillOpacity = 0.7, popup = ~paste(NOC,":",`Nb Medals`,"medailles", sep = " "),
                            highlightOptions = highlightOptions(color = "white", weight = 2)) %>%
                addLegend("bottomright", pal = pal, values = ~`Nb Medals`, title = "Nb medals", opacity = 0.9) %>% 
                addEasyButton(easyButton( icon="fa-globe", title="Origine", onClick=JS("function(btn, map){ map.setZoom(2); }")))
        })
    })
    
    
    ####################################################### onglet donnÃ©es brutes
    
    # tables 
    output$table_athlete <- renderDataTable({ 
        
        datatable(athlete, caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center; color: black',
                                                             'Table :', htmltools::em('Jeux de donnÃ©es')
        ), rownames = FALSE) 
    })
    
    
    
    # summary 
    output$summary_athlete <- renderPrint({ summary(data) })
    
    # str 
    output$str_athlete <- renderPrint({ str(data) })

})
