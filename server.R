server <- function(input, output, session) {
  
  
  h <- 150
  # height of the bar
  
  cull <- reactive({
    firstData <- 
      filter(df[order(df$attacktype1 == input$Type, decreasing=TRUE)[df$iyear >= input$years[1] & df$iyear <= input$years[2]],])
    if(!is.null(input$regions!=13)){
      
      firstData <- filter(
        firstData,region %in% input$regions
      )
      return(firstData)
    }
    
  })
  
  
  values <- reactive({
    attack <- cull()
    value <- h * attack$attacktype1 /max(attack$attacktype1)
    col <- rainbow(10, start=2.8/5, end=3.4/25)
    names(col) <- c()
    # Extend palette to data values
    col <- col[floor(length(col) * (h - value)/ h)+1]
    list(value=value, color=col ,attack=attack)
  })
  
  filteredData <- reactive({
    Dataset %>% 
      filter(Type %in% input$Type) %>%
      filter(Value >= input$years[1]) %>% 
      filter(Value <= input$years[2]) 
  })
  filteredData2 <- reactive({
    Dataset2 %>% 
      filter(Type1 %in% input$Type) %>%
      filter(Value1 >= input$years[1]) %>% 
      filter(Value1 <= input$years[2]) 
  })
  
  df %>%
    dplyr::group_by(
      country_txt = df$country_txt,
      iyear = df$iyear,
    )%>%
    tally() 
  
  india_df %>%
    dplyr::group_by(
      provstate = india_df$provstate,
      iyear = india_df$iyear,
    )%>%
    tally() 
  
  output$tbl <- DT::renderDataTable(datatable(df[, rep(1:65)]))
  
  output$showData2 <- renderDataTable(
    india_df,
    options = list(
      pageLength = 10,
      scrollY="400p"
      
    )
  )
  
  output$WorldKilledBox <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  
  output$WorldKilledBox2 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  
  output$WorldKilledBox3 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nkill[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nkill[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$WorldInjuredBox3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$nwound[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$countryaffected3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(df$attacktype1[df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(df$attacktype1[ df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  output$KilledBox1 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$InjuredBox1 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$Indiaaffected <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  ###Value box count of people Killed
  output$KilledBox2 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
  output$InjuredBox2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
  
  output$Indiaaffected2 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  ###Value box count of people Killed
  output$KilledBox3 <- renderInfoBox({
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
                  
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nkill[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-KILLS IN INDIA",
                  
                  status = "danger",
                  icon = "skull-crossbones",
                  footer ="" 
      )
    }
    
    
  })
###Value box count of people injured 
  output$InjuredBox3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer =""
                  
      )
    }
    else {
      bs4ValueBox(value = round(sum(india_df$nwound[india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-INJURED IN INDIA",
                  status = "warning",
                  icon = "user-injured",
                  footer ="" 
                  
      )
    }
    
  })
   ###Value box count of attacks
  output$Indiaaffected3 <- renderInfoBox({
    
    if (input$success==2) {
      bs4ValueBox(value = round(sum(india_df$attacktype1[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    else {
      bs4ValueBox(value = round(sum(india_df$attacktype1[ india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]], na.rm=TRUE),0),subtitle = "TOTAL-ATTACKS IN INDIA",
                  status = "success",
                  icon = "radiation-alt",
                  footer ="" 
                  
      )
      
    }
    
  })
  ###Region wise count of attacks
  output$region <- renderPlotly({
    if (input$success==2) {
      y<-data.frame(table(df$region_txt[df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(df$region_txt[df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("blue"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  output$map <- renderLeaflet({
    leaflet(Dataset) %>% addTiles() %>%
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions())
    
    leaflet(Dataset)%>% addTiles() %>% 
      addMarkers(data = filteredData(), clusterOptions = markerClusterOptions(freezeAtZoom = 6))
    leaflet(Dataset) %>%  addTiles() %>% 
      addLabelOnlyMarkers(data = filteredData(),
                          lng = ~Long, lat = ~Lat,
                          label = ~as.character(filteredData()),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(multiple = T,
                                                      direction = "auto"))
  })   
  
  output$piecountry <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$region==input$regions)
      }
      else {#All regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$success==input$success, df$region==input$regions)
      }
      else {
        jk <- df %>% filter(df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    
    a <- data.frame(table(jk$country_txt))
    colnames(a) <- c("COUNTRY","FREQ")
    a <- a[order(a$FREQ, decreasing = TRUE),]
    attack_country <- a[1:10,]
    attack_country
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(attack_country, labels = ~COUNTRY, values = ~FREQ, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Attacks:', FREQ),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
    
    p1  
  })
  
  
  
  output$piecountry2 <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$region==input$regions)
      }
      else {#All regions
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        jk <- df %>% filter(iyear>=input$years[1], iyear<=input$years[2], df$success==input$success, df$region==input$regions)
      }
      else {
        jk <- df %>% filter(df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
      }
    }
    b <- aggregate(nkill ~ country_txt ,data = jk , FUN = sum)
    colnames(b) <- c("COUNTRY","DEATH")
    b <- b[order(b$DEATH, decreasing = TRUE),]
    death_country <- b[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p3 <- plot_ly(death_country, labels = ~COUNTRY, values = ~DEATH, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Fatalities:', DEATH),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p3  
  })
  
  output$pieindia <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      jk <- india_df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
    }
    else {#If input$success are 0 or 1
      jk <- india_df %>% filter(india_df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
    }
    a <- data.frame(table(jk$provstate))
    colnames(a) <- c("STATE","FREQ")
    a <- a[order(a$FREQ, decreasing = TRUE),]
    attack_State <- a[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    
    p1 <- plot_ly(attack_State, labels = ~STATE, values = ~FREQ, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Attacks:', FREQ),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = F)
    
    p1  
  })
  
  output$pieindia2 <- renderPlotly({
    jk<-data.frame()
    if (input$success==2) {
      jk <- india_df %>% filter(iyear>=input$years[1], iyear<=input$years[2])
    }
    else {#If input$success are 0 or 1
      jk <- india_df %>% filter(india_df$success==input$success, iyear>=input$years[1], iyear<=input$years[2])
    }
    
    b <- aggregate(nkill ~ provstate, data = jk, FUN = sum)
    colnames(b) <- c("STATE","DEATH")
    b <- b[order(b$DEATH, decreasing = TRUE),]
    death_state <- b[1:10,]
    
    m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)
    p3 <- plot_ly(death_state, labels = ~STATE, values = ~DEATH, type = 'pie',
                  textposition = 'inside', textinfo = 'label',
                  insidetextfont = list(color = '#FFFFFF'), hoverinfo = 'text',
                  text = ~paste('Total Fatalities:', DEATH),showlegend = FALSE) %>%
      layout(margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      config(displayModeBar = F)
    p3 
  })
  
  output$indiamap <- renderLeaflet({
    leaflet(Dataset2) %>% addTiles() %>%
      addMarkers(data = filteredData2(), clusterOptions = markerClusterOptions())
    
    leaflet(Dataset2)%>% addTiles() %>% 
      addMarkers(data = filteredData2(), clusterOptions = markerClusterOptions(freezeAtZoom = 6))
    leaflet(Dataset2) %>%  addTiles() %>% 
      addLabelOnlyMarkers(data = filteredData2(),
                          lng = ~Long, lat = ~Lat,
                          label = ~as.character(filteredData2()),
                          clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(multiple = T,
                                                      direction = "auto"))
  })   
  
  output$terr_gp_data_by_region <- renderTable({
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        z    <- data.frame(table(df$gname[(df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {#All regions
        z    <- data.frame(table(df$gname[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$region==input$regions &
                                             df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
      else {
        z    <- data.frame(table(df$gname[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
      }
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  ###Weapons used in the attacks worldwide
  output$weapon <- renderPlotly({
    #par(mar=c(14,3,2,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$weaptype1_txt[(df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$weaptype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$region==input$regions &
                                                df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$weaptype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("green"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  ### Types of Attacks : worldwide
  output$attacktype <- renderPlotly({
    # par(mar=c(15,3,4,2))
    if (input$success==2) {
      if (input$regions!=13) {#Specific regions
        y<-data.frame(table(df$attacktype1_txt[(df$region==input$regions & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", 'All attacks\n', sep='')
      }
      else {#All regions
        y<-data.frame(table(df$attacktype1_txt[(df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n", sep="")
      }
    }
    else {#If input$success are 0 or 1
      if (input$regions!=13) {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$region==input$regions &
                                                  df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
      else {
        y<-data.frame(table(df$attacktype1_txt[(df$success==input$success & df$iyear>=input$years[1] & df$iyear<=input$years[2])]))
        title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n", sep="")
      }
    }
    fig1 <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("red"), cliponaxis='FALSE')
    fig1 <- fig1 %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig1
  })
  
  ##### INDIA VISUALISATIONS
  
  ###State wise details for India
  output$India_state_wise <- renderPlotly({
    #par(mar=c(9,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$provstate[india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2]]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("blue"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    ggplotly(fig)
  })
  
  
  ### Types of Attacks : India
  output$India_attacktype <- renderPlotly({
    #par(mar=c(15,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$attacktype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("red"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 180), xaxis = list(tickangle = 45))
    fig
  })
  
  ###Top 10 terror groups in the India
  output$India_terr_gp <- renderTable({
    if (input$success==2) {
      z    <- data.frame(table(india_df$gname[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    else {#If input$success are 0 or 1
      z    <- data.frame(table(india_df$gname[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
    }
    if (nrow(z)!=0) {
      y<-z[!z$Var1=="Unknown",]
      names(y) <- c("Terrorist Groups","Number of Attacks")
      head(y[order(-y$"Number of Attacks"),],10)
    }
    else {
      paste("No data in the selected time period")
    }
  })
  
  ###Weapons used in the attacks in India
  output$India_weapon <- renderPlotly({
    #par(mar=c(14,3,3,2))
    if (input$success==2) {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", "All attacks\n\n", sep="")
    }
    else {
      y<-data.frame(table(india_df$weaptype1_txt[(india_df$success==input$success & india_df$iyear>=input$years[1] & india_df$iyear<=input$years[2])]))
      title_string<-paste(input$years[1], " - ", input$years[2],"\n", ifelse(input$success==0,"Failed Attacks", "Successful Attacks"),"\n\n", sep="")
    }
    fig <- plot_ly(y, x = ~Var1, y = ~Freq, type = 'bar', color=I("green"), cliponaxis='FALSE')
    fig <- fig %>% layout(title = title_string, xaxis=list(title = ''), yaxis = list(title = 'Number of Attacks'),margin = list(b = 160), xaxis = list(tickangle = 45))
    fig
  })
  
  output$globe <- renderGlobe({
    v <- values()
    args <- c(earth_dark, list(lat=v$attack$latitude, long=v$attack$longitude, value=v$value, color=v$color,pointsize = 1,bg ="black",atmosphere=TRUE))
    do.call(globejs, args=args)
    
  })
  
  output$attacks_chi <- renderPlot({ 
    if (nrow(df)!=0) {
      df2<-df[!df$attacktype1_txt=="Unknown",]
    }
    t1 <- table(df2$attacktype1_txt,df2$success)
 
    balloonplot(t(t1), main ="Balloon Plot", xlab ="Success", ylab="Attack Types" ,text.size=1.2,
                label = FALSE, show.margins =FALSE,dotsize=4)
  })
  output$weapon_chi <- renderPlot({ 
    if (nrow(df)!=0) {
      df1<-df[!df$weaptype1_txt=="Unknown",]
    }
    t1 <- table(df1$weaptype1_txt,df1$success)
  
    balloonplot(t(t1), main ="Balloon Plot", xlab ="Success", ylab="Weapon Types" ,text.size=1.2,
                label = FALSE, show.margins =FALSE,dotsize=4)
  })
  
  
  
  
  
  output$tree <- renderD3tree2({fruitTree <- d3tree(treemap(df,
                                                            index = c("country_txt","iyear"), type = "value",
                                                            vSize = "nkill",vColor="nwound" ,palette = c("#FFFFFF","#FFFFFF","#CC0000")
  ), rootname = "World")
  })
  
  output$indiatree=renderD3tree2({
    
    fruitTree <- d3tree(treemap(india_df,
                                index = c("provstate","iyear"), type = "value",
                                vSize = "nkill",vColor="nwound" ,palette = c("#FFFFFF","#FFFFFF","#CC0000")
    ), rootname = "india")
  })
}


shinyApp(ui, server)