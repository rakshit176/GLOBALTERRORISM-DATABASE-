library(leaflet)
library(threejs)
library(png)
library(dplyr)
library(treemap)
library(d3treeR)
library(datasets)
library(rstatix)
library(dslabs)
library(tidyverse)
library(gplots)
library(readxl)
earth_dark <- list(img=system.file("images/world.jpg", package="threejs"),
                   bodycolor="#ffffff",
                   emissive="#000000",
                   lightcolor="#ffffff")
#df <- RGP3
#india_df<-data.frame()
#for (i in 1:nrow(df)){
#if (df$country[i]==92 & df$provstate[i]!="Unknown"){
#india_df <- rbind(india_df,df[i,])
#}
#}


Dataset <- data.frame(
  Type = c(df$attacktype1),
  Value = c(df$iyear),
  Lat = c(df$latitude), 
  Long = c(df$longitude)
)
Dataset2 <- data.frame(
  Type1 = c(india_df$attacktype1),
  Value1 = c(india_df$iyear),
  Lat = c(india_df$latitude), 
  Long = c(india_df$longitude)
)




# Theme -------------------------------------------------------------------

bs4DashTheme <- create_theme(
  bs4dash_vars(
    navbar_dark_color = "#bec5cb",
    navbar_dark_active_color = "#FFF",
    navbar_dark_hover_color = "#FFF"
  ),
  bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30"),
  bs4dash_layout(main_bg = "#353c42"),
  bs4dash_sidebar_dark(
    bg = "#272c30", color = "#bec5cb", hover_color = "#FFF",
    submenu_bg = "#272c30", submenu_color = "#FFF", submenu_hover_color = "#FFF"
  ),
  bs4dash_status(dark = "#272c30"),
  bs4dash_color(gray_900 = "#FFF", white = "#272c30")
)

rock ="green"

# App ---------------------------------------------------------------------


ui <- bs4DashPage(
  title = "GTD DASHBOARD",
  # sidebar_collapsed = FALSE,
  navbar = bs4DashNavbar(skin = "dark"),
  controlbar = bs4DashControlbar(
    skin = "dark",
    "This is the control bar"
  ),
 
  sidebar = bs4DashSidebar(
    title = "GTD DASHBOARD",
    skin = "dark",
    bs4SidebarMenu(
      bs4SidebarHeader("Menu:"),
      bs4SidebarMenuItem(
        tabName = "home",
        text = "DESCRIPTION",
        icon = "info-circle"
      ),
      bs4SidebarMenuItem(
        tabName = "data",
        text = "WORLD-DATA SET",
        icon = "database"
      ),
      bs4SidebarMenuItem(
        tabName = "tab1",
        text = "WORLD DATA",
        icon = "globe"
      ),
      bs4SidebarMenuItem(
        tabName = "tab2",
        text = "INDIA DATA",
        icon = "flag"
      ),
      bs4SidebarMenuItem(
        tabName = "tab4",
        text = "CONCLUSION",
        icon = "stop"
      ),
      bs4SidebarMenuItem(
        tabName = "tab3",
        text = " 3D GLOBE",
        icon = "cube"
      ),
      bs4SidebarMenuItem(
        "Success/Failure", tabName="Success/Failure", icon="list-ul",
        radioButtons("success", label = h4(""),
                     choices = list("Failed attacks" = 0, "Successful Attacks" = 1, "All Attacks" = 2), 
                     selected = 2)
      ),
      bs4SidebarMenuItem(
        "Select the time period", tabName="Select the time period", icon="sliders-h",
        sliderInput(inputId = "years",
                    label = "Years:",
                    min = 1970,
                    max = 2017,
                    value =  c(1970,1990),
                    width = 200)
      ),
      
      
      bs4SidebarMenuItem(
        "World", tabName="World", icon="globe-asia",
        radioButtons("regions", label = h3("Regions"),
                     choices = list("North America" = 1, "Central America & Caribbean" = 2, 
                                    "South America" = 3, "East Asia" = 4, "Southeast Asia" = 5, 
                                    "South Asia" = 6, "Central Asia"=7, "Western Europe" = 8, 
                                    "Eastern Europe" = 9, "Middle East & North Africa" = 10, 
                                    "Sub-Saharan Africa"= 11, "Australasia & Oceania"= 12, 
                                    "All Regions" = 13 ), 
                     selected = 1)
      ),
      
      bs4SidebarMenuItem(
        "Attack Types", tabName="Attack Types", icon="jedi",
        radioButtons( "Type", "Type", choices = c("Assassination" = 1,
                                                  "Armed Assault" = 2,
                                                  "Bombing/Explosion" = 3,
                                                  "Hijacking" = 4,
                                                  "Hostage Taking (Barricade Incident)" = 5,
                                                  "Hostage Taking (Kidnapping)" = 6,
                                                  "Facility/Infrastructure Attack" = 7,
                                                  "Unarmed Assault" = 8,
                                                  "Unknown" = 9 
        ), selected = 1 )
      )
    )
  ),
  
  body = bs4DashBody(
    
    use_theme(bs4DashTheme),
    
    bs4TabItems(
      bs4TabItem(
        tabName = "home",
        mainPanel(absolutePanel(width = 20,bottom = 1, right = -150, top = -100,
                                img(src = "https://i.ibb.co/TPJkFXT/GTD.png" , height = 1000, width = 400)
        ),
        h1(span(strong("The Global Terrorism Database", style = "color:red"))),
        h3(p("The Global Terrorism Database (GTD) is the most comprehensive unclassified database of terrorist attacks in the world. 
            The National Consortium for the Study of Terrorism and Responses to Terrorism (START) makes the GTD available via", 
             strong(em("https://www.start.umd.edu/gtd/ ")),
             "in an effort to improve understanding of terrorist violence, so that it can be more readily studied and defeated. 
            The GTD is produced by a dedicated team of researchers and technical staff."),align ="justify"),
        br(),
        h3("The GTD is an open-source database, which provides information on domestic and international terrorist attacks around the 
            world since 1970, and includes more than 180,000 events. For each event, a wide range of information is available, 
            including the date and location of the incident, the weapons used, nature of the target, the number of casualties, 
            and - when identifiable - the group or individual responsible. ", align ="justify"), 
        
        br(),
        h2(span(strong("Characteristics of the GTD ", style = 'color:red'))),
        tags$ul(
          tags$li(h3("Contains information on over 180,000 terrorist attacks.")),
          p(""),
          tags$li(h3("Currently the most comprehensive unclassified database on terrorist attacks in the world.",align ="justify")),
          p(""),
          tags$li(h3("Includes information on more than 95,000 bombings, 20,000 assassinations, and 15,000 kidnappings and hostage 
            events since 1970.")),
          p(""),
          tags$li(h3("Includes information on at least 45 variables for each case, with more recent incidents including information on 
            more than 120 variables.",align ="justify")),
          p(""),
          tags$li(h3("More than 4,000,000 news articles and 25,000 news sources were reviewed to collect incident data from 1998 to 2017 alone.",align ="justify")),)
        )#mainPanel
      ),
      
      #tabitem
      bs4TabItem(
        tabName = "data",
        tags$h4("DATA-SET"),
        fluidRow(
          bs4Card(
            title = "",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            gradientColor = "warning",
            collapsible = TRUE,
            div(DT::dataTableOutput("tbl"), style = 'overflow-x: auto'))
          
        )
        
      ),
      bs4TabItem(
        tabName = "tab1",
        bs4TabSetPanel(id = "tab", side = "center",
                       bs4TabPanel(tabName = "WORLD PANEL-1",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox",width = 4),
                                            bs4ValueBoxOutput("countryaffected",width = 4)
                                   ),
                                   tags$h4("MAP PLOT"),
                                   fluidRow(
                                     bs4Card(
                                       title = "WORLD MAP : NO OF ATTACKS ",
                                       closable = FALSE,
                                       width = 12,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       leafletOutput(outputId = "map",width ="100%")
                                     )
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "TOTAL ATTACKS : REGION WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "piecountry")
                                     ),
                                     bs4Card(
                                       title = "TOP 10 TERRORIST GROUPS : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       tableOutput(outputId = "terr_gp_data_by_region")
                                     )
                                   )) ,   
                       
                       bs4TabPanel(tabName = "WORLD PANEL-2",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox2",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox2",width = 4),
                                            bs4ValueBoxOutput("countryaffected2",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "FATALITIES : REGION WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "piecountry2")
                                     ),
                                     bs4Card(
                                       title = "REGION WISE COUNT : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "region")
                                     ),
                                     bs4Card(
                                       title = "TYPES OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "attacktype")
                                     ),
                                     bs4Card(
                                       title = "WEAPONS/METHODS USED",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       plotlyOutput("weapon")
                                     )
                                   )
                                   
                       ),
                       
                       bs4TabPanel(tabName = "WORLD PANEL-3",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("WorldKilledBox3",width = 4),
                                            bs4ValueBoxOutput("WorldInjuredBox3",width = 4),
                                            bs4ValueBoxOutput("countryaffected3",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "WORLD : TREE MAP",
                                       closable = FALSE,
                                       width = 8,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       d3tree2Output("tree")
                                     )
                                   )))),
      

      bs4TabItem(
        tabName = "tab2",
        bs4TabSetPanel(id = "tabs", side = "center",
                       bs4TabPanel(tabName = "INDIA PANEL-1",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox1",width = 4),
                                            bs4ValueBoxOutput("InjuredBox1",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected",width = 4)
                                   ),
                                   tags$h4("MAP PLOT"),
                                   fluidRow(
                                     bs4Card(
                                       title = "MAP : NO OF ATTACKS",
                                       closable = FALSE,
                                       width = 12,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       leafletOutput(outputId = "indiamap",width ="100%")
                                     )
                                   ),
                                   
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "FATALITIES : STATE WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "pieindia2")
                                     ),
                                     bs4Card(
                                       title = "TOP 10 TERRORIST GROUPS: NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       tableOutput(outputId = "India_terr_gp")
                                     )
                                   )) ,   
                       
                       bs4TabPanel(tabName = "INDIA PANEL-2",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox2",width = 4),
                                            bs4ValueBoxOutput("InjuredBox2",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected2",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "TOTAL ATTACKS : STATE WISE",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "pieindia")
                                     ),
                                     bs4Card(
                                       title = "INDIAN STATEWISE COUNT : NUMBER OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "India_state_wise")
                                     ),
                                     bs4Card(
                                       title = "TYPES OF ATTACKS",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "danger",
                                       collapsible = TRUE,
                                       plotlyOutput(outputId = "India_attacktype")
                                     ),
                                     bs4Card(
                                       title = "WEAPONS/METHODS USED ",
                                       closable = FALSE,
                                       width = 6,
                                       solidHeader = TRUE,
                                       status = "success",
                                       collapsible = TRUE,
                                       plotlyOutput("India_weapon")
                                     )
                                   )
                       ),
                       
                       bs4TabPanel(tabName = "INDIA PANEL-3",
                                   tags$h2("THE GLOBAL TERRORISM DATABASE"),
                                   tags$h4(""),
                                   fluidRow(bs4ValueBoxOutput("KilledBox3",width = 4),
                                            bs4ValueBoxOutput("InjuredBox3",width = 4),
                                            bs4ValueBoxOutput("Indiaaffected3",width = 4)
                                   ),
                                   tags$h4(""),
                                   fluidRow(
                                     bs4Card(
                                       title = "INDIA:TREE MAP",
                                       closable = FALSE,
                                       width = 8,
                                       solidHeader = TRUE,
                                       status = "primary",
                                       collapsible = TRUE,
                                       d3tree2Output("indiatree")
                                     )
                                   )
                                   
                       ))),
      bs4TabItem(
        tabName = "tab3",
        fluidRow(
          bs4Card(
            title = "3D GLOBE",
            closable = FALSE,
            width = 12,
            height = 12,
            solidHeader = TRUE,
            status = "warning",
            collapsible = TRUE,
            globeOutput("globe")
          )
        )
        
      ),
      bs4TabItem(
        tabName = "tab4",
        fluidRow( 
        bs4Card(
          title = "",
          closable = FALSE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          h2(span("CHI-SQUARE TEST OF INDEPENDENCE", style = 'color:red')),
          tags$ul(
            h3("The" ,strong ("Chi-square test of independence ")," compares two variables in a contingency table to see if they are related. In a more general sense, it tests to see whether distributions of categorical variables differ from each another. ", align ="justify"),
            p(""),
            h3("The Chi-square test of independence is used to analyse the frequency table (i.e. contingency table) formed by two categorical variables.",align ="justify"),
            br(),
            h3("The chi-square statistic is represented by the expression - "),
            img(src = "https://i.ibb.co/17zh7ZJ/Picture9.png" , height = 100, width = 400),
            br(),
            br(),
            h3("C is the Degrees of Freedom, Oi s are the Observed Frequencies and Ei s are the Expected Frequencies [Degree of freedom = (No of rows - 1)*(no of columns -1)].",align ="justify"),
            br(),
            h3(strong ("Null hypothesis:"),"There is no relationship between the categorical variables.  "),
            p(""),
            h3(strong ("Alternative hypothesis:")," There is a relationship between the categorical variables."),
            br(),
            h3("Here, the Level of Significance (alpha) is important. "),
            br(),
            h3("If p-value <= alpha : Significant result -> reject null hypothesis (H0)"),
            p(""),
            h3("If p-value > alpha : Not a significant result -> accept null hypothesis (H0)"),
           br(),
           br(),
            h2(span("(I) Chi-Square test of Independence for Attack Types and Success" , style = "color:red")),
             p(""),
            h3(strong("H0:"), "Attack types and success are independent."),
            p(""),
            h3(strong("H1:"), "Attack types and success are not independent."),
           br(),
           br(),
            h2(span("Contingency Table for Attack Type and Success" , style = "color:red")),
            img(src = "https://i.ibb.co/7CwtfHL/picture12.jpg" , height = 350, width = 700),
           br(),
           br(),
             h2(span("Chi-Square test", style = "color:red")),
            img(src = "https://i.ibb.co/chFwjX5/picture15.jpg" , height = 150, width = 900),
            h3(strong("The P-value (2.2e-16) is less than the significance level (0.05). Therefore, we reject the null hypothesis. ")),
           br(),
           br(),
            h2(span("(II) Chi-Square test of Independence for Weapon types and success", style = "color:red" )),
            p(""),
            h3(strong("H0:"), "Weapon types and success are independent. "),
            p(""),
            h3(strong("H1:"), " Weapon types and success are not independent. "),
           br(),
           br(),
            h2(span("Contingency Table for Weapon type and Success", style = "color:red")),
            img(src = "https://i.ibb.co/DLNWXvG/picture13.jpg" , height = 400, width = 700),
           br(),
           br(),
            h2(span("Chi-Square test" , style = "color:red")),
            img(src = "https://i.ibb.co/52HStV2/picture14.jpg" , height = 150, width = 900),
            p(""),
            h3(strong("The P-value (2.2e-16) is less than the significance level (0.05). Therefore, we reject the null hypothesis. ")),
           br(),
           br(),
           h3(strong("Contingency table has been visualised using balloonplot from the gplots package. ", align ="justify")),
        )),
        bs4Card(
          title = "ATTACK  TYPES  VS  SUCCESS",
          closable = FALSE,
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          plotOutput(outputId = "attacks_chi")
        ),
        bs4Card(
          title = "WEAPON  TYPES  VS  SUCCESS",
          closable = FALSE,
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          plotOutput(outputId = "weapon_chi")
        )
          
          
          ))
      
    )
    
  )
)