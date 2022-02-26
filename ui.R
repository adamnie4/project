library(shiny)
library(plotly)
shinyUI(fluidPage(

  titlePanel("Adam Niewiadomski),

  sidebarLayout(

    sidebarPanel(

     # textInput("sqlQueryInput",
     #   label = "Zapytanie SQL",
     #   value = "select * from sqlie_master"    
     #   )
            
    ),

    mainPanel(
      tabsetPanel(type = "tabs",
        
          tabPanel("Pobieranie danych",
               HTML(paste0("<hr>Pobierz dane ze strony GUS:<hr>")),
               actionButton(inputId = "open_page_GUS", label = "Otworz polaczenie http"),
               HTML(paste0("<hr>a teraz<hr>")),
               actionButton(inputId = "gus_baza", label = "Stworz baze danych"),

               HTML(paste0("<hr>Pobierz dane ze strony EUROSTAT:<hr>")),
               actionButton(inputId = "open_page_EUROSTAT", label = "Otworz polaczenie http"),
               HTML(paste0("<hr>a teraz<hr>")),
               actionButton(inputId = "eur_baza", label = "Stworz baze danych"),
          ),
        
          tabPanel("SQL",
                   actionButton(inputId = "guziksql", label = "Stworz baze danych sql"),
                   textInput("sqlQueryInput",
                             label = "Zapytanie SQL",
                             value = "select * from eurostat_db"),
                   actionButton(inputId = "zapytanie", label = "wykonaj"),
                   verbatimTextOutput("plainSQLText"),
                   tabPanel("Tabela", DT::dataTableOutput("tabelka")),
                   
          ),
          
          tabPanel("Mapa GUS",
                   selectInput("wybor_plec_mgus",label = "Plec",
                               choices = as.vector(as.character(c("Ogolem","Kobiety","Mezczyzni")),mode="list")),
                   selectInput("wybor_wiek_mgus",label = "Kategoria wiekowa",
                               choices = as.vector(as.character(c("0 - Inf","00 - 04","05 - 09","10 - 14","15 - 19","20 - 24","25 - 29","30 - 34","35 - 39","40 - 44","45 - 49","50 - 54","55 - 59","60 - 64","65 - 69","70 - 74","75 - 79","80 - 84","85 - 89","90 - Inf")),mode="list")),
                   HTML(paste0("<hr>Okres mapka bezposrednia:<hr>")),
                   textInput("mgusbezod",label = "Od",
                             value = "2020-01-01"),
                   textInput("mgusbezdo",label = "Do",
                             value = "2021-12-01"),
                   HTML(paste0("<hr>Okres mapka wzgledna:<hr>")),
                   textInput("mguswzod",label = "Od",
                             value = "2015-01-01"),
                   textInput("mguswzdo",label = "Do",
                             value = "2019-12-31"),
                   column(10,actionButton(inputId = "guzikmgus", label = "wykonaj")),
                   column(5,htmlOutput("mapkagusfinalbez")),
                   column(5,htmlOutput("mapkagusfinalwz"))
                   
                   ),
          tabPanel("Mapa Eurostat",
                   selectInput("plecmeur",label = "Plec",
                               choices = as.vector(as.character(c("Total","Females","Males")),mode="list")),
                   HTML(paste0("<hr>Okres mapka bezposrednia:<hr>")),
                   textInput("meurbezod",
                             label = "Od",value = "2020-01-01"),
                   textInput("meurbezdo",
                             label = "Do",value = "2021-12-01"),
                   HTML(paste0("<hr>Okres mapka wzgledna:<hr>")),
                   textInput("meurwzod",
                             label = "Od",value = "2015-01-01"),
                   textInput("meurwzdo",
                             label = "Do",value = "2019-12-31"),
                   column(10,actionButton(inputId = "guzikmeur", label = "wykonaj")),
                   column(5,htmlOutput("meur")),
                   column(5,htmlOutput("meurwz"))
                   ),
          tabPanel("Szereg Gus",
                   selectInput("splecgus",label = "Plec",
                               choices = as.vector(as.character(c("Ogolem","Kobiety","Mezczyzni")),mode="list")),
                   selectInput("swiekgus",label = "Grupy Wiekowe",
                               choices = as.vector(as.character(c("0 - Inf","00 - 04","05 - 09","10 - 14","15 - 19","20 - 24","25 - 29","30 - 34","35 - 39","40 - 44","45 - 49","50 - 54","55 - 59","60 - 64","65 - 69","70 - 74","75 - 79","80 - 84","85 - 89","90 - Inf")),mode="list")),
                   selectInput("sregiongus",label = "Teren",
                               choices = as.vector(as.character(c("Polska","Dolnoslaskie","Kujawsko-Pomorskie","Lubelskie",           
                                                                  "Lubuskie","Lodzkie","Malopolskie","Mazowieckie",         
                                                                  "Opolskie","Podkarpackie","Podlaskie",           
                                                                  "Pomorskie", "Slaskie", "Swietokrzyskie",
                                                                  "Warminsko-Mazurskie", "Wielkopolskie",  "Zachodniopomorskie" )),mode="list")),
                   selectInput("sgranulacjagus",label = "Granulacja",
                               choices = as.vector(as.character(c("Tydzien","Miesiac","Rok")),mode="list")),
                   HTML(paste0("<hr>Okres szereg bezposredni:<hr>")),
                   textInput("sgusbezod",
                             label = "Od",
                             value = "2000-01-03"    
                   ),
                   textInput("sgusbezdo",
                             label = "Do",
                             value = Sys.Date()    
                   ),
                   HTML(paste0("<hr>Okres do porownania:<hr>")),
                   textInput("sguswzod",
                             label = "Od",
                             value = "2000-01-03"
                   ),
                   textInput("sguswzdo",
                             label = "Do",
                             value = Sys.Date()
                   ),
                   actionButton(inputId = "guziksgus", label = "wykonaj"),
                   column(5,plotlyOutput("sguswydruk1")),
                   column(5,plotlyOutput("sguswydruk2"))
                   
                   ),
          tabPanel("Szereg Eurostat",
                   selectInput("plecseur",label = "Plec",
                               choices = as.vector(as.character(c("Total","Females","Males")),mode="list")),
                   selectInput("krajseur",label = "teren",
                               choices = as.vector(as.character(c("Poland","Bulgaria","Germany","Estonia","Spain","Croatia","Latvia",
                                                              "Latvia","Lithuania","Luxembourg","Hungary","Netherlands","Austria",
                                                              "Belgium","Portugal","Slovenia","Slovakia","Finland","Sweden",
                                                              "Iceland","Liechtenstein","Norway","Switzerland","Serbia","Czechia",
                                                              "Montenegro","Denmark","Andorra","Italy","Malta","France","Georgia",
                                                              "Greece","Cyprus","Romania","United Kingdom","Albania","Armenia","Ireland")),mode="list")),
                   selectInput("granulacjaseur",label = "granulacja",
                               choices = as.vector(as.character(c("Tydzien","Miesiac","Rok")),mode="list")),
                   HTML(paste0("<hr>Okres szereg bezposredni:<hr>")),
                   textInput("seurbezod",
                             label = "Od",value = "2000-01-03"),
                   textInput("seurbezdo",
                             label = "Do",value = Sys.Date()),
                   HTML(paste0("<hr>Okres do porownania:<hr>")),
                   textInput("seurwzod",
                             label = "Od",value = "2000-01-03"),
                   textInput("seurwzdo",
                             label = "Do",value = Sys.Date()),
                   actionButton(inputId = "guzikseur", label = "wykonaj"),
                   column(5,plotlyOutput("wydrukseur")),
                   column(5,plotlyOutput("seurwydruk2"))

                   )
                   
                   
      )
    )
  )

))
