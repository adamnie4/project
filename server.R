library(shiny)
library(plotly)
library(data.table)
library(reshape)
library(readxl)
library(DT)
library(googleVis)
library(dplyr)
library(RSQLite)


shinyServer(function(input, output) {

  sqlVar <- reactiveValues(
    sqlText = NULL
  )

  observeEvent(input$sqlQueryInput,{
    sqlVar$sqlText <- input$sqlQueryInput
  })

  output$plainSQLText <- renderPrint({
    return(cat(paste(sqlVar$sqlText,"\n")))
  })
#ZAKLADKA 1 POBIERANIE I OBROBKA DANYCH 
  observeEvent(input$open_page_GUS,{
    dataDir    <- file.path(getwd(),"data")
    if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })  

  observeEvent(input$open_page_EUROSTAT,{
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
    if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })  
  
  
  obr <- reactive({
    try({
      dataDir    <- file.path(getwd(),"data")
      unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)
      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      try({
        lapply(dir(),function(f){
          file.rename(
            from=f, 
            to = gsub(" ","_",gsub("\x88","l",f))
          )
        })
      })
      setwd(hd)
      
    })
    
    czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
      
      d <- as.data.frame(read_excel(f,sheet=sheet))
      colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
      d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
      
      tygodnie <- 1:(ncol(d)-3)
      tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
      colnames(d)[4:ncol(d)] <- tygodnie
      
      d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
      colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
      d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
      d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
      #d$Liczba[is.na(d$Liczba)] <- 0 
      d <- cbind("Plec"=plec,d)
      
      return(d)
      
    }
    
    hd <- getwd()
    setwd(file.path(dataDir,"zgony_wg_tygodni"))
    
    try({
      mainRet <- do.call("rbind",lapply(dir(),function(f){
        print(f)
        
        ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
        mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
        kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
        
        dane <- rbind(ogolem,mezczyzni,kobiety)
        
        tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
        tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
          return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
        }))
        tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(tygodnie$Tydzien,"-"),function(x){x[2]})))
        rownames(tygodnie) <- NULL
        
        dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
        dane <- dane[,-which(colnames(dane)=="Tydzien")]
        
        dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
        dane$Liczba <- as.integer(dane$Liczba)
        
        dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
        dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
        
        return(dane)
      }))
      
      write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
    })
    
    setwd(hd)
    
  })
  
  obr1 <- reactive({
    try({
      dataDir    <- file.path(getwd(),"data")
      unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)
      rm(list=ls())
      d <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
      d$Value <- as.integer(gsub(",|:","",d$Value))
      
    })
    
  })

  observeEvent(input$gus_baza,{
    obr()
  })
  
  observeEvent(input$eur_baza,{
    obr1()
  })
  #Mapka interaktywna z danymi GUS
  #wartosci interaktywne z ustawionymi wartosciami domyslnymi:
  mgusbezbaza <- reactiveValues(
  )
  meurbezbaza <- reactiveValues(
  )
  mmplecgus <- reactiveValues(
    plecmgus = "Ogolem"
  )
  observeEvent(input$wybor_plec_mgus,{
    mmplecgus$plecmgus <- input$wybor_plec_mgus
  })
  
  mmwiekgus <- reactiveValues(
    wiekmgus = "0 - Inf"
  )
  observeEvent(input$wybor_wiek_mgus,{
    mmwiekgus$wiekmgus <- input$wybor_wiek_mgus
  })

  #Mapka gus
  observeEvent(input$guzikmgus,{
    obrobka_mgus <- read.csv(file="data/GUS_dane_przetworzone_pelne.csv", sep = ';')
    obrobka_mgus <- obrobka_mgus[obrobka_mgus$Region_id == 'PL21' | obrobka_mgus$Region_id == 'PL22' | obrobka_mgus$Region_id == 'PL41' | obrobka_mgus$Region_id == 'PL42' | 
                                   obrobka_mgus$Region_id == 'PL51' | obrobka_mgus$Region_id == 'PL52' | obrobka_mgus$Region_id == 'PL61' | obrobka_mgus$Region_id == 'PL62' | 
                                   obrobka_mgus$Region_id == 'PL63' | obrobka_mgus$Region_id == 'PL71' | obrobka_mgus$Region_id == 'PL72' | obrobka_mgus$Region_id == 'PL81' | 
                                   obrobka_mgus$Region_id == 'PL82' | obrobka_mgus$Region_id == 'PL84' | obrobka_mgus$Region_id == 'PL9' | obrobka_mgus$Region_id == 'PL43',]
    obrobka_mgus <- obrobka_mgus[obrobka_mgus$Plec == mmplecgus$plecmgus ,]
    obrobka_mgus <- obrobka_mgus[obrobka_mgus$Grupa_wiekowa == mmwiekgus$wiekmgus ,]
    obrobka_mgus$Od  <-as.Date(obrobka_mgus$Od, format = "%Y-%m-%d")
    obrobka_mgus$Do  <-as.Date(obrobka_mgus$Do, format = "%Y-%m-%d")
    
    mgusbezbaza$mgusbezodx <- input$mgusbezod
    mgusbezbaza$mgusbezdox <- input$mgusbezdo
    mgusbezbaza$mguswzodx <- input$mguswzod
    mgusbezbaza$mguswzdox <- input$mguswzdo
    mgusbezbaza$mgusbezodx  <-as.Date(mgusbezbaza$mgusbezodx, format = "%Y-%m-%d")
    mgusbezbaza$mgusbezdox  <-as.Date(mgusbezbaza$mgusbezdox, format = "%Y-%m-%d")
    mgusbezbaza$mguswzodx  <-as.Date(mgusbezbaza$mguswzodx, format = "%Y-%m-%d")
    mgusbezbaza$mguswzdox  <-as.Date(mgusbezbaza$mguswzdox, format = "%Y-%m-%d")
    #Ustalany okres dla mapy bezposredniej
    obrobka_mgusbez <- obrobka_mgus[obrobka_mgus$Od >= mgusbezbaza$mgusbezodx & obrobka_mgus$Od <= mgusbezbaza$mgusbezdox ,]
    #suma przypadkow 
    sumaprzypbez<-aggregate(obrobka_mgusbez$Liczba, by=list(Category=obrobka_mgusbez$Region_id), FUN=sum)
    
    domapygusbez <- reactive({
      dfgus <- data.frame(
        Wojewodztwo=c("Malopolskie","Slaskie","Wielkopolskie",           
                      "Zachodniopomorskie","Lubuskie","Dolnoslaskie","Opolskie",         
                      "Kujawsko-Pomorskie","Warminsko-Mazurskie","Pomorskie", "Lodzkie",           
                      "Swietokrzyskie", "Lubelskie", "Podkarpackie",
                      "Podlaskie", "Mazowieckie" 
        ),
        Liczba = unlist(sumaprzypbez[,c(2)])
      )
      return(dfgus)
    })
   

    #mapa wzgledna 
    #okres do porownania
    obrobka_mguswz <- obrobka_mgus[obrobka_mgus$Od >= mgusbezbaza$mguswzodx & obrobka_mgus$Od <= mgusbezbaza$mguswzdox ,]
    sumaprzypporownanie<-aggregate(obrobka_mguswz$Liczba, by=list(Category=obrobka_mguswz$Region_id), FUN=sum)
    
    gora = unlist(sumaprzypbez[,c(2)])
    dol = unlist(sumaprzypporownanie[,c(2)])
    
    domapyguswz <- reactive({
      dfgus <- data.frame(
        Wojewodztwo=c("Malopolskie","Slaskie","Wielkopolskie",           
                      "Zachodniopomorskie","Lubuskie","Dolnoslaskie","Opolskie",         
                      "Kujawsko-Pomorskie","Warminsko-Mazurskie","Pomorskie", "Lodzkie",           
                      "Swietokrzyskie", "Lubelskie", "Podkarpackie",
                      "Podlaskie", "Mazowieckie" 
        ),
        Liczba = gora/dol
      )
      return(dfgus)
    })
    
    output$mapkagusfinalwz <- renderGvis({
      gvisGeoChart(domapyguswz(), "Wojewodztwo", "Liczba",
                   options=list(region="PL",
                                displayMode="regions",
                                resolution="provinces",
                                width=250, height=250))
    })
    
    output$mapkagusfinalbez <- renderGvis({
      gvisGeoChart(domapygusbez(), "Wojewodztwo", "Liczba",
                   options=list(region="PL",
                                displayMode="regions",
                                resolution="provinces",
                                width=250, height=250))
    })
    
  })
  
  #Eurostat mapka interaktywna 
  
  mmpleceur <- reactiveValues(
    plecmeur = "Total"
  )
  observeEvent(input$plecmeur,{
    mmpleceur$plecmeur <- input$plecmeur
  })
  
  observeEvent(input$guzikmeur,{
    
    dane_eur <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
    dane_eur <- na.omit(dane_eur)
    dane_eur$GEO[dane_eur$GEO =='Germany (until 1990 former territory of the FRG)'] <-"Germany"
    dane_eur$GEO[dane_eur$GEO =='Czechia'] <-"Czech Republic"
    dane_eur$Value<-as.integer(gsub(",|:","",dane_eur$Value))
    dane_eur <- na.omit(dane_eur)
    
    #daty
    dane_eur$year <- substring(dane_eur$TIME, 1, 4)
    dane_eur$week <- substring(dane_eur$TIME, 6, 7)
    dane_eur$TIME <- as.character(as.Date(paste(dane_eur$year, dane_eur$week, 1, sep="-"), "%Y-%W-%u"))
    #plec
    dane_eur <- dane_eur[dane_eur$SEX == mmpleceur$plecmeur ,]
    #okres
    
    dane_eur_bez <- dane_eur[as.Date(dane_eur$TIME,format = "%Y-%m-%d")>=as.Date(input$meurbezod) & as.Date(dane_eur$TIME,format = "%Y-%m-%d")<=as.Date(input$meurbezdo),]
    #suma
    summapaeur<-aggregate(dane_eur_bez$Value, by=list(Category=dane_eur_bez$GEO), FUN=sum)
    
    danemeur <- reactive({
      df <- data.frame(
        country=unlist(summapaeur[,c(1)]),
        wartosc_eur = unlist(summapaeur[,c(2)])
      )
      
      return(df)
      
    })
    
    #wydruk 
    
    output$meur <- renderGvis({
      gvisGeoChart(danemeur(), locationvar="country", colorvar="wartosc_eur",
                   options=list(region="150",resolution="countries",width=250,height=250,as.is=T))
    })
    
    #okres wzgledny
    dane_eur_wz <- dane_eur[as.Date(dane_eur$TIME,format = "%Y-%m-%d")>=as.Date(input$meurwzod) & as.Date(dane_eur$TIME,format = "%Y-%m-%d")<=as.Date(input$meurwzdo),]
    summapaeurwz<-aggregate(dane_eur_wz$Value, by=list(Category=dane_eur_wz$GEO), FUN=sum)
    
    danemeurwz <- reactive({
      dfx <- data.frame(
        country=unlist(summapaeurwz[,c(1)]),
        wartosc_eur = unlist(summapaeur[,c(2)])/unlist(summapaeurwz[,c(2)])
      )
      
      return(dfx)
      
    })
    
    output$meurwz <- renderGvis({
      gvisGeoChart(danemeurwz(), locationvar="country", colorvar="wartosc_eur",
                   options=list(region="150",resolution="countries",width=250,height=250,as.is=T))
    })
  })
  #szeregi 
  observeEvent(input$guziksgus,{
    sgusobrobka <- read.csv(file="data/GUS_dane_przetworzone_pelne.csv", sep = ';')
    sgusobrobka <- sgusobrobka[sgusobrobka$Region_id == 'PL' |sgusobrobka$Region_id == 'PL21' | sgusobrobka$Region_id == 'PL22' | sgusobrobka$Region_id == 'PL41' | sgusobrobka$Region_id == 'PL42' |
                           sgusobrobka$Region_id == 'PL51' | sgusobrobka$Region_id == 'PL52' | sgusobrobka$Region_id == 'PL61' | sgusobrobka$Region_id == 'PL62' |
                           sgusobrobka$Region_id == 'PL63' | sgusobrobka$Region_id == 'PL71' | sgusobrobka$Region_id == 'PL72' | sgusobrobka$Region_id == 'PL81' | 
                           sgusobrobka$Region_id == 'PL82' | sgusobrobka$Region_id == 'PL84' | sgusobrobka$Region_id == 'PL9' | sgusobrobka$Region_id == 'PL43',]
    sgusobrobka <- sgusobrobka[sgusobrobka$Plec == input$splecgus ,]
    sgusobrobka <- sgusobrobka[sgusobrobka$Grupa_wiekowa == input$swiekgus ,]

    sgusobrobka$Od  <-as.Date(sgusobrobka$Od, format = "%Y-%m-%d")
    sgusobrobka$Do  <-as.Date(sgusobrobka$Do, format = "%Y-%m-%d")
    
    sgusobrobka$year <- strftime(sgusobrobka$Do, "%Y")
    sgusobrobka$month <- strftime(sgusobrobka$Od, "%m")
    sgusobrobka$month <- paste(sgusobrobka$year,sgusobrobka$month,sep="-")
    sgusobrobka$month <- paste(sgusobrobka$month,"01",sep="-")
    
    
    
    if(input$sregiongus == "Polska"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL"),]
    }
    if(input$sregiongus == "Swietokorzyskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL72"),]
    }
    if(input$sregiongus == "Lubelskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL81"),]
    }
    if(input$sregiongus == "Podkarpackie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL82"),]
    }
    if(input$sregiongus == "Podlaskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL84"),]
    }
    if(input$sregiongus == "Mazowieckie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL9"),]
    }
    if(input$sregiongus == "Malopolskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL21"),]
    }
    if(input$sregiongus == "Slaskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL22"),]
    }
    if(input$sregiongus == "Wielkopolskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL41"),]
    }
    if(input$sregiongus == "Zachodniopomorskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL42"),]
    }
    if(input$sregiongus == "Lubuskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL43"),]
    }
    if(input$sregiongus == "Dolnoslaskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL51"),]
    }
    if(input$sregiongus == "Opolskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL52"),]
    }
    if(input$sregiongus == "Kujawsko-Pomorskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL61"),]
    }
    if(input$sregiongus == "Warminsko-Mazurskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL62"),]
    }
    if(input$sregiongus == "Pomorskie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL63"),]
    }
    if(input$sregiongus == "Lodzkie"){
      sgusobrobka<-sgusobrobka[(sgusobrobka$Region_id == "PL71"),]
    }
    
    
    #pierwszy wykres
    sgusbez <- sgusobrobka[as.Date(sgusobrobka$Od,format = "%Y-%m-%d")>=as.Date(input$sgusbezod) & as.Date(sgusobrobka$Od,format = "%Y-%m-%d")<=as.Date(input$sgusbezdo),]
    
    sgusbez <- na.omit(sgusbez)
    
    if (input$sgranulacjagus == "Tydzien"){
      sumaszereggus<-aggregate(sgusbez$Liczba, by=list(Category=sgusbez$Od), FUN=sum)}
    else if (input$sgranulacjagus == "Miesiac"){
      sumaszereggus<-aggregate(sgusbez$Liczba, by=list(Category=sgusbez$month), FUN=sum)  
    }
    else if (input$sgranulacjagus == "Rok"){
      sumaszereggus<-aggregate(sgusbez$Liczba, by=list(Category=sgusbez$year), FUN=sum) 
    }
    #wydruk
    output$sguswydruk1 <- renderPlotly({
      sguswydruk <- plot_ly(sumaszereggus, x = ~Category, y = ~x, type = 'scatter', mode = 'lines')%>%
      layout(title = 'Szereg Gus', xaxis = list(title = 'Data'), yaxis = list(title = 'Zgony'))
      return(sguswydruk)
    })
    #drugi szereg
    sguswz <- sgusobrobka[as.Date(sgusobrobka$Od,format = "%Y-%m-%d")>=as.Date(input$sguswzod) & as.Date(sgusobrobka$Od,format = "%Y-%m-%d")<=as.Date(input$sguswzdo),]
    sguswz <- na.omit(sguswz)
    #srednia z okresu do porownania (okres do porownania ma te same parametry tylko jest obciety do innego wymiaru czasowego)
    srednia <-  mean(sguswz$Liczba)

    output$sguswydruk2 <- renderPlotly({
      sguswydruk1 <- plot_ly(sumaszereggus, x = ~Category, y = ~x/srednia, type = 'scatter', mode = 'lines')%>%
        layout(title = 'Szereg Gus', xaxis = list(title = 'Data'), yaxis = list(title = 'Zgony'))
      return(sguswydruk1)
    })
    
  })
  
  #sql
  
  observeEvent(input$guziksql,{
    dataDir    <- file.path(getwd(),"data")
    dbName <- file.path(dataDir,"database.db")
    conn <- dbConnect(dbDriver("SQLite"),
      dbname = dbName)
    df <- read.table(file=file.path(dataDir,"GUS_dane_przetworzone_pelne.csv"),sep=";",dec=",",header=T)
    dbWriteTable(conn, "gus_db", df, overwrite = TRUE, row.names = FALSE)
    #Dodanie tabeli z danymi EUROSTAT
    baza <- read.table(file=file.path(dataDir,"demo_r_mwk_ts_1_Data.csv"),sep=";",dec=",",header=T)
    dbWriteTable(conn, "eurostat_db", baza, overwrite = TRUE, row.names = FALSE)
    dbDisconnect(conn)
  })
  
  observeEvent(input$zapytanie, {
    sqlVar$sqlText <- input$sqlQueryInput
    dataDir <- file.path(getwd(),"data")
    conn <- dbConnect(dbDriver("SQLite"),
    dbname <- file.path(dataDir,"database.db"))
    zapytanie <- dbGetQuery(conn, sqlVar$sqlText)
    
    output$tabelka <- DT::renderDataTable({
      DT::datatable(zapytanie, rownames = FALSE,options = list(scrollX = TRUE,
          pageLength = 16,
          lengthMenu = seq(from=10,by=10,to=100)))})
  })
  
  #szereg eurostat
  
  observeEvent(input$guzikseur,{
    eur_obrobka <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
    eur_obrobka$year <- substring(eur_obrobka$TIME, 1, 4)
    eur_obrobka$week <- substring(eur_obrobka$TIME, 6, 7)
    eur_obrobka$TIME <- as.character(as.Date(paste(eur_obrobka$year, eur_obrobka$week, 1, sep="-"), "%Y-%W-%u"))
    eur_obrobka$month <-strftime(eur_obrobka$TIME, "%m")
    eur_obrobka$month <- paste(eur_obrobka$year,eur_obrobka$month,sep="-")
    eur_obrobka$month <- paste(eur_obrobka$month,"01",sep="-")
    eur_obrobka$Value<-as.integer(gsub(",|:","",eur_obrobka$Value))
    eur_obrobka$GEO[eur_obrobka$GEO =='Germany (until 1990 former territory of the FRG)'] <-"Germany"
    eur_obrobka$GEO[eur_obrobka$GEO =='Czechia'] <-"Czech Republic"
    eur_obrobka <- na.omit(eur_obrobka)
    
    #Parametry
    
    eur_obrobka<- eur_obrobka[eur_obrobka$GEO == input$krajseur ,]
    eur_obrobka <- eur_obrobka[eur_obrobka$SEX == input$plecseur ,]
    
    #szereg1
    seurbez <- eur_obrobka[as.Date(eur_obrobka$TIME,format = "%Y-%m-%d")>=as.Date(input$seurbezod) & as.Date(eur_obrobka$TIME,format = "%Y-%m-%d")<=as.Date(input$seurbezdo),]
    
    
    if (input$granulacjaseur == "Tydzien"){
      sseuro<-aggregate(seurbez$Value, by=list(Category=seurbez$TIME), FUN=sum)}
    else if (input$granulacjaseur == "Miesiac"){
      sseuro<-aggregate(seurbez$Value, by=list(Category=seurbez$month), FUN=sum)  
    }
    else if (input$granulacjaseur == "Rok"){
      sseuro<-aggregate(seurbez$Value, by=list(Category=seurbez$year), FUN=sum) 
    }
    
    output$wydrukseur <- renderPlotly({
      seurwydruk1 <- plot_ly(sseuro, x = ~Category, y = ~x/srednia, type = 'scatter', mode = 'lines')%>%
        layout(title = 'Szereg eur', xaxis = list(title = 'Data'), yaxis = list(title = 'Zgony'))
      #return(seurwydruk1)
    })
    #drugi szereg
    seurwz <- eur_obrobka[as.Date(eur_obrobka$Od,format = "%Y-%m-%d")>=as.Date(input$seurwzod) & as.Date(eur_obrobka$Od,format = "%Y-%m-%d")<=as.Date(input$seurwzdo),]
    seurwz <- na.omit(seurwz)
    #srednia z okresu do porownania (okres do porownania ma te same parametry tylko jest obciety do innego wymiaru czasowego)
    srednia <-  mean(seurwz$Value)
    
    output$seurwydruk2 <- renderPlotly({
      seurwydruk1 <- plot_ly(sseuro, x = ~Category, y = ~x/srednia, type = 'scatter', mode = 'lines')%>%
        layout(title = 'Szereg eur', xaxis = list(title = 'Data'), yaxis = list(title = 'Zgony'))
      #return(seurwydruk1)
    })
    
  })
  
  
  
})
