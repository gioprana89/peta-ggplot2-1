

#library(webr)


#library(plyr)
library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)
library(shinycssloaders)
library(shiny)
library(scales)

library(DT)
library(data.table)

library(leaflet)
library(sf)


library(ggplot2)

library(dplyr)
library(tibble)



########################################
########UI (User Interface)#############
########################################

modul_peta_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
  
    
    includeHTML("home.html"),
    
    br(),
    br(),
    
    
    fluidRow(
      column(6,
             
             
             uiOutput(ns("pemilihan_wilayah_radiobutton")),
             
             br(),
             br(),
             
             shinycssloaders::withSpinner(plotOutput(ns("peta_sumut"), width="100%")),
             
             
             
             br()
             
      ),
      
      
      column(6,
             
             
             
             uiOutput(ns("pemilihan_wilayah_terpilih_checkboxGroupInput")),
             
             br(),
             br(),
             
             shinycssloaders::withSpinner(plotOutput(ns("peta_sumut_2"), width="100%")),
             
             
             br()
             
      )
      
      
      
    ), #Akhir fluidrow
    
    
    
    
    
    
    
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_peta_ui

#Akhir dari modul_peta_ui
#Akhir dari modul_peta_ui
#Akhir dari modul_peta_ui
#Akhir dari modul_peta_ui











































































########################################
################Server##################
########################################



modul_peta_server <- function(input, output, session) {
  
  
  
  
  
  
  
  
  
  output$peta_sumut <- renderPlot({
    
    
    
    shp_sumut <- read_sf("shp sumut/shp_sumut.shp") 
    
    wilayah <- shp_sumut$NAME_2
    warna <- rep("white", length(wilayah) )
    
    pilih_wilayah <- input$terpilih_pemilihan_wilayah_radiobutton
    
    indeks_pilih_wilayah <- which(wilayah == pilih_wilayah)
    
    warna_baru <- warna
    
    warna_baru[indeks_pilih_wilayah] = "red"
    
    
    
    
    
    
    p <- ggplot(shp_sumut) +
      geom_sf(fill = warna_baru , color = "grey")
    
    print(p)
    
  })
  
  
  
  
  ##############
  ##############
  
  
  kirim_nama_wilayah <- function()
  {
    
    
    shp_sumut <- read_sf("shp sumut/shp_sumut.shp") 
    
    return(shp_sumut$NAME_2)
    
  }
  
  
  ############
  ############
  
  
  output$pemilihan_wilayah_radiobutton <- renderUI({
    
    
    
    radioButtons(session$ns("terpilih_pemilihan_wilayah_radiobutton"),
                 label="Pilih Wilayah Kabupaten/Kota:", choices = c( kirim_nama_wilayah()), inline = TRUE, selected=c())
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############
  
  
  
  
  output$peta_sumut_2 <- renderPlot({
    
    
    
    shp_sumut <- read_sf("shp sumut/shp_sumut.shp") 
    
    wilayah <- shp_sumut$NAME_2
    warna <- rep("white", length(wilayah) )
    
    pilih_wilayah <- input$terpilih_pemilihan_wilayah_terpilih_checkboxGroupInput
    pilih_wilayah <- unlist(pilih_wilayah)
    pilih_wilayah <- as.character(pilih_wilayah)
    
    
    indeks_pilih_wilayah <- wilayah %in% pilih_wilayah
    
    indeks_pilih_wilayah <- which(indeks_pilih_wilayah == TRUE)
    
    
    
    
    warna_baru <- warna
    
    warna_baru[indeks_pilih_wilayah] = rep("red", length(indeks_pilih_wilayah) )
    
    
    
    
    
    
    p <- ggplot(shp_sumut) +
      geom_sf(fill = warna_baru , color = "grey")
    
    print(p)
    
  })
  
  
  
  
  
  
  
  
  ############
  ############
  
  
  output$pemilihan_wilayah_terpilih_checkboxGroupInput <- renderUI({
    
    
    
    checkboxGroupInput(session$ns("terpilih_pemilihan_wilayah_terpilih_checkboxGroupInput"), 
                       label="Pilih Wilayah Kabupaten/Kota:", choices = c( kirim_nama_wilayah()), inline = TRUE, selected=c())
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_peta_server

#akhir dari modul_peta_server
#akhir dari modul_peta_server
#akhir dari modul_peta_server

















































































ui <- fluidPage(
  
  
  #includeHTML("intro_home.html"),
  
  
  uiOutput("modul_peta"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_peta <- renderUI({
    
    
    
    #source("module//modul_peta.R")
    callModule(module = modul_peta_server, id = "modul_peta")
    modul_peta_ui(id = "modul_peta")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














