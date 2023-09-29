library(shiny)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(this.path)

setwd(this.path::here())
print(this.path::here())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dat <- read.csv('/srv/shiny-server/ONFARM-soil-data.csv')

ui <- fluidPage(
  titlePanel(div("ONFARM Soil Health Indicator Comparison Tool" , style="font-family:Merriweather; font-weight:700; font-size:45px;")),
  p("The 25 ONFARM sites comparing soil health BMPs are spread across Southern and Eastern Ontario, and are reflective of Ontario's wide range in farming operations, soil types, topography, and climate. Each year the sites are sampled in June by Soil Resource Group to monitor soil health indicators. This dataset serves to show the range in results for a mix of standard indicators, and several more novel tests. This tool can be used to compare relationships between indicators, to see the variability Ontario farms might expect to see in their soils, and to track potential impacts from BMP use over time." , style="font-family:Montserrat; font-size:17px;"),
  plotOutput("scatter"),
  hr(),
  fluidRow(
    style="font-family:Montserrat; font-size:17px;",
    column(4,
      varSelectInput("xvar", "X-axis Variable:", dat[,14:26], selected = "om"),
      varSelectInput("yvar", "Y-axis Variable:", dat[,14:26], selected = "active_c"),
      checkboxInput("by_col", "Show colours", TRUE),
      checkboxInput("by_shp", "Show shapes", FALSE),
      varSelectInput("col", "Colour by:", dat[,2:9], selected = "soil_texture_group"),
      varSelectInput("shp", "Shapes by:", dat[,2:9], selected = "bmp_type")),
    column(4,  
        checkboxGroupInput(
            "calendar_year", "Filter by Sampling Year",
             choices = unique(dat$calendar_year), 
             selected = unique(dat$calendar_year)),
        checkboxGroupInput(
          "bmp_type", "Filter by BMP Type",
          choices = unique(dat$bmp_type), 
          selected = unique(dat$bmp_type)),
        checkboxGroupInput(
          "landscape_position", "Filter by Landscape Position",
          choices = unique(dat$landscape_position), 
          selected = unique(dat$landscape_position)),
        checkboxGroupInput(
          "region", "Filter by ONFARM Region",
          choices = unique(dat$region), 
          selected = unique(dat$region)),
        checkboxGroupInput(
          "texture", "Filter by Soil Texture Group",
          choices = unique(dat$soil_texture_group),
          selected = unique(dat$soil_texture_group)
        )),
    column(4,
      checkboxGroupInput(
        "site_num", "Filter by Site Number",
        choices = unique(dat$site_num), 
        selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "22", "23", "24", "25")
    ))
))

server <- function(input, output, session) {
  
  xlabel <- reactive({
    if (input$xvar == "ace") {
    print("Autoclaved-Citrate Extractable Protein (ppm)")
  } else if (input$xvar == "active_c") {
    print("Active Carbon (ppm)")
  } else if (input$xvar == "agg_stability") {
    print("Wet Aggregate Stability (%)") 
  } else if (input$xvar == "bulk_density") {
    print("Bulk Density (g/cm^3))")
  } else if (input$xvar == "cec") {
    print("Cation Exchange Capacity (meq/100 g)")
  } else if (input$xvar == "k") {
    print("Soil Test Potassium (ppm)")
  } else if (input$xvar == "mg") {
    print("Soil Test Magnesium (ppm)")
  } else if (input$xvar == "om") {
    print("Organic Matter (%)")
  } else if (input$xvar == "p") {
    print("Soil Test Phosphorus (ppm)")
  } else if (input$xvar == "ph") {
    print("pH")
  } else if (input$xvar == "pmn") {
    print("Potentially Mineralizable Nitrogen (ppm)")
  } else if (input$xvar == "slan") {
    print("Solvita Labile Amino-Nitrogen (ppm)")
  } else if (input$xvar == "solvita") {
    print("Solvita CO2-Burst (ppm)")
          }})
  
  ylabel <- reactive({
    if (input$yvar == "ace") {
      print("Autoclaved-Citrate Extractable Protein (ppm)")
    } else if (input$yvar == "active_c") {
      print("Active Carbon (ppm)")
    } else if (input$yvar == "agg_stability") {
      print("Wet Aggregate Stability (%)") 
    } else if (input$yvar == "bulk_density") {
      print("Bulk Density (g/cm^3))")
    } else if (input$yvar == "cec") {
      print("Cation Exchange Capacity (meq/100 g)")
    } else if (input$yvar == "k") {
      print("Soil Test Potassium (ppm)")
    } else if (input$yvar == "mg") {
      print("Soil Test Magnesium (ppm)")
    } else if (input$yvar == "om") {
      print("Organic Matter (%)")
    } else if (input$yvar == "p") {
      print("Soil Test Phosphorus (ppm)")
    } else if (input$yvar == "ph") {
      print("pH")
    } else if (input$yvar == "pmn") {
      print("Potentially Mineralizable Nitrogen (ppm)")
    } else if (input$yvar == "slan") {
      print("Solvita Labile Amino-Nitrogen (ppm)")
    } else if (input$yvar == "solvita") {
      print("Solvita CO2-Burst (ppm)")
    }})
  
  subsetted2 <- reactive({
    req(input$calendar_year)
    subsetted() |> filter(calendar_year %in% input$calendar_year)
  })
  
  subsetted <- reactive({
    req(input$site_num)
        dat |> filter(site_num %in% input$site_num)
  })
  

  
  subsetted3 <- reactive({
    req(input$bmp_type)
    subsetted2() |> filter(bmp_type %in% input$bmp_type)
  })
  
  subsetted4 <- reactive({
    req(input$region)
    subsetted3() |> filter(region %in% input$region)
  })
 
   subsetted5 <- reactive({
    req(input$landscape_position)
    subsetted4() |> filter(landscape_position %in% input$landscape_position)
  })

      subsetted6 <- reactive({
     req(input$texture)
     subsetted5() |> filter(soil_texture_group %in% input$texture)
   })
     
  output$scatter <- renderPlot({
    
    scatterp<-ggplot(subsetted6(), aes(!!input$xvar, !!input$yvar)) + list(
      geom_point(size=2, alpha= 0.65),
      if (input$by_col) aes_string(color=input$col),
      if (input$by_shp) aes_string(shape=input$shp),
      theme_bw(),
      xlab(xlabel()),
      ylab(ylabel()))
    
    scatterp
  }, res= 96)
  
  }

shinyApp(ui, server)