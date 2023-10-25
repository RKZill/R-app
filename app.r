library(shiny)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(this.path)
setwd(this.path::here())
print(this.path::here())

dat <- read.csv('/srv/shiny-server/ONFARM-soil-data.csv')

ui <- navbarPage("ONFARM Soil Health Database Tool", id='plots',
                 tabPanel("Scatterplot",
                          fluidPage(
                            p("The 25 ONFARM sites comparing soil health BMPs are spread across Southern and Eastern Ontario, and are reflective of Ontario's wide range in farming operations, soil types, topography, and climate. Each year the sites are sampled in June by Soil Resource Group to monitor soil health indicators. This dataset serves to show the range in results for a mix of standard indicators, and several more novel tests. This tool can be used to compare relationships between indicators, to see the variability Ontario farms might expect to see in their soils, and to track potential impacts from BMP use over time. Select between a scatterplot or boxplot view using the buttons at the top of the application."),
                            plotOutput("scatter"),
                            hr(),
                            fluidRow(
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
                 ),
                 
                 tabPanel("Boxplot", tabName='Boxplot',
                          fluidPage(
                            p("The 25 ONFARM sites comparing soil health BMPs are spread across Southern and Eastern Ontario, and are reflective of Ontario's wide range in farming operations, soil types, topography, and climate. Each year the sites are sampled in June by Soil Resource Group to monitor soil health indicators. This dataset serves to show the range in results for a mix of standard indicators, and several more novel tests. This tool can be used to compare relationships between indicators, to see the variability Ontario farms might expect to see in their soils, and to track potential impacts from BMP use over time. Select between a scatterplot or boxplot view using the buttons at the top of the application."),
                            plotOutput("box"),
                            hr(),
                            fluidRow(
                              column(4,
                                     varSelectInput("ypar", "Y-axis Variable:", dat[,14:26], selected = "om"),
                                     varSelectInput("xgroup", "X-axis Grouping:", dat[,2:9], selected = "region"),
                                     checkboxInput("by_col_box", "Show Colours", TRUE),
                                     varSelectInput("col_box", "Colour by:", dat[,2:9], selected = "soil_texture_group"),
                                     #checkboxInput("filter_out_box", "Filter Outliers", TRUE)
                              ),
                              column(4,
                                     checkboxGroupInput(
                                       "calendar_year_box", "Filter by Sampling Year",
                                       choices = unique(dat$calendar_year), 
                                       selected = unique(dat$calendar_year)),
                                     checkboxGroupInput(
                                       "bmp_type_box", "Filter by BMP Type",
                                       choices = unique(dat$bmp_type), 
                                       selected = unique(dat$bmp_type)),
                                     checkboxGroupInput(
                                       "landscape_position_box", "Filter by Landscape Position",
                                       choices = unique(dat$landscape_position), 
                                       selected = unique(dat$landscape_position)),
                                     checkboxGroupInput(
                                       "region_box", "Filter by ONFARM Region",
                                       choices = unique(dat$region), 
                                       selected = unique(dat$region)),
                                     checkboxGroupInput(
                                       "texture_box", "Filter by Soil Texture Group",
                                       choices = unique(dat$soil_texture_group),
                                       selected = unique(dat$soil_texture_group))),
                              column(4,
                                     checkboxGroupInput(
                                       "site_num_box", "Filter by Site Number",
                                       choices = unique(dat$site_num), 
                                       selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "22", "23", "24", "25")
                                     ))
                            )
                          )))


server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "plot=box"){
      updateNavbarPage(session, inputId = "plots", selected = "Boxplot")
    }
    if(query1 == "plot=scatter"){
      updateNavbarPage(session, inputId = "plots", selected = "Scatter")
    }
  })
  xlabel_scatter <- reactive({
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
  
  ylabel_scatter <- reactive({
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
  
  legendlabel_col <- reactive({
    if (input$col == "bmp_type") {
      print ("BMP Type")
    } else if (input$col == "landscape_position") {
      print ("Landscape Position")
    } else if (input$col == "region") {
      print ("Region")
    } else if (input$col == "soil_type_general") {
      print ("General Soil Type")
    } else if (input$col == "soil_texture_group") {
      print ("Soil Texture Group")
    } else if (input$col == "tillage_general") {
      print ("General Tillage Practice")
    } else if (input$col == "operation_type") {
      print ("Farm Operation Type")
    }  else if (input$col == "calendar_year") {
      print ("Year")
    }  
  })
  
  legendlabel_shp <- reactive({
    if (input$shp == "bmp_type") {
      print ("BMP Type")
    } else if (input$shp == "landscape_position") {
      print ("Landscape Position")
    } else if (input$shp == "region") {
      print ("Region")
    } else if (input$shp == "soil_type_general") {
      print ("General Soil Type")
    } else if (input$shp == "soil_texture_group") {
      print ("Soil Texture Group")
    } else if (input$shp == "tillage_general") {
      print ("General Tillage Practice")
    } else if (input$shp == "operation_type") {
      print ("Farm Operation Type")
    }  else if (input$shp == "calendar_year") {
      print ("Year")
    }  
  })
  
  xlabel_box <- reactive({
    if (input$xgroup == "bmp_type") {
      print("BMP Type")
    } else if (input$xgroup == "landscape_position") {
      print("Landscape Position")
    } else if (input$xgroup == "region") {
      print("Region")
    } else if (input$xgroup == "soil_type") {
      print("Soil Type")
    } else if (input$xgroup == "soil_texture") {
      print("Soil Texture")
    } else if (input$xgroup == "tillage_general") {
      print("General Tillage Practice")
    } else if (input$xgroup == "operation_type") {
      print("Farm Operation Type")
    } else if (input$xgroup == "calendar_year") {
      print("Year")}
  })
  
  ylabel_box <- reactive({
    if (input$ypar == "ace") {
      print("Autoclaved-Citrate Extractable Protein (ppm)")
    } else if (input$ypar == "active_c") {
      print("Active Carbon (ppm)")
    } else if (input$ypar == "agg_stability") {
      print("Wet Aggregate Stability (%)") 
    } else if (input$ypar == "bulk_density") {
      print("Bulk Density (g/cm^3))")
    } else if (input$ypar == "cec") {
      print("Cation Exchange Capacity (meq/100 g)")
    } else if (input$ypar == "k") {
      print("Soil Test Potassium (ppm)")
    } else if (input$ypar == "mg") {
      print("Soil Test Magnesium (ppm)")
    } else if (input$ypar == "om") {
      print("Organic Matter (%)")
    } else if (input$ypar == "p") {
      print("Soil Test Phosphorus (ppm)")
    } else if (input$ypar == "ph") {
      print("pH")
    } else if (input$ypar == "pmn") {
      print("Potentially Mineralizable Nitrogen (ppm)")
    } else if (input$ypar == "slan") {
      print("Solvita Labile Amino-Nitrogen (ppm)")
    } else if (input$ypar == "solvita") {
      print("Solvita CO2-Burst (ppm)")
    }})
  
  legendlabel_box <- reactive({
    if (input$col_box == "bmp_type") {
      print ("BMP Type")
    } else if (input$col_box == "landscape_position") {
      print ("Landscape Position")
    } else if (input$col_box == "region") {
      print ("Region")
    } else if (input$col_box == "soil_type_general") {
      print ("General Soil Type")
    } else if (input$col_box == "soil_texture_group") {
      print ("Soil Texture Group")
    } else if (input$col_box == "tillage_general") {
      print ("General Tillage Practice")
    } else if (input$col_box == "operation_type") {
      print ("Farm Operation Type")
    }  else if (input$col_box == "calendar_year") {
      print ("Year")
    }  
  })
  
  subsetted <- reactive({
    req(input$site_num)
    dat |> filter(site_num %in% input$site_num)
  })
  
  subsetted2 <- reactive({
    req(input$calendar_year)
    subsetted() |> filter(calendar_year %in% input$calendar_year)
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
  
  
  subsetted_box <- reactive({
    req(input$site_num_box)
    dat |> filter(site_num %in% input$site_num_box)
  })
  
  subsetted2_box <- reactive({
    req(input$calendar_year_box)
    subsetted_box() |> filter(calendar_year %in% input$calendar_year_box)
  })
  
  subsetted3_box <- reactive({
    req(input$bmp_type_box)
    subsetted2_box() |> filter(bmp_type %in% input$bmp_type_box)
  })
  
  subsetted4_box <- reactive({
    req(input$region_box)
    subsetted3_box() |> filter(region %in% input$region_box)
  })
  
  subsetted5_box <- reactive({
    req(input$landscape_position_box)
    subsetted4_box() |> filter(landscape_position %in% input$landscape_position_box)
  })
  
  subsetted6_box <- reactive({
    req(input$texture_box)
    subsetted5_box() |> filter(soil_texture_group %in% input$texture_box)
  })  
  
  
  output$scatter <- renderPlot({
    scatterp<-ggplot(subsetted6(), aes(!!input$xvar, !!input$yvar)) + list(
      geom_point(size=2, alpha= 0.65),
      if (input$by_col) aes_string(color=input$col),
      if (input$by_shp) aes_string(shape=input$shp),
      theme_bw(),
      xlab(xlabel_scatter()),
      ylab(ylabel_scatter()),
      labs(colour = legendlabel_col(), shape = legendlabel_shp()))
    
    scatterp
  }, res= 96)
  
  
  output$box <- renderPlot({
    boxp<-ggplot(subsetted6_box(), aes(!!input$xgroup, !!input$ypar)) + list(
      geom_boxplot(),
      if (input$by_col_box) aes_string(fill=input$col_box),
      theme_bw(),
      xlab(xlabel_box()),
      ylab(ylabel_box()),
      labs(fill = legendlabel_box())
    )
    
    boxp
  }, res= 96)
  
}

shinyApp(ui, server)