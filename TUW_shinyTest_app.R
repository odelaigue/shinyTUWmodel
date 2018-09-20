
#! ===============================
#!
#! Description       : TUW model Shiny app
#! 
#! Authors           : Olivier Delaigue <olivier.delaigue@irstea.fr>
#! 
#! Creation date     : 2018-09-19
#! 
#! Commentaire       : 
#!
#! ===============================






#! ------------------------------------- Packages

library(shiny)





#! ------------------------------------- UI

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("TUW model"),
  
  # Sidebar with sliders inputs for TUW parameters
  sidebarLayout(
    sidebarPanel(
      
      fileInput(inputId = "dataLoad", label = "Choose dataset",
                accept = ".RData"),
      
      selectInput(inputId = "dataChoice", label = NULL, choices = NULL),
      
      h3("Parameters values:"),
      sliderInput(inputId = "p01", label = "SCF:",
                  min = 0.9, max = 1.5, value = 1),
      sliderInput(inputId = "p02", label = "DDF:",
                  min = 0.0, max = 5.5, value = 1),
      sliderInput(inputId = "p03", label = "Tr:",
                  min = 1.0, max = 3.3, value = 2),
      sliderInput(inputId = "p04", label = "Ts:",
                  min = -3.0, max = 1.0, value = 0),
      sliderInput(inputId = "p05", label = "Tm:",
                  min = -2.0, max = 2.2, value = 0)#,
      # sliderInput(inputId = "p06", label = "LPrat:",
      #             min = 0.0, max = 1.0, value = 0.5), 
      # sliderInput(inputId = "p07", label = "FC:",
      #             min = 0.0, max = 600.0, value = 300.0),
      # sliderInput(inputId = "p08", label = "BETA:",
      #             min = 0.0, max = 20.0, value = 10.0),
      # sliderInput(inputId = "p09", label = "k0:",
      #             min = 0.0, max = 2.0, value = 1.0), 
      # sliderInput(inputId = "p10", label = "k1:",
      #             min = 2.0, max = 30.0, value = 14.0),      
      # sliderInput(inputId = "p11", label = "k2:",
      #             min = 30.0, max = 250.0, value = 100.0),
      # sliderInput(inputId = "p12", label = "lsuz:",
      #             min = 1.0, max = 100.0, value = 50.0),
      # sliderInput(inputId = "p13", label = "cperc:",
      #             min = 0.0, max = 8.0, value = 4.0), 
      # sliderInput(inputId = "p14", label = "bmax:",
      #             min = 0.0, max = 30.0, value = 15.0),
      # sliderInput(inputId = "p15", label = "croute:",
      #             min = 0.0, max = 50.0, value = 25.0)
    ),
    
    # Show a plot of discharge
    mainPanel(
      plotOutput("Qplot")
    )
  )
)





#! ------------------------------------- Server

server <- function(input, output, session) {
  
  #! ------------- Dataset choice
  
  observeEvent(input$dataLoad, {
    load(input$dataLoad$name)
    updateSelectInput(session, inputId = "dataChoice",
                      choices = names(basin_list))
  })
  
  
  
  #! ------------- Data preparation
  
  getBV <- reactive({
    dataBV <- basin_list[[input$dataChoice]]
    paramIni <- dataBV$paramTUW
    return(list(ts = dataBV$ts, area = dataBV$area, paramIni = paramIni))
  })
  
  
  
  #! ------------- Run TUW model
  
  getTUW <- eventReactive({input$dataLoad ; input$dataChoice ;
    input$p01 ; input$p02 ; input$p03 ; input$p04 ; input$p05}, {
    # input$p01 ; input$p02 ; input$p03 ; input$p04 ; input$p05 ;
    # input$p06 ; input$p07 ; input$p08 ; input$p09 ; input$p10 ;
    # input$p11 ; input$p12 ; input$p13 ; input$p14 ; input$p15}, {
    param <- c(input$p01, input$p02, input$p03, input$p04, input$p05,
               getBV()$paramIni[-c(1:5)])
    # param <- c(input$p01, input$p02, input$p03, input$p04, input$p05,
    #            input$p06, input$p07, input$p08, input$p09, input$p10,
    #            input$p11, input$p12, input$p13, input$p14, input$p15)
    simLump <- TUWmodel::TUWmodel(
      prec  = getBV()$ts$prec,
      airt  = getBV()$ts$airt,
      ep    = getBV()$ts$ep,
      area  = getBV()$area,
      param = param)
    print(param)
    return(list(TUWout = simLump))
  })
  
  
  
  #! ------------- Update widgets
  
  #! Update parameters sliders when a datset is loaded
  observeEvent({input$dataLoad ; input$dataChoice}, {
    sapply(1:5, function(i) {
      updateSliderInput(session, inputId = sprintf("p%02i", i), value = getBV()$paramIni[i])
    })
  })
  
  
  
  #! ------------- Plots
  
  # Plot of discharge
  output$Qplot <- renderPlot({
    if (input$dataChoice != "") {
      k <- 1:300
      # print(tail(getTUW()$TUWout$q[1, ][k]))
      plot(getBV()$ts$qobs[k], type = "o", pch = 19, cex = 0.4, xlab = "Time", ylab = "Flow")
      lines(getTUW()$TUWout$q[k], col = "red", lty = 2, lwd = 2)
      legend("topright",
             legend = c("Obs", "Sim"), col = c("black", "red"),
             lty = c(1, 2), lwd = c(1, 2))
    } else {
      NULL
    }
  })
  
  
}




#! -------------------------------------  Run the application 

shinyApp(ui = ui, server = server)

