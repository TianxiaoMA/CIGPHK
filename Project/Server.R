library(shiny)
library(datasets)
library(ggplot2)
library(DT)



# Preprocess to the database

csv.path <- paste(path, "/AAPL.csv", sep = '')
raw.data <- read.csv(csv.path)
raw.data$num <- c(1:nrow(raw.data))
raw.data$RPrice <- raw.data$Price/raw.data$Daily.exchange.rate # Compute price in ref currency
current.data <- raw.data[raw.data$Date == "2016/12/30", ] # Need to define current date
data <- current.data
hist.data <- raw.data[raw.data$Date != "2016/12/30", ] # "2016/12/30" to avoid some confliction between my random number

for (j in min(hist.data$Position) : max(hist.data$Position)){
  Position.data <- hist.data[hist.data$Position == j, ]
  for(i in 1:length(Position.data$Price)){
  Position.data$volatility[i] <- var(Position.data$RPrice[1:i])
  hist.data$volatility[hist.data$Position == j][i] <- Position.data$volatility[i]
  }
}
raw.data$Position <- factor(raw.data$Position)
raw.data$Asset.class <- factor(raw.data$Asset.class)
raw.data$Currency <- factor(raw.data$Currency)
raw.data$Geography <- factor(raw.data$Geography)

hist.data$Position <- factor(hist.data$Position)
hist.data$Asset.class <- factor(hist.data$Asset.class)
hist.data$Currency <- factor(hist.data$Currency)
hist.data$Geography <- factor(hist.data$Geography)

# Define server logic required to plot various variables
shinyServer(function(input, output) {
  
  formulaText <- reactive({
    paste("Investment -", input$variable) 
  })
  # Return the formula text for printing as a caption
  output$caption <- renderText({
        formulaText()
  })
  
  
  # List with all holdings and investments, with current market price, individual performance since investment, 
  # with filters by asset class, geography, sectors, currency
  
  # Generate a list of the requested variable
  output$table <- DT::renderDataTable(
    DT::datatable(
      if(input$variable == "List"){
        if (input$Asset.Class != "All") {
        data <- data[data$Asset.class == input$Asset.Class, ]
        }
        
        if (input$Geography != "All") {
        data <- data[data$Geography == input$Geography, ]
        }
        
        if (input$Currency != "All") {
        data <- data[data$Currency == input$Currency, ]
        }
        
        data
        }
      )
    )

          
 
      

  # Generate a plot of the requested variable
    output$Plot <- renderPlot({
      if(input$variable == "Summary"){
      p = ggplot(current.data, aes(x = Asset.class, y = current.data$RPrice, fill = Geography)) +
        geom_bar(stat = "identity")
      p
      # Summary top line with : Total assets in ref currency, MTD performance, YTD Performance, Volatility aggregate;
    }
    else{
      if("PCAC" %in% input$variable){
        p = ggplot(current.data, aes(x = "", y = current.data$RPrice, fill = Asset.class)) + 
          geom_bar(stat = "identity", position = 'stack', width = 1) + 
          coord_polar(theta = "y") +
          xlab("") + ylab("")
        p
        # Pie chart with holdings by asset class (in %); Value: A-B
      }
      else{
        if("PCC" %in% input$variable){
          p = ggplot(current.data, aes(x = "", y = current.data$RPrice, fill = Currency)) + 
            geom_bar(stat = "identity", position = 'stack', width = 1) + 
            coord_polar(theta = "y") +
            xlab("") + ylab("")
          p
          # Pie char with holdings by currency (in%); Value: f-g
        }
        else{
          if("PCG" %in% input$variable){
            p = ggplot(current.data, aes(x = "", y = current.data$RPrice, fill = current.data$Geography)) + 
              geom_bar(stat = "identity", position = 'stack', width = 1) + 
              coord_polar(theta = "y") +
              xlab("") + ylab("")
            p
            # Pie chart with holdings by geography (in %); Value: a-b
          }
          else{
            if("HP" %in% input$variable){
              p <- ggplot(hist.data, aes(x = num, y = RPrice, group = Geography, color = Position)) + geom_line() +
              theme(axis.text.x = element_blank())
              p 
              # Line chart with historical performance of individual positions; Value: 6-7
            }
            else{
              if("HRV" %in% input$variable){
                p <- ggplot(hist.data, aes(x = num, y = volatility, group = Geography, color = Position)) + geom_line() +
                  theme(axis.text.x = element_blank())
                p 
                #  Line chart with historical realized volatility of individual position and or aggregate portfolio;
              }
              else{}
          }
        }
      }
    }
    }         
  })

  })