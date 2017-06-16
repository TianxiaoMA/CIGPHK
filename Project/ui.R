library(shiny)

# Define UI for the dashboard

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



shinyUI(
  fluidPage(
  
    # Application title
    headerPanel("Your Account @ CIGP"), 
   
    # sidebarLayout
    fluidRow(
    sidebarPanel(
      # Select elements to be shown
      selectInput("variable", "Variable:",
                c(   "Summary" = "Summary", 
                     "Holdings by Asset Class" = "PCAC",
                     "Holdings by Currency" = "PCC",
                     "Holdings by Geography" = "PCG",
                     "Historical Performance" = "HP",
                     "Historical Realized Volatility" = "HRV",
                     "List of all" = "List")
              )), 
    conditionalPanel(condition = "input.variable == 'List'",
                     fluidRow(
                       column(3,
                              selectInput("Asset.Class",
                                          "Asset Class:",
                                          c("All",
                                            unique(as.character(hist.data$Asset.class))))
                       ),
                       column(3,
                              selectInput("Geography",
                                          "Geography:",
                                          c("All",
                                            unique(as.character(hist.data$Geography))))
                       ),
                       column(3,
                              selectInput("Currency",
                                          "Currency:",
                                          c("All",
                                            unique(as.character(hist.data$Currency))))
                       )
                     )
  )
  ),
  fluidRow(
  column(4, ""),
  mainPanel(h3(textOutput("caption")))),
  fluidRow(
    conditionalPanel(condition = "input.variable != 'List'", plotOutput("Plot")),
    column(5, offset = 0.3,
    conditionalPanel(condition = "input.variable == 'List'", DT::dataTableOutput("table"))
    )
    )
  ))