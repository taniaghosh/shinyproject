fluidPage(
  titlePanel("Healthy Diet and COVID"),
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      conditionalPanel(condition = "input.tabselected == 1",
      selectInput(inputId = "continent", label = h3("Continent"), 
                  choices = unique(food$Continent))),
      
      conditionalPanel(condition = "input.tabselected == 2",
      selectInput(inputId = "country", label = h3("Country"), 
                  choices = unique(food_country$Country))),
      conditionalPanel(condition = "input.tabselected == 3",
      selectInput(inputId = "Continent", label = h3("Continent"), 
                  choices = unique(food$Continent))),
      conditionalPanel(condition = "input.tabselected == 4",
      selectInput(inputId = "obesity_level", label = h3("Obesity"), 
                  choices = unique(food_obes$obesity_level))),
      conditionalPanel(condition = "input.tabselected == 5",
      selectInput(inputId = "undernorished", label = h3("Undernorished"), 
                  choices = unique(food_UN$UN_rate))),
      conditionalPanel(condition = "input.tabselected == 7",
      selectInput(inputId = "variable", label = h3("Variable"), 
                  choices =colnames(food[, -c(1,2)]))),
      
      
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs", id="tabselected", selected = 1,
        tabPanel("Food categories and COVID", plotOutput("plot"),value = 1,
                 fluidRow(
                          column(6,plotOutput("plot2"), value = 1), 
                          column(6,plotOutput("plot3"), value = 1)),
        ),
                 
        tabPanel("Food profile of Countries", plotOutput("plot4"), value = 2),
        tabPanel("Alcohol vs Covid", plotOutput("plot5"), plotOutput("plot6"), value = 3),
        tabPanel("Obesity", plotOutput("plot7"), value = 4),
        tabPanel("Undernorishment", plotOutput("plot8"), value = 5),
        tabPanel("Correlation", value = 6,
                 fluidRow(
                   column(6,plotOutput("plot9")),
                   column(6,plotOutput("plot10"))),
        ),
        tabPanel("Summary Statistics of Variables", tableOutput("table"), value = 7),
        tabPanel("Conclusions", img(src = "healthydiet.jpeg", width="50%", align="right"),img(src = "covid.png", width="50%", align="left"), 
        h3(textOutput("text")), h4(textOutput("text1"), h4(textOutput("text2")), h4(textOutput("text3")), h4(textOutput("text4"))))
       
        
      
        
         
        
      )
    )
  )
)
