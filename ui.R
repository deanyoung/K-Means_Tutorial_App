shinyUI(fluidPage(
  
  h1(strong("K-Means Tutorial App"), align="center"),
  
  sidebarLayout(
    sidebarPanel(
      h3(strong("Options")),
      sliderInput("k", "Select K", min = 1, 
                  max = 10, value = 3),
      actionButton("go","Submit")
      
    ),
    
    mainPanel()
  
  )
  
  
  
  
        )
)