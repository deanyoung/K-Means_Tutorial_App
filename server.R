shinyServer(function(input, output) {
  
  glist <- reactive({
    
    km.iterate(mt,input$k)
    
  })
  
  output$max <- renderText({
    
    paste("Total Iterations:", as.character((length(glist()) - 1)/2))
    
  })
  
  output$graph <- renderPlot({
    
    glist()[counter]
    
  })
  
  output$status <- renderText({
    
    if(current.iter==0){
      
      "Current iteration: Initial random assignment"
      
    }else{
      
      paste("Current iteration:", current.iter, "Step:", current.step)
      
    }
    
  })
  
  observeEvent(input$forward,{
    
    if(counter < length(glist())){
      counter <<- counter + 1
      
      if(current.iter==0){
        current.iter <<- 1
        current.step <<- "a"
        
        
      }else{
        
        if(current.step=="a"){
          current.step <<- "b"
        }else{
          current.step <<- "a"
          current.iter <<- current.iter + 1
          
        }
        
        
      }
      
      
    }
    
    output$graph <- renderPlot({
      
      glist()[counter]
      
    })
    
    output$status <- renderText({
      
      if(current.iter==0){
        
        "Current iteration: Initial random assignment"
        
      }else{
        
        paste("Current iteration:", current.iter, "Step:", current.step)
        
      }
      
    })
    
  })
  
  observeEvent(input$backward,{
    
    if(counter > 1){
      counter <<- counter - 1
    }
    
    output$graph <- renderPlot({
      
      glist()[counter]
      
    })
    
    output$status <- renderText({
      
      if(current.iter==0){
        
        "Current iteration: Initial random assignment"
        
      }else{
        
        paste("Current iteration:", current.iter, "Step:", current.step)
        
      }
      
    })
    
  })
  
  
  
  
})