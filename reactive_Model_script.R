
eventReactive(c(
 
  input$run_mod
),
  {
    #req(c(input$alarm_indicators_New_model,input$nlags))
    req(c(input$alarm_indicators_New_model,input$nlags,input$new_model_Year_validation))
    #source("New_model_helper_Functions.R",local =T,echo=T)
    cat(paste('\nValidation year now ..\n',input$new_model_Year_validation),'\n\n')
    
    #source("New_model_helper_Functions.R")
   
    
    
    
    
    
  },
  ignoreNULL=F)
