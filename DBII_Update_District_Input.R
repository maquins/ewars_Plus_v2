
Uploaded_dat_DBII <- eventReactive(
  
  c(input$dat_prospective),{           
      
      req(input$dat_prospective)
      #req()
      #req(input$spat_Input_Type)
      
      
    get_D_ata<-function(p){
      if(str_detect(p,".xlsx$")){
        data <- data.frame(read_xlsx(p,sheet=1),stringsAsFactors =F)
      }
      else if(str_detect(p,".xls$")){
        data <- data.frame(read_xls(p,sheet=1),stringsAsFactors =F)
      } 
      else if(str_detect(p,".csv$")){
        data <- read.csv(p,header =T,stringsAsFactors =F)
      } else{
        data <- data.frame(read_xlsx("Demo_Data.xlsx",sheet=1),stringsAsFactors =F)
      }
      data
    }
    
    #inFile_a <- input$dat_new_Model
    inFile_b <- input$dat_prospective
    
    data<-get_D_ata(inFile_b$datapath)
    disTricts<-sort(unique(data$district))


      updateSelectInput(session,"district_prospective",choices=disTricts,
                        selected =disTricts[1])
      
      list(data=data,dists=sort(unique(data$district)))
      
      
      
    })




observe({
  
  dists_pros<-Uploaded_dat_DBII()$dists
  updateSelectInput(session,"district_prospective",
                    choices=dists_pros,
                    selected =dists_pros[1])
  
  # dists_pros<-Uploaded_dat_DBII()$dists
  # 
  # updateSelectInput(session,"district_prospective",
  #                   choices=15:20,
  #                   selected =15)
  
  
  
}
) 




