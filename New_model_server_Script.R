
var_names_New_model <- eventReactive(
  
  c(input$dat_new_Model,
    input$shape_file_new_Model),{           
      
      req(input$shape_file_new_Model,input$dat_new_Model)
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
      
      
      inFile <- input$dat_new_Model
      data<-get_D_ata(inFile$datapath)
      #print("from vars_names")
      #print(head(data))
      #print("from vars_names...")
      #dist<-sort(unique(data$district))
      #names(data)
      #data<-data %>% arrange(district,year,week) 
      #choose  temp , rainfall
      
      inFile_shp <- input$shape_file_new_Model
      #print(inFile_shp)
      shp_pos1<-grep(".shp",inFile_shp$name)
      layer_nm<-str_remove(inFile_shp$name[shp_pos1],'.shp')
      #rename file
      
      #dist<-data$district
      #years.dat<-sort(unique(data$year))
      
      #output$valid_section<-renderUI(eval(parse(text=validation_tab_Func())))
      
      #print("layer_name")
      #print(layer_nm)
      ##get file with shp
      rename_fnm<-paste0(dirname(inFile_shp$datapath),'/',inFile_shp$name)
      file.rename(inFile_shp$datapath,rename_fnm)
      #ogrListLayers(inFile_shp$datapath[shp_pos])
      #list.files(dirname(inFile_shp$datapath))
      
      #pp<-readOGR(rename_fnm[shp_pos1],layer_nm,verbose =F)
      pp<-sf::st_read(rename_fnm[shp_pos1],layer_nm) |> 
        sf::as_Spatial()
      disTricts<-unique(data$district)
      disTricts_SHP<-unique(as.numeric(pp$district))
      
      districts_shape<-disTricts[which(disTricts%in%disTricts_SHP)]
      
      updateSelectInput(session,"district_new",choices=districts_shape,
                        selected =districts_shape[1])
      
      dat_A<-data %>% 
        dplyr::arrange(district,year,week) %>% 
        dplyr::filter(district %in% pp$district &!week==53)
      
      beg.year<-min(dat_A$year)
      end.year<-max(dat_A$year)
      
      dat_dist<-expand.grid(week=1:52,
                            year=sort(unique(dat_A$year)),
                            district=sort(unique(dat_A$district))) %>% 
        dplyr::select(district,year,week)
      
      data_augmented<-merge(dat_dist,dat_A,by=c("district","year","week"),all.x=T,sort=T) %>% 
        dplyr::mutate(district_w=paste0(district,'_',week)) %>% 
        dplyr::arrange(district,year,week)
      
      list(vars=names(data),dat=data,
           data_augmented=data_augmented,
           dists=dist,SHP=pp,
           districts_shape=districts_shape)
      
      
      
    })




observe({
  
  
  #al_vs<-grep("rain|temp|rainfall|precipitation|prec|humid|rh|humid|ovi|eggs",var_names()$vars,ignore.case=T)
  al_vs<-grep("rain|temp|rainfall|precipitation|prec",var_names_New_model()$vars,ignore.case=T)
  
  hos_vs<-grep("hosp",var_names_New_model()$vars,ignore.case=T)
  pop_vs<-grep("population|poblacion",var_names_New_model()$vars,ignore.case=T)
  
  #freezeReactiveValue(input, "alarm_indicators")
  #freezeReactiveValue(input, "number_of_cases")
  #freezeReactiveValue(input, "population")

  
  #output$dist_Input1<-renderUI(eval(parse(text=create_input_UI_district("district_new"))))
  #output$dist_Input2<-renderUI(eval(parse(text=create_input_UI_district("output_dist_seas"))))
  
  updateSelectInput(session,"alarm_indicators_New_model",choices=var_names_New_model()$vars,
                    selected =var_names_New_model()$vars[al_vs] )
  
  #updateSelectInput(session,"other_alarm_indicators_New_model",choices=var_names_New_model()$vars,
                    #selected ="rhdailymean" )
 
  updateSelectInput(session,"number_of_cases_New_model",choices=var_names_New_model()$vars,
                      #selected=var_names()$vars[3])
                      selected=var_names_New_model()$vars[hos_vs])
  updateSelectInput(session,"population_New_model",choices=var_names_New_model()$vars,
                      selected =var_names_New_model()$vars[pop_vs])
  
  updateSelectInput(session,"district_new",choices=var_names_New_model()$districts_shape,
                    selected =var_names_New_model()$districts_shape[1])
  
  updateSelectInput(session,"district_seas",choices=var_names_New_model()$districts_shape,
                    selected =var_names_New_model()$districts_shape[1])
  
  updateSelectInput(session,"district_validation",choices=var_names_New_model()$districts_shape,
                    selected =var_names_New_model()$districts_shape[1])
  
  #district_manage
  
  updateSelectInput(session,"district_manage",choices=var_names_New_model()$districts_shape,
                    selected =var_names_New_model()$districts_shape[1])
  
  
  updateSliderInput(session,"new_model_Year_plot",
                    min=min(var_names_New_model()$dat$year),
                    max=max(var_names_New_model()$dat$year),
                    value =max(var_names_New_model()$dat$year))
  
  updateSliderInput(session,"new_model_Year_validation",
                    min=min(var_names_New_model()$dat$year)+1,
                    max=max(var_names_New_model()$dat$year),
                    value =max(var_names_New_model()$dat$year))
  
  #value=max(var_names_spat()$dat$year)-1)
  
  updateSliderInput(session,"new_model_Week_plot_spat",
                    min=min(var_names_New_model()$dat$week),
                    max=max(var_names_New_model()$dat$week),
                    value =min(var_names_New_model()$dat$week))
  #value=max(var_names_spat()$dat$year)-1)
  
  # output$Uploaded_data<-DT::renderDataTable(DT::datatable(var_names_New_model()$dat,
  #                                                         options = list(autoWidth = TRUE,
  #                                                                        searching = T)))
  boundary_file<-var_names_New_model()$SHP
  
  
  population<-input$population_New_model
  pop.var.dat<-input$population_New_model
  alarm_indicators<-input$alarm_indicators_New_model
  other_alarm_indicators<-input$other_alarm_indicators_New_model
  number_of_cases<-input$number_of_cases_New_model
  base_vars<-c("district","year","week")
  
  covar_to_Plot<-c(number_of_cases,pop.var.dat,alarm_indicators)
  names_cov_Plot<-c("Cases","Population",alarm_indicators)
  
  sel_var_endemic<-c(base_vars,number_of_cases,population)
  #cat(names_cov_Plot,sep=' \n')
  #stop("helo")
  
  covar_to_Plot<-c(number_of_cases,pop.var.dat,alarm_indicators)
  
  
  dat_A<-var_names_New_model()$dat %>% 
    dplyr::arrange(district,year,week) %>% 
    dplyr::filter(district %in% boundary_file$district &!week==53)
  source("New_model_helper_Functions_v2.R",local =T,echo=F)
  #dplyr::filter(district %in% c(3,15) &!week==53)
  
  alarm_indicators<-input$alarm_indicators_New_model
  cat(paste('\nAlarm indicators now ..\n'),paste(input$alarm_indicators_New_model,collapse =','),'\n\n')
  alarm.indicators<-alarm_indicators
  
  dat_slider<-dat_A
  
  all_slice<-foreach(a=1:length(alarm_indicators))%do% get_fluid_slice_Output(a,'alarm.indicators')
  
  
  
  output$var_Slices_Plots<-renderUI(eval(parse(text=paste('tabsetPanel(',paste(unlist(all_slice),collapse =','),')'))))
  
  ##plot the shapefile and render the data
  
  ##update the risk reactive values
  
  par_text0<-get_UI_update_d(obj.plots=covar_to_Plot,
                             panel_Update="Descriptive Plots" ,
                             shinyOutputType="plotOutput",
                             cols=2,
                             out_ref="descr")
  
  output$new_model_data_descriptive_Plots<-renderUI({
    tagList(eval(par_text0))
  })
  
  par_text2<-get_UI_update_d(obj.plots=covar_to_Plot,
                             panel_Update="Time_series" ,
                             shinyOutputType="dygraphOutput",
                             cols=1,
                             out_ref="time_series")
  
  output$new_model_Time_series_Plots<-renderUI({
    tagList(eval(par_text2))
  })
  
  
  par_text1<-get_UI_update_d(obj.plots=covar_to_Plot,
                             panel_Update="Spat_Covariate_Plots_new_Model" ,
                             shinyOutputType="leafletOutput",
                             cols=2,
                             out_ref="spat_cov")
  
  
  output$Spat_Covariate_Plots_new_Model<-renderUI({
    tagList(eval(par_text1))
  })
  
  
  
}
) 




