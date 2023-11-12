
#nlag<-12

#alarm_var<-"rainsum"
#dat<-"data_augmented"

#get_cross_basis<-function(alarm_var,dat){
get_cross_basis<-function(alarm_var,data_b=data.basis,nlag=nlag){
  #dat.N<-get(dat)[,c('district',alarm_var)]
  dat.N<-data_b[,c('district',alarm_var)]
  names(dat.N)[2]<-"alarm_var"
  
  if(str_detect(alarm_var,"rain|prec")){
    lag_knots<-equalknots(0:nlag, 3)
  }else{
    lag_knots<-nlag/2
  }
  
  lag_var<- tsModel::Lag(dat.N$alarm_var, group = dat.N$district, k = 0:nlag)
  
  basis_var <- crossbasis(lag_var,
                            argvar = list(fun = "ns", knots = equalknots(dat.N$alarm_var, 2)),
                            arglag = list(fun = "ns", knots = lag_knots))
  
  colnames(basis_var) = paste0("basis_",alarm_var,'.', colnames(basis_var))
  basis.var<-list(basis_var)
  names(basis.var)<-paste0("basis_",alarm_var)
  basis.var
}


precision.prior <<- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))


mymodel <- function(formula, data = df, family = "nbinomial", config = FALSE)
  
{
  model <- inla(formula = formula, data = data, family = family, offset = log(E),
                control.inla = list(strategy = 'adaptive'), 
                control.compute = list(dic = T, config = config, 
                                       cpo = F, return.marginals = F),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = F)
 # model <- inla.rerun(model)
  gc()
  return(model)
}



## UI ouput function

get_UI_update_d<-function(obj.plots,panel_Update,shinyOutputType,cols,
                          out_ref){
  
  dat_cont_UI<-data.frame(var=obj.plots,num=1:length(obj.plots))
  
  #length(covar_to_Plot)/2
  
  max_groups<-ceiling(length(obj.plots)/cols)
  
  dat_cont_UI$group=expand.grid(a=1:cols,b=1:max_groups)[,2][1:length(obj.plots)]
  
  
  dat_cont_UI$plot_Output<-paste0(out_ref,"_plot_",dat_cont_UI$num)
  
  if(cols==1){
    column_size<-12
  }else{
    column_size<-6 
  }
  #t<-1
  get_Fluid<-function(t){
    dat_For_Ui1<-dat_cont_UI %>% dplyr::filter(group==t)
    
    if(cols==2){
      
      dd<-paste("fluidRow(",
                paste0('column(',column_size,',',shinyOutputType,'("',dat_For_Ui1$plot_Output[1],'")),'),
                paste0('column(',column_size,',offset =0,',shinyOutputType,'("',dat_For_Ui1$plot_Output[2],'")))')
      )
    }else{
      dd<-paste("fluidRow(",
                paste0('column(',column_size,',',shinyOutputType,'("',dat_For_Ui1$plot_Output[1],'")))')
      )
    }
    #DT::dataTableOutput("Uploaded_data")
    dd
  }
  
  all_out<-foreach(a=1:max_groups,.combine =c)%do% get_Fluid(a)
  
  
  par_text0<-parse(text=
                     paste0('tabPanel("',panel_Update,'",',
                            paste(all_out,collapse =',')
                            , ')'))
  
  par_text0
  
}

## update log plots UI

get_slider_input_lag<-function(var,pp){
  if(pp==1){
    offset_val<-0
  }else{
    offset_val<-4
  }
  
  aa<-tribble(~var,
              "column(12,",
              paste0('offset=',offset_val,","),
              paste0('sliderInput(inputId = "',var,'_',pp,'",'),
              paste0('label = "',"Var_",pp,'",'),
              paste0('min=',min,","),
              paste0('max=',max,","),
              paste0('value=',vals_beg[pp],","),
              paste0('step=',step.val,')'),
              
  )
  paste(aa$var,collapse =' ')
}
#all_basis_vars$basis_rainsum

get_fluid_slice_Output<-function(p,var.Obj){
  var<-get(var.Obj)[p]
    #dat_slider<-var_names_New_model()$dat
  min<-round(min(dat_slider[,var],na.rm=T),0)
  min.1<-min(dat_slider[,var],na.rm=T)
  max<-round(max(dat_slider[,var],na.rm=T),0)
  max.1<-max(dat_slider[,var],na.rm=T)
  
  val.slid<-pretty(min:max,50)
  id.rm<-which(val.slid<min.1|val.slid>max.1)
  if(length(id.rm)>0){
    val.sliders<-val.slid[-which(val.slid<min.1|val.slid>max.1)]
    
  }else{
    val.sliders<-val.slid
  }
  

  min.slid<-min(val.sliders,na.rm=T)
  max.slid<-max(val.sliders,na.rm=T)
  
  if(str_detect(paste0(val.sliders[2]),'[.]')){
    dec_points<-str_length(str_extract(val.sliders[2],'[.][:number:]+'))-1
  
  step.val<-round(unique(diff(val.sliders))[1],dec_points)
}else{
  step.val<-unique(diff(val.sliders))[1]
}  
  values_length<-1:length(val.sliders)
  
  sel_idx<-c(as.integer(quantile(values_length,0.25)),
             as.integer(quantile(values_length,0.35)),
             as.integer(quantile(values_length,0.95)))
  
  vals_beg<-val.sliders[sel_idx]
  
  get_slider_input_lag<-function(var,pp){
    if(pp==1){
      offset_val<-0
    }else{
      offset_val<-4
    }
    
    aa<-tribble(~var,
                "column(12,",
                paste0('offset=',offset_val,","),
                paste0('sliderInput(inputId = "',var,'_',pp,'",'),
                paste0('label = "'," ",'",'),
                paste0('min=',min.slid,","),
                paste0('max=',max.slid,","),
                paste0('value=',vals_beg[pp],","),
                paste0('step=',step.val,'))'),
                
    )
    paste(aa$var,collapse =' ')
  }
                    
  
  all_slider<-foreach(a=1:3)%do% get_slider_input_lag(var,a)
  cmd_str<-paste0('tabPanel("',var,'_slice",',
                  paste('inputPanel(',paste(unlist(all_slider),collapse =','),'),'),
                  paste0('plotOutput("',var,'_Slice_plot"))'))
  #eval(parse(text=cmd_str))
  cmd_str
}


##slider update finction

update_slider_vals<-function(tt,var.Obj){
  Var<-get(var.Obj)[tt]
  #dat_slider<-var_names_New_model()$dat
  min1<-min(dat_slider[,Var],na.rm=T)
  max1<-max(dat_slider[,Var],na.rm=T)
  
  val.slid<-round(seq(min1,max1+5,length=200),2)
  
  min<-min(val.slid,na.rm=T)
  max<-max(val.slid,na.rm=T)
  
  
  sel_idx<-c(as.integer(quantile(1:200,0.25)),
             as.integer(quantile(1:200,0.75)),
             as.integer(quantile(1:200,0.95)))
  
  vals_beg<-val.slid[sel_idx]
  
  step.val<-max(diff(seq(min,max+5,length=200)))
  
  slider_up_lag<-function(var,pp){
    var1<-paste0(var,'_',1:3)
    aa<-tribble(~var,
                paste0('updateSliderInput(session,'),
                paste0('"',var1[pp],'",'),      
                paste0('min=',min,","),
                paste0('max=',max,","),
                paste0('value=',vals_beg[pp],","),
                paste0('step=',step.val,')'),
                
    )
    paste(aa$var,collapse =' ')
  }
  
  
  all_slider<-foreach(a=1:3)%do% slider_up_lag(Var,a)
  
  cmd_str<-unlist(all_slider)
  #eval(parse(text=cmd_str))
  cmd_str
}

output_dist_new_model<-selectInput(inputId = 'district_new',
                                   label = 'District',
                                   choices = c(3:20),
                                   selected =20,
                                   selectize =T,
                                   multiple =F)
var<-'district_new'
#dist<-20:25


create_input_UI_district<-function(var){
  dTT1<<-sort(unique(dat_slider$district))
  dTT<<-dTT1[dTT1%in% bound_Data_Districts]
  aa<-tribble(~var,
              paste0('selectInput(inputId="',var,'",'),
              "label = 'District',",      
              'choices=dTT,',
              'selected =dTT[1],',
              'selectize =T,',
              'multiple =F)',
              
  )
  aa$var
  paste(aa$var,collapse =' ')
}

create_input_UI_district_pros<-function(var){
  # dat_Slid_districts<-isolate(UI_reactive_objs$dat_slider$district)
  # dTT1<-sort(unique(dat_Slid_districts))
  # dTT<-dTT1[dTT1%in% bound_Data_Districts]
  aa<-tribble(~var,
              paste0('selectInput(inputId="',var,'",'),
              "label = 'District',",      
              'choices=dTT,',
              'selected =dTT[1],',
              'selectize =T,',
              'multiple =F)',
              
  )
  aa$var
  paste(aa$var,collapse =' ')
}

#create_input_UI_district(var)
year_validation<-sliderInput(inputId = "new_model_Year_validation",
                             label = "Year",
                             min = 2008,
                             max=2030,
                             value =2013,
                             sep='',
                             step=1)

years.dat<-2012:2020

create_input_UI_year<-function(var){
  years.dat<-sort(unique(dat_slider$year))
  aa<-tribble(~var,
              paste0('sliderInput(inputId="',var,'",'),
              "label = 'Year',",      
              'min=min(years.dat)+1,',
              'max=max(years.dat),',
              'value =min(years.dat)+1,',
              'sep="",',
              'step=1)',
              
  )
  aa$var
  paste(aa$var,collapse =' ')
}

create_input_UI_year2<-function(var){
  years.dat<-sort(unique(dat_slider$year))
  aa<-tribble(~var,
              paste0('sliderInput(inputId="',var,'",'),
              "label = 'Year',",      
              'min=min(years.dat),',
              'max=max(years.dat),',
              'value =min(years.dat),',
              'sep="",',
              'step=1)',
              
  )
  aa$var
  paste(aa$var,collapse =' ')
}

#create_input_UI_year2("aa")

#output$dist_Input1<-renderUI(eval(parse(text=create_input_UI_district("district_new"))))
#output$dist_Input2<-renderUI(eval(parse(text=create_input_UI_district("output_dist_seas"))))
#output$dist_Input3<-renderUI(eval(parse(text=create_input_UI_district("output_dist_validation"))))
#output$Year_Input1<-renderUI(eval(parse(text=create_input_UI_district("new_model_Year_validation"))))
#output$Year_Input2<-renderUI(eval(parse(text=create_input_UI_year2("new_model_Year_plot"))))

validation_tab_Func<-function(){
  dist1<-sort(unique(dat_slider$district))
  dist<-dist1[which(dist1%in% bound_Data_Districts)]
  
  years.dat<-sort(unique(dat_slider$year))
  
  dist_in<<-eval(parse(text=create_input_UI_district("district_validation")))
  year_in<<-eval(parse(text=create_input_UI_year("new_model_Year_validation")))
  validation_Tab<-tribble(~var,
                          'fluidPage(inputPanel(column(12,offset=0,',
                          'dist_in),',
                          'column(12,offset=4,',
                          'year_in),',
                          'column(12,offset=4,z_outbreak_New)),',
                          'tabsetPanel(tabPanel("Runin period",',
                          'plotOutput("runin_ggplot_New_model"),',
                          'dygraphOutput("runin_interactive_New_model"),id="to_show"),',
                          'tabPanel("Validation_period",',
                          'tabsetPanel(',
                          'tabPanel("Plots",',
                          'plotOutput("validation_ggplot_New_model"),',
                          'dygraphOutput("validation_interactive_New_model")),',
                          'tabPanel("Sensitivity/Specificity",',
                          'tableOutput("sen_spec_table_New_model")',
                          ')',
                          ')',
                          '))',
                          ')',
  )
  #validation_Tab$var
  paste(validation_Tab$var,collapse =' ')
}

#eval(parse(text=validation_tab_Func()))
