


base_vars<-c("district","year","week")
boundary_file<-var_names_New_model()$SHP
alarm_vars<-input$alarm_indicators_New_model

population<-input$population_New_model
pop.var.dat<-input$population_New_model
alarm_indicators<-input$alarm_indicators_New_model
alarm_vars<-alarm_indicators
other_alarm_indicators<-input$other_alarm_indicators_New_model
number_of_cases<-input$number_of_cases_New_model
new_model_Year_validation<-input$new_model_Year_validation
nlag<-input$nlags

covar_to_Plot<-c(number_of_cases,pop.var.dat,alarm_indicators)
names_cov_Plot<-c("Cases","Population",alarm_indicators)

sel_var_endemic<-c(base_vars,number_of_cases,population)
#cat(names_cov_Plot,sep=' \n')
#stop("helo")

covar_to_Plot<-c(number_of_cases,pop.var.dat,alarm_indicators)




##Compute endemic Channel

years_dat<-sort(unique(data_augmented0$year))

year_ranges<-range(years_dat)
date_Beg<-as.Date(paste0(year_ranges[1],'-01-01'))
date_end<-as.Date(paste0(year_ranges[2],'-12-31'))


beg.year<-year_ranges[1]
end.year<-year_ranges[2]


dates_year_Week<-data.frame(date=seq.Date(date_Beg,date_end,by="week")) |> 
  dplyr::mutate(year=year(date),
                week=week(date))
#new_model_Year_validation

get_endemic<-function(Dat_end,Year){
  
  Dat_end %>% 
    #dplyr::filter(!year==Year & !is.na(Cases) ) %>% 
    dplyr::filter(year<new_model_Year_validation ) %>% 
    dplyr::mutate(rate=(Cases/Pop)*1e5) %>% 
    dplyr::group_by(district,week) %>% 
    dplyr::summarise(.groups="drop",
                     mean_cases=mean(Cases,na.rm =T),
                     mean_rate=mean(rate,na.rm =T),
                     sd_cases=sd(Cases,na.rm =T),
                     sd_rate=sd(rate,na.rm =T)) %>% 
    dplyr::mutate(year=Year) %>% 
    dplyr::select(district,year,week,mean_cases,sd_cases,mean_rate,sd_rate)
}

all_endemic<-foreach(Yr=years_dat,.combine =rbind)%do% get_endemic(data_augmented0,Yr)

data_augmented<-data_augmented0 |> 
  dplyr::left_join(all_endemic,by=c("district","year","week")) |> 
  dplyr::left_join(dates_year_Week,by=c("year","week"))


Dist_IDS<-boundary_file@data |> 
  dplyr::mutate(district=as.numeric(district),
                ID_spat=1:n(),
                ID_spat1=1:n(),
                ID_spat2=1:n()) |> 
  dplyr::select(district,ID_spat,ID_spat1,ID_spat2)



Year_IDS<-data.frame(year=sort(unique(data_augmented$year))) |> 
  dplyr::mutate(ID_year=(1:n()),
                ID_year1=(1:n()),
                ID_year2=(1:n()))

summary_combs<-data.frame(beg=seq(1,12,4),
                          end=seq(4,12,4))

get_lag_Summaries<-function(pp,Lag_Matrix,lag_Pref){
  
  col_idx<-summary_combs[pp,1]:summary_combs[pp,2]
  lag_beg<-summary_combs[pp,1]
  lag_end<-summary_combs[pp,2]
  Lag_name<-paste0(lag_Pref,lag_beg,'_',lag_end)
  
  if(lag_Pref=="meantemperature"){
    var.sum<-apply(Lag_Matrix[,col_idx],1,FUN=function(x) mean(x,na.rm=T))
  }else{
    var.sum<-apply(Lag_Matrix[,col_idx],1,FUN=function(x) sum(x,na.rm=T))
    
  }
  var_sum<-data.frame(var=var.sum)
  names(var_sum)<-Lag_name
  var_sum
}



vars_Base<-c("district","year","week","date","Cases",
             "mean_cases","sd_cases",
             "mean_rate","sd_rate",
             "Pop","log_Pop")

Get_alarm_vars<-function(pp){
  
  dat_Lg<-data_augmented |> 
    #dplyr::filter(data_augmented[[alarm_vars[pp]]]==0) |> 
    dplyr::rename(alarm_var=alarm_vars[pp]) |> 
    dplyr::select(district,year,week,alarm_var)
  names(dat_Lg)
  
  Var_lags<-tsModel::Lag(dat_Lg$alarm_var, group = dat_Lg$district, k = 1:nlag) |> 
    data.frame()
  
  names(Var_lags)<-paste0(alarm_vars[pp],"_LAG",1:nlag)
  
  ## compute Lags Summaries
  
  #Var_lag_summaries<-foreach(aa=1:nrow(summary_combs),.combine =cbind)%do% get_lag_Summaries(aa,Var_lags,paste0(alarm_vars[pp],"_LAG"))
  
  if(pp==1){
    #all_Lags<-cbind(data_augmented[,vars_Base],Var_lags,Var_lag_summaries)
    all_Lags<-cbind(data_augmented[,vars_Base],Var_lags)
    
    
  }else{
    #all_Lags<-cbind(Var_lags,Var_lag_summaries)
    all_Lags<-Var_lags
    
    
  }
  all_Lags
  
}

cat(alarm_vars,sep='\n')

Model_data_lags<-foreach(aa=1:length(alarm_vars),.combine =cbind)%do% Get_alarm_vars(aa)

## Compute by district here

Dat_mod<-Model_data_lags |> 
  #data.frame() |> 
  dplyr::left_join(Dist_IDS,by="district") |> 
  dplyr::left_join(Year_IDS,by="year")


#cat(names(Dat_mod),sep='\n')


alarm_vars<-alarm_indicators


## run by District

all_districts<-unique(data_augmented$district)

#DD<-1

DIC1_run<-T
DIC2_run<-T
CV_run<-T



out_Path<-file.path(getwd(),'Outputs')


all_files_Path<-file.path(out_Path,"For_Shiny","All_district")



fold_CR1<-file.path(out_Path,"For_Shiny","All_district")
if(!dir.exists(fold_CR1)){
  dir.create(fold_CR1,recursive =T)
}




Time_one_Dist<-system.time({
  
  
  
  for (DD in 1:length(all_districts)){
    
    District_Now<-all_districts[DD]
    
    one_of_dist_str<-paste0('(',DD,' of ',length(all_districts),' districts)')
    
    
    
    #pp<-1
    #names(data_augmented)
    
    Dat_mod_sub<-Dat_mod |> 
      dplyr::filter(district==District_Now)
    
    Model_data_lags_sub<-Model_data_lags |> 
      dplyr::filter(district==District_Now)
    
    
    get_Model<-function(form_mod,mod_Family,Dat_m) {
      
      mod<-inla(form_mod,
                offset =log_Pop,
                data=Dat_m,
                family =mod_Family,
                #control.inla = list(strategy = 'adaptive'), 
                control.inla = list(strategy = 'simplified.laplace',
                                    parallel.linesearch=F,
                                    improved.simplified.laplace=F), 
                control.compute = list(dic = TRUE, 
                                       config = T, 
                                       cpo = TRUE, 
                                       waic=T,
                                       #po=T,
                                       return.marginals = F,
                                       smtp="taucs"
                ),
                num.threads="2:1",
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = F)
      mod
    }
    
    form_baseline<- Cases ~ 1 +
      f(ID_spat,model='iid',replicate=ID_year)+
      f(week,model='rw1',cyclic=T,scale.model =T)
    
    
    #Dat_mod_sub_c<<-Dat_mod_sub
    
    list_out_mod<<-list(form_baseline=form_baseline,
                       Dat_mod_sub=Dat_mod_sub,
                       get_Model=get_Model)
    
    baseline_model<-get_Model(form_baseline,"nbinomial",Dat_mod_sub)
    
    summary(baseline_model)
    
    baseline_model$summary.hyperpar$mean
    
    
    ## compare different 
    
    theta_beg<-baseline_model$internal.summary.hyperpar
    
    
    Sel_Vars<-function(form_mod,mod_Family,mod.Data,start_again) {
      
      mod<-inla(form_mod,
                offset =log_Pop,
                data=mod.Data,
                family =mod_Family,
                control.inla = list(strategy = 'simplified.laplace',
                                    parallel.linesearch=F,
                                    improved.simplified.laplace=F), 
                control.compute = list(dic = TRUE, 
                                       config = T, 
                                       cpo = TRUE, 
                                       waic=T,
                                       #po=T,
                                       return.marginals = F,
                                       smtp="taucs"
                ),
                num.threads="2:1",
                control.mode = list(theta = theta_beg$mean, restart = start_again),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = F)
      mod
    }
    
    
    id_lag<-grep("LAG",names(Model_data_lags_sub),ignore.case =T)
    
    lag.vars<-names(Model_data_lags_sub)[id_lag]
    
    
    folders_Create<-c("Lag_selection","Lag_Comb_selection","Weekly_crossValidations","For_Shiny","For_Shiny_DBII","Prediction_weights")
    folder_Vars<-c("path_dic1","path_dic2","cv_path","shiny_obj_pth","shinyDBII_obj_pth","pred_weights_pth")
    
    #ff<-1
    
    for (ff in 1:length(folders_Create)){
      
      dist_padded<-stringr::str_pad(District_Now,width =3,pad=0,side='left')
      
      dist_Folder<-paste0("District_",dist_padded)
      
      dir_Now<-file.path(out_Path,folders_Create[ff],dist_Folder)
      
      if(!dir.exists(dir_Now)){
        dir.create(dir_Now,recursive =T)
      }
      
      assign(folder_Vars[ff],dir_Now)
      
    }
    
    
    #unlink(list.files(path_dic1,full.names =T))
    
    Run_DIC1<-DIC1_run
    
    if(Run_DIC1){
      time_dic1<-system.time({
        #compare_DIC<-function(ss){
        #Header_progress<-paste0("Running lags district:",District_Now,'br()',one_of_dist_str,'br()')
        Header_progress<-paste0("Running lags district:",District_Now,'\n',one_of_dist_str,'\n')
        p_progress <- Progress$new(min=0,max=length(lag.vars))
        p_progress$set(message =Header_progress ,value=0)
        
        cat("Running lags ...",sep='\n')
        
        for(ss in 1 :length(lag.vars)){
          gc()
         # cat(ss,sep='\n')
          cat(paste0(ss," "),sep=' ')
          
          form_str <- as.formula(paste("Cases ~ 1 +f(ID_spat,model='iid',replicate=ID_year)+f(week,model='rw1',cyclic=T,scale.model =T)+",
                                       lag.vars[ss],collapse =""))
          
          model_Out<-Sel_Vars(form_str,"nbinomial",Dat_mod_sub,T)
          summary(model_Out)
          
          Dic_tab<-data.frame(var=lag.vars[ss],DIC=model_Out$dic$dic,
                              coeff=model_Out$summary.fixed$mean[2],
                              low=model_Out$summary.fixed$`0.025quant`[2],
                              high=model_Out$summary.fixed$`0.975quant`[2],
                              logcpo=-mean( log(model_Out$cpo$cpo) ,na.rm=T)
          )
          ob_nam<-paste0("Lag_Dic_tab",ss)
          saveRDS(Dic_tab,file.path(path_dic1,ob_nam))
          one_of_str<-paste0('(',ss,' of ',length(lag.vars),')')
          mess_lag<-paste0(lag.vars[ss],' ',one_of_str)
          p_progress$set(value = ss, detail = mess_lag)
        }
        p_progress$close()
      })
      time_dic1[3]/60
    }
    
    #time_dic1<-system.time({
    
    #})
    
    
    
    #Lag_dic_comp<-foreach(aa=1:length(lag.vars),.combine =rbind)%do% compare_DIC(aa)
    Lag_dic_comp<-foreach(aa=1:length(lag.vars),.combine =rbind)%do% readRDS(file.path(path_dic1,paste0("Lag_Dic_tab",aa)))
    
    
    Var_ext_lag0<-paste(alarm_vars,collapse ="|")
    Var_ext_Lag<-paste(paste0(alarm_vars,'_LAG'),collapse ="|")
    
    
    Lag_dic_comp1<-Lag_dic_comp |> 
      dplyr::mutate(Variable=str_extract(var,paste(alarm_vars,collapse ="|")),
                    lag=str_remove(var,Var_ext_Lag)
      ) |> 
      dplyr::arrange(Variable,DIC) |> 
      #dplyr::filter(lag=="0") |> 
      dplyr::group_by(Variable) |> 
      dplyr::mutate(Rank=1:n())
    
    
    ## get best combination of LAG
    
    
    lag_Var_Dat<-data.frame(var=lag.vars) |> 
      dplyr::mutate(Variable=str_extract(var,Var_ext_Lag),
                    lag=str_remove(var,Var_ext_Lag))
    
    
    names(Lag_dic_comp1)
    
    ## chosen lags 
    
    Selected_lags<-Lag_dic_comp1 |> 
      dplyr::filter(Rank==1)
    
    
    
    if(length(alarm_vars)==2){
      
      Lags_to_Comb<-3
      
      Selected_lags_Comb<-Lag_dic_comp1 |> 
        dplyr::filter(Rank %in% 1:Lags_to_Comb)
      
      Lag_Vars1<-Selected_lags_Comb$var[Selected_lags_Comb$Variable==alarm_vars[1]]
      Lag_Vars2<-Selected_lags_Comb$var[Selected_lags_Comb$Variable==alarm_vars[2]]
      
      
      combs<-rbind(c(1,1),combn2(1:Lags_to_Comb),c(Lags_to_Comb,Lags_to_Comb))
      
      lag_combs<-data.frame(var1=Lag_Vars1[combs[,1]],
                            var2=Lag_Vars2[combs[,2]])
      
      
      
      
      #unlink(list.files(path_dic2,full.names =T))
      
      Run_DIC2<-DIC2_run
      ##ss<-1
      if(Run_DIC2){
        time_dic2<-system.time({
          #compare_DIC_lag_Combs<-function(ss){
          Header_progress<-paste0("Running lag Combs district:",District_Now,' ',one_of_dist_str)
          p_progress <- Progress$new(min=0,max=nrow(lag_combs))
          p_progress$set(message =Header_progress ,value=0)
          
          cat("",sep='\n')
          cat("Running Lag combs ...",sep='\n')
          
          for (ss in 1 :nrow(lag_combs)){
            cat(paste0(ss," "),sep=',')
            gc()
            comb_str<-paste0(lag_combs[ss,],collapse ="+")
            form_str <- as.formula(paste("Cases ~ 1 +f(ID_spat,model='iid',replicate=ID_year)+f(week,model='rw1',cyclic=T,scale.model =T)+",
                                         comb_str,collapse =""))
            
            
            #cat(ss,sep='\n')
           
            
            ## select suitable lags based on  CV for 2019
            
            model_Out<-Sel_Vars(form_str,"nbinomial",Dat_mod_sub,T)
            summary(model_Out)
            
            Dic_tab<-data.frame(Lag_Comb=comb_str,
                                DIC=model_Out$dic$dic,
                                coeff=model_Out$summary.fixed$mean[2],
                                low=model_Out$summary.fixed$`0.025quant`[2],
                                high=model_Out$summary.fixed$`0.975quant`[2],
                                logcpo=-mean( log(model_Out$cpo$cpo) ,na.rm=T)
            )
            ob_nam<-paste0("Lag_comb_Dic_tab",ss)
            saveRDS(Dic_tab,file.path(path_dic2,ob_nam))
            mess_lag<-paste0(ss,' of ',nrow(lag_combs))
            p_progress$set(value = ss, detail = mess_lag)
            
          }
          p_progress$close()
        })
        
        time_dic2[3]/60
      }
      
      nrow(lag_combs)
      #compare_DIC_lag_Combs(44)
      
      
      Lag_combinations_dic<-foreach(aa=1:nrow(lag_combs),.combine =rbind)%do% readRDS(file.path(path_dic2,paste0("Lag_comb_Dic_tab",aa)))
      
      
      (Lag_combinations_dic1<-Lag_combinations_dic |> 
          dplyr::arrange(DIC))
      
      #Lag_combinations_dic1[1,]
      
      Selected_lag_Vars_a<-as.character(stringr::str_split(Lag_combinations_dic1$Lag_Comb[1],'[+]',simplify =T))
      
    }else{
      
      Selected_lag_Vars_a<-Selected_lags$var
    }
    
    Min_Sel_Lag<-min(as.numeric(str_extract(Selected_lag_Vars_a,'[:number:]+')))
    
    Selected_lag_Vars<-str_replace(Selected_lag_Vars_a,'[:number:]+',as.character(Min_Sel_Lag))
    
    vars_Base1<-c("district",'ID_spat',"year",'ID_year',"week","date","Cases",
                  "mean_cases","sd_cases",
                  "mean_rate" ,"sd_rate",
                  "Pop","log_Pop")
    
    
    Vars_Final<-c(vars_Base1,Selected_lag_Vars)
    
    names(Dat_mod)
    
    Dat_mod_Selected<-Dat_mod_sub |> 
      dplyr::select(all_of(Vars_Final))
    
    df_spline<-4
    
    ns_test<-list(Dat_mod_Selected=Dat_mod_Selected,
                   Selected_lag_Vars=Selected_lag_Vars)
    cat("",sep='\n')
    
    for (gg in 1:length(alarm_vars)){
      
      Spline_obj_Name<-paste0('Var',gg,'_Spline')
      
      Var_ns_pref<-paste0('Var',gg)
      
      Var_Cre_bs1<-Dat_mod_Selected[,Selected_lag_Vars[gg]]
      #sd(Var_Cre_bs1,na.rm = T)
      pctn_99<-quantile(Var_Cre_bs1,0.99,na.rm=T)
      max_Val<-max(Var_Cre_bs1,na.rm=T)
      
      outlier_ratio<-max_Val/pctn_99
      
      if(str_detect(Selected_lag_Vars[gg],'rain|prec') & outlier_ratio>2){
        Var_Cre_bs<-Var_Cre_bs1
        Var_Cre_bs[which(Var_Cre_bs1>pctn_99)]<-NA
      }else{
        Var_Cre_bs<-Var_Cre_bs1
      }
      
      
      knots_Equi<-dlnm::equalknots(Var_Cre_bs,3)
      
      #Var_Sp<-splines::ns(Var_Cre_bs,df=df_spline)
      Var_Sp<-splines::ns(Var_Cre_bs,knots =knots_Equi)
      
      
      colnames(Var_Sp)<-paste0(Var_ns_pref,1:df_spline)
      
      assign(Spline_obj_Name,
             Var_Sp)
      
      
    }
    
    pp<-1
    
    knot_save<-function(pp){
      
      Spline.obj<-get(paste0('Var',pp,'_Spline'))
      kn_out<-list(Knots=attr(Spline.obj,"knots"),
           Boundary.knots=attr(Spline.obj,"Boundary.knots"))
      
      names(kn_out)<-paste0('Var',pp,c("_knots","_Boundary_knots"))
      kn_out
      
    }
  
    knots_Vars<-foreach(aa=1:length(alarm_vars),.combine =c)%do% knot_save(aa)
    
    spline_vars<-paste0('Var',1:length(alarm_vars),'_Spline')
    
    #Select_Lag_Comb<-paste(Selected_lags$var,collapse ="+")
    Select_Lag_Comb<-paste(Selected_lag_Vars,collapse ="+")
    
    
    
    Select_Lag_Comb_ns<-paste(spline_vars,collapse ="+")
    
    
    selected_Model_form <- as.formula(paste("Cases ~ 1 +f(ID_spat,model='iid',replicate=ID_year)+f(week,model='rw1'
                                        ,cyclic=T,scale.model =T)+",Select_Lag_Comb,collapse =""))
    
    selected_Model_form_ns <- as.formula(paste("Cases ~ 1 +f(ID_spat,model='iid',replicate=ID_year)+f(week,model='rw1'
                                           ,cyclic=T,scale.model =T)+",Select_Lag_Comb_ns,collapse =""))
    
    
    
    model_final_Lin<-Sel_Vars(selected_Model_form,"nbinomial",Dat_mod_Selected,T)
    model_final_ns<-Sel_Vars(selected_Model_form_ns,"nbinomial",Dat_mod_Selected,T)
    
    
    summary(model_final_Lin)
    
    summary(model_final_ns)
    
    
    sort(c(DIC_lin=model_final_Lin$dic$dic,
           DIC_ns=model_final_ns$dic$dic))
    
    sort(c(WAIC_lin=model_final_Lin$waic$waic,
           WAIC_ns=model_final_ns$waic$waic))
    
    
    ## perform the weekly CV /test Yearly
    
    
    Max_yrm<-range(data_augmented$year)[2]
    
    ## week=1, month=4,quarter=16,month,year=52
    
    #52/13
    
    
    
    #Intervals<-c(1,4,13,52)
    Intervals<-c(4,13)
    
    get_CV_work<-function(WeekIntreval){
      
      week_Intervals<-WeekIntreval
      beg_week<-seq(1,52,week_Intervals)
      end_week<-seq(week_Intervals,52,week_Intervals)
      length_Beg<-length(beg_week)
      length_End<-length(end_week)
      
      if(length_End<length_Beg){
        end_week1<-c(end_week,52)
      }else{
        end_week1<-end_week
        end_week1[length_Beg]<-52
      }
      
      int_padded<-str_pad(week_Intervals,width=2,side='left',pad=0)
      
      data.frame(year=Max_yrm,
                 week_Interval=week_Intervals,
                 beg_week=beg_week,
                 end_week=end_week1) |> 
        dplyr::mutate(beg_pad=str_pad(beg_week,width=2,side='left',pad=0),
                      end_pad=str_pad(end_week,width=2,side='left',pad=0),
                      Name_out=paste0('Weekly_Cross_validations_',
                                      year,'_Interval(',int_padded,')_',
                                      beg_pad,'_',end_pad))
      
    }
    
    
    #work_CV<<-get_CV_work(4)
    
    # work_CV_ls<-vector(length(Intervals),mode='list')
    # 
    # for(tt in 1:length(Intervals)){
    #   work_CV_ls[[tt]]<-get_CV_work(Intervals[tt])
    # }
    
    #cat("contents CV work::\n")
    
    #print(work_CV)
    
    #work_CV<-do.call(rbind,work_CV_ls)
    
    
    #work_CV<-expand.grid(Week=1:52,year=Max_yrm)
    
    # test_CV<<-list(Dat_mod_Selected=Dat_mod_Selected,
    #                work_CV=work_CV,
    #                selected_Model_form_ns=selected_Model_form_ns,
    #                Sel_Vars=Sel_Vars)
    
    #unlink(list.files(cv_path,full.names =T))
    
    #work_CV<-foreach(aa=Intervals,.combine =rbind)%do% get_CV_work(aa)
    work_CV<-get_CV_work(4)
    
    
    
    Run_CV<-CV_run
    
    if(Run_CV){
      
      time_CV<-system.time({
        #cc<-1

        Header_progress<-paste0("Cross validations district:",District_Now,' ',one_of_dist_str)
        p_progress <- Progress$new(min=0,max=nrow(work_CV))
        p_progress$set(message =Header_progress ,value=0)
        cat("",sep='\n')
        cat("Running Cross validations ...",sep='\n')
        
        for (cc in 1:nrow(work_CV)){
         
          
          #cat(paste("CV::",cc),sep='\n')
          cat(paste0(cc," "),sep=',')
          
          week_Sub<-work_CV$beg_week[cc]:work_CV$end_week[cc]
          
          CV_data<-Dat_mod_Selected |> 
            dplyr::mutate(Cases=case_when((year==work_CV$year[cc] & week %in% week_Sub)~NA,
                                          TRUE~Cases
            ))
          #cat(paste0('cv_data exists::',exists("CV_data")),sep='\n')
          
          
          
          #cv_idx<-with(CV_data,which(year==work_CV$year[cc] & week%in% week_Sub))
          
          cv_idx<-which(CV_data$year==work_CV$year[cc] & CV_data$week%in% week_Sub)
          
          
          
          model_CV<-Sel_Vars(selected_Model_form_ns,"nbinomial",CV_data,T)
          
          #cat(paste0('model_CV exists::',exists("model_CV")),sep='\n')
          
          #summary(model_CV)
          
          Nsamples<-1000
          
          xx <- inla.posterior.sample(Nsamples,model_CV,num.threads ="1:1")
          #xx <- inla.posterior.sample(Nsamples,model_CV,seed =2314557716)
          #cat(paste0('cv_idx exists::',exists("cv_idx")),sep='\n')
          
          
          
          #xx.s <- inla.posterior.sample.eval(function(...) c(theta[1], Predictor[cv_idx]), xx)
          #xx.s <- inla.posterior.sample.eval(function(...) c(theta[1],theta[2], Predictor[idx.pred]), xx)
          # list_sample<-list(xx=xx,
          #                   model_CV=model_CV,
          #                   Nsamples=Nsamples,
          #                   cv_idx=cv_idx,
          #                   CV_data=CV_data)
          # path_test<-"/media/sewe/Sewe_Nvme_Data/Projects/Ewars_Plus_2023_v2/test_Objs"
          # fname_test<-file.path(path_test,"inla_sample_test.rds")
          # 
          # saveRDS(list_sample,fname_test,compress=T)
          
          xx.size<-inla.posterior.sample.eval(function(...) c(theta[1]), xx)
          xx.s<-inla.posterior.sample.eval(function(...) c(Predictor), xx)[cv_idx,]
          gc()
          
          #xx.s<-xx.Pred[cv_idx,]
          
          mpred<-length(cv_idx)
          y.pred <- matrix(NA, mpred, Nsamples)
          
          #s.idx<-1
          
          for(s.idx in 1:Nsamples) {
            xx.sample <- xx.s[, s.idx]
            xx.size.sample<-xx.size[s.idx]
            #cat(xx.sample,sep='\n')
            y.pred[, s.idx] <- rnbinom(mpred, mu = exp(xx.sample), size = xx.size.sample)
            #y.pred[, s.idx] <- rnbinom(mpred, mu = (xx.sample[-1]), size = xx.sample[1])
            #y.pred[, s.idx] <- rpois(mpred, lambda = exp(xx.sample[-1]))
            #y.pred[, s.idx] <-  VGAM::rzipois(mpred, lambda = exp(xx.sample[-1]),pstr0 =xx.sample[1])
            
            
          }
          
          
          #names(Dat_mod_Selected)
          preds<-Dat_mod_Selected[cv_idx,] |> 
            dplyr::select(all_of(Vars_Final)) |> 
            dplyr::mutate(idx.pred = cv_idx, 
                          mean = apply(y.pred, 1, function(x) mean(x,na.rm=T)), 
                          median = apply(y.pred, 1, function(x) median(x,na.rm=T)),
                          lci = apply(y.pred, 1, function(x) quantile(x,c(0.025),na.rm=T)),
                          uci = apply(y.pred, 1, function(x) quantile(x,c(0.975),na.rm=T)))
          
          
          rownames(preds)<-NULL
          
          
          
          out_ls<-list(preds=preds,
                       y.pred=y.pred,
                       xx.s=xx.s)
          
          
          save_name<-file.path(cv_path,paste0(work_CV$Name_out[cc],'.rds'))
          
          unlink(save_name)
          saveRDS(out_ls,save_name,compress = T)
          
          pctn_done<-paste0(round((cc/nrow(work_CV))*100,1),' %')
          
          one_of_str<-paste0(cc,' of ',nrow(work_CV))
          
          mess_cv<-paste0(one_of_str,' (',pctn_done,")")
          p_progress$set(value = cc, detail = mess_cv)
          
          
        }
        
        p_progress$close()
        
        #Cross_Validation(1)
        
        #foreach(aa=1:nrow(work_CV))%do% Cross_Validation(aa)
      })
      
      time_CV[3]/60
    }
    
    
    all_files_Cv<-data.frame(fname=list.files(cv_path),
                             fullpath=list.files(cv_path,full.names =T)) |> 
      dplyr::filter(str_detect(fname,"Interval"))
    #aa<-1
    
    get_preds<-function(aa){
      
      Interval_wk<-str_remove(str_extract(all_files_Cv$fname[aa],"Interval[(][:number:]+"),"Interval[(]")
      
      preds<-readRDS(all_files_Cv$fullpath[aa])$preds |> 
        dplyr::mutate(week_Interval=Interval_wk)
      preds
    }
    
    get_Ypreds<-function(aa){
      
      Interval_wk<-str_remove(str_extract(all_files_Cv$fname[aa],"Interval[(][:number:]+"),"Interval[(]")
      Year_wk<-as.numeric(str_remove(str_extract(all_files_Cv$fname[aa],"validations_[:number:]+"),"validations_"))
      Weeks.Int<-as.numeric(str_split(str_extract(all_files_Cv$fname[aa],"[:number:]+_[:number:]+"),'_',simplify =T))
      Weeks_dat<-Weeks.Int[1]:Weeks.Int[2]
      
      dat_Meta<-data.frame(year=Year_wk,week=Weeks_dat) |> 
        dplyr::mutate(week_Interval=Interval_wk)
      
      y.pred0<-data.frame(dat_Meta,readRDS(all_files_Cv$fullpath[aa])$y.pred)
      y.pred_Long<-reshape2::melt(y.pred0,c("year","week","week_Interval"))
      y.pred_Long
    }
    
    all_cv<-foreach(aa=1:nrow(all_files_Cv),.combine =rbind)%dopar%get_preds(aa)
    
    y.PREDS<-foreach(aa=1:nrow(all_files_Cv),.combine =rbind)%dopar%get_Ypreds(aa)
    
    ## save weights here
    
    source("Save_Prediction_Weights.R",local=T)
    
    ## Convert pred to long datasets
    
    #names(all_cv)
    
    z_value<-1.2
    
    dat_Plot<-all_cv |> 
      dplyr::select(district,date,Cases,Pop,mean,lci,uci,mean_rate,sd_rate,week_Interval) |> 
      dplyr::mutate(obs_rate=(Cases/Pop)*1e5,
                    Threshold=mean_rate+(sd_rate*z_value),
                    pred_rate=(mean/Pop)*1e5,
                    pred_rate_lower=(lci/Pop)*1e5,
                    pred_rate_Upper=(uci/Pop)*1e5) |> 
      dplyr::select(-mean,-lci,-uci,-mean_rate,-sd_rate)
    
    
    ggplot(aes(x=date,y=obs_rate),data=dat_Plot)+
      facet_wrap(~week_Interval,ncol=2,scales ="free_y")+
      geom_ribbon(aes(ymin=pred_rate_lower,
                      ymax=pred_rate_Upper,
                      fill="CI"),
                  alpha=0.8)+
      geom_line(aes(col="Observed"),lwd=1.2)+
      geom_line(aes(y=pred_rate,col="Predicted"),lwd=1.2)+
      
      #geom_line(aes(x=date,y=pred_rate_lower),col='red')+
      #geom_line(aes(x=date,y=pred_rate_Upper),col='red')+
      
      #scale_x_continuous(breaks=1:12)+
      scale_x_date(date_breaks = "1 months",
                   labels=function(x) format.Date(x,"%b %Y"))+
      
      scale_colour_manual(values=c('Observed'='#7b3241','Predicted'='#32327b'))+
      scale_fill_manual(values=c('CI'='grey90'))+
      
      theme_bw()+
      ylab("Dengue \nincidence rate \n per 100000")+
      guides(col=guide_legend(title=NULL),
             fill=guide_legend(title=NULL))+
      theme(
        legend.box ="horizantol")+
      theme(axis.text.x =element_text(size=8,angle=45))
    
    unique(dat_Plot$week_Interval)
    
    dat_Plot_Subset<-dat_Plot |> 
      dplyr::filter(!week_Interval=="52")
    
    names(dat_Plot_Subset)
    
    Cases_pl<-dat_Plot_Subset |> 
      dplyr::select(district,date,obs_rate) |> 
      unique() |> 
      dplyr::rename(Rate=obs_rate) |> 
      dplyr::mutate(Cat="Observed")
    
    Cases_p2<-dat_Plot_Subset |> 
      dplyr::select(district,date,pred_rate,week_Interval) |> 
      unique() |> 
      dplyr::rename(Rate=pred_rate) |> 
      dplyr::mutate(Cat=paste0("Interval",week_Interval)) |> 
      dplyr::select(-week_Interval)
    
    Intervals_Comp<-rbind(Cases_pl,Cases_p2)
    
    Intervals_Comp_Wide<-Intervals_Comp |> 
      dplyr::group_by(date) |> 
      tidyr::spread(Cat,Rate)
    
    names(Intervals_Comp_Wide)
    
    #summary(lm(Interval01~Interval13,data=Intervals_Comp_Wide))
    #summary(lm(Observed~Interval01,data=Intervals_Comp_Wide))
    summary(lm(Observed~Interval04,data=Intervals_Comp_Wide))
    
    # cbind(
    #   #hydroGOF::gof(Intervals_Comp_Wide$Observed,Intervals_Comp_Wide$Interval01),
    #   hydroGOF::gof(Intervals_Comp_Wide$Observed,Intervals_Comp_Wide$Interval04),
    #   #hydroGOF::gof(Intervals_Comp_Wide$Observed,Intervals_Comp_Wide$Interval13)
    # )
    hydroGOF::gof(Intervals_Comp_Wide$Observed,Intervals_Comp_Wide$Interval04)
    
    ggplot(aes(x=date,y=Rate),data=Intervals_Comp)+
      
      geom_line(aes(col=Cat),lwd=1.2)+
      
      scale_x_date(date_breaks = "1 months",
                   labels=function(x) format.Date(x,"%b %Y"))+
      
      
      theme_bw()+
      ylab("Dengue \nincidence rate \n per 100000")+
      guides(col=guide_legend(title=NULL),
             fill=guide_legend(title=NULL))+
      theme(
        legend.box ="horizantol")+
      theme(axis.text.x =element_text(size=8,angle=90))
    
    
    # Compute  for Probabilities
    
    names(all_cv)
    
    dat_Sensitivity<-all_cv |> 
      dplyr::select(district,year,week,Cases,mean_rate,sd_rate,week_Interval,Pop) |> 
      dplyr::mutate(obs_rate=(Cases/Pop)*1e5,
                    Threshold=mean_rate+(sd_rate*z_value)
      ) |> 
      dplyr::filter(!week_Interval=="52")
    
    Combined_sensitivy<-dat_Sensitivity |> 
      dplyr::left_join(y.PREDS,by=c("year","week","week_Interval")) |> 
      dplyr::rename(pred_Cases=value) |> 
      dplyr::mutate(pred_rate=(pred_Cases/Pop)*1e5,
                    Outbreak=as.numeric(obs_rate>Threshold),
                    exceed=as.numeric(pred_rate>Threshold)) 
    
    ## compute probabilities
    
    probs_Exceed<-Combined_sensitivy |> 
      dplyr::group_by(year,week,week_Interval) |> 
      dplyr::summarise(.groups ="drop",
                       Outbreak=mean(Outbreak),
                       total=n(),
                       total_Exceed=sum(exceed),
                       exceed_prob=mean(exceed))
    
    # probs_Exceed_01<-probs_Exceed |> 
    #   dplyr::filter(week_Interval=="01")

    probs_Exceed_04<-probs_Exceed |>
      dplyr::filter(week_Interval=="04")
    
    # probs_Exceed_13<-probs_Exceed |> 
    #   dplyr::filter(week_Interval=="13")
    # 
    
    # reportROC(gold=probs_Exceed_01$Outbreak,
    #           predictor=probs_Exceed_01$exceed_prob,
    #           important="se")
    # 
    suppressMessages(suppressWarnings(reportROC(gold=probs_Exceed_04$Outbreak,
              predictor=probs_Exceed_04$exceed_prob,
              important="se")))

    # reportROC(gold=probs_Exceed_13$Outbreak,
    #           predictor=probs_Exceed_13$exceed_prob,
    #           important="se")
    
    
    ## automatically select best through data driven process
    
    z_test<-seq(1,2.1,0.02)
    length(z_test)
    
    #ZValue<-0.8
    
    select_Z_values<-function(ZValue){
      
      gc()
      
      dat_Sensitivity<-all_cv |> 
        dplyr::select(district,year,week,Cases,mean_rate,sd_rate,week_Interval,Pop) |> 
        dplyr::mutate(obs_rate=(Cases/Pop)*1e5,
                      Threshold=mean_rate+(sd_rate*ZValue)
        ) |> 
        dplyr::filter(!week_Interval=="52")
      
      Combined_sensitivy<-dat_Sensitivity |> 
        dplyr::left_join(y.PREDS,by=c("year","week","week_Interval")) |> 
        dplyr::rename(pred_Cases=value) |> 
        dplyr::mutate(pred_rate=(pred_Cases/Pop)*1e5,
                      Outbreak=as.numeric(obs_rate>Threshold),
                      exceed=as.numeric(pred_rate>Threshold)) 
      
      ## compute probabilities
      
      probs_Exceed<-Combined_sensitivy |> 
        dplyr::group_by(year,week,week_Interval) |> 
        dplyr::summarise(.groups ="drop",
                         Outbreak=mean(Outbreak),
                         total=n(),
                         total_Exceed=sum(exceed),
                         exceed_prob=mean(exceed))
      
      probs_Exceed_Sub<-probs_Exceed |> 
        dplyr::filter(week_Interval=="04")
      
    
      #probs_Exceed_13$exceed_prob
      

      roc_try<-suppressMessages(suppressWarnings(try(reportROC(gold=probs_Exceed_Sub$Outbreak,
                             predictor=probs_Exceed_Sub$exceed_prob,
                             important="se"),
                   outFile =warning("ROC_error.txt"))))
      
      if(class(roc_try) %in% c("NULL","try-error")){
        data_ztest<-data.frame(zvalue=ZValue,
                               Cutoff=NA,
                               AUC=NA,
                               sens=NA,
                               spec=NA,
                               ppv=NA,
                               npv=NA,
                               Accuracy=NA)
      }else{
        sen_score<-suppressMessages(suppressWarnings(reportROC(gold=probs_Exceed_Sub$Outbreak,
                             predictor=probs_Exceed_Sub$exceed_prob,
                             important="se")))
        data_ztest<-data.frame(zvalue=ZValue,
                               Cutoff=sen_score$Cutoff,
                               AUC=sen_score$AUC,
                               sens=sen_score$SEN,
                               spec=sen_score$SPE,
                               ppv=sen_score$PPV,
                               npv=sen_score$NPV,
                               Accuracy=sen_score$ACC)
      }
      rm(Combined_sensitivy)
      gc()
      data_ztest
      
    }
    
    ssample_z_Select<-20
    z_test_Sample<-sample(z_test,size=ssample_z_Select)
    
    #zvalue_sel<-foreach(aa=z_test_Sample,.combine =rbind)%do%select_Z_values(aa)
    
    zvalue_sel_ls<-vector(ssample_z_Select,mode='list')
    
    Header_progress<-paste0("Z_value selection:",District_Now,' ',one_of_dist_str)
    p_progress <- Progress$new(min=0,max=ssample_z_Select)
    p_progress$set(message =Header_progress ,value=0)
    
    cat("",sep='\n')
    cat("selecting Z value ...",sep='\n')
    
    for(zz in 1:ssample_z_Select){
      
      cat(paste0(zz," "),sep=',')
      
      zvalue_sel_ls[[zz]]<-select_Z_values(z_test_Sample[zz])
      
      pctn_done<-paste0(round((zz/ssample_z_Select)*100,1),' %')
      
      one_of_str<-paste0(zz,' of ',ssample_z_Select)
      
      #mess_z<-paste0(one_of_str,' (',pctn_done,")")
      mess_z<-paste0('Done.. (',pctn_done,")")
      
      p_progress$set(value = zz, detail = mess_z)
      
    }
    
    p_progress$close()
    
    zvalue_sel<-do.call(rbind,zvalue_sel_ls)
    
    zvalue_sel_Ordered1<-zvalue_sel |> 
      dplyr::arrange(desc(AUC))
    
    zvalue_sel_Ordered<-zvalue_sel |> 
      dplyr::mutate(AUC=as.numeric(AUC),
                    ppv=as.numeric(AUC),
                    spec=as.numeric(AUC),
                    sens=as.numeric(sens),
                    npv=as.numeric(npv),
                    Accuracy=as.numeric(Accuracy),
                    score=AUC+ppv+spec+sens+npv+Accuracy) |> 
      dplyr::filter(!(sens==1|spec==1)) |> 
      dplyr::arrange(-AUC) 
    
    
    selected_zvalue<-zvalue_sel_Ordered$zvalue[1]
    
    # do the Plots 
    
    #district_new<-15
    
    data_one<-data_augmented %>% 
      dplyr::filter(district==District_Now) %>% 
      #dplyr::mutate(DIR=.data[[number_of_cases]]/get(pop.var.dat))*1e5)
      dplyr::mutate(DIR=(.data[[number_of_cases]]/.data[[pop.var.dat]])*1e5)
    
    
    
    vars_get_summary<-c(number_of_cases,pop.var.dat,input$alarm_indicators_New_model)
    var.sum<-c("district","year","week",vars_get_summary)
    
    
    dat_sum<-data_one[,var.sum]
    
    dat_sum_long<-reshape2::melt(dat_sum,c("district","year","week"))
    
    dat_kl<-dat_sum_long %>% 
      dplyr::group_by(variable,year) %>% 
      dplyr::summarise(.groups="drop",min=min(value,na.rm =T),
                       max=max(value,na.rm =T),
                       mean=mean(value,na.rm =T),
                       median=quantile(value,0.5,na.rm =T),
                       p25=quantile(value,0.25,na.rm =T),
                       p75=quantile(value,0.75,na.rm =T),
                       pctn_missing=paste0(round((sum(is.na(value))/n())*100,1),"%"))%>% 
      
      dplyr::mutate_at(.vars=c("min","max","mean","p25","median","p75"),
                       .funs = function(x) ifelse(x %in% c(Inf,-Inf),NA,x)) %>% 
      dplyr::mutate_at(.vars=c("min","max","mean","p25","median","p75"),
                       .funs = function(x) round(x,1))
    
    names(dat_kl)<-c("Variable","Year","Min","Max","Mean","Median","25th Percentile",
                     "75th Percentile","% Missing")
    
    
    get_packed_st<-function(x){
      paste0("pack_rows('",x,"'",',',min(which(dat_kl$Variable==x)),',',max(which(dat_kl$Variable==x)),')')
    }

    all_kl<-foreach(a=as.character(unique(dat_kl$Variable)),.combine =c)%do% get_packed_st(a)

    all_kl_cmd<-paste(c("function() { dat_kl[,-1]","kbl(format='html',caption = paste('District ',unique(dat_sum_long$district)))","kable_styling('striped', full_width = F)",
                        "column_spec(8,background='#94a323')",all_kl),collapse ='%>%\n')


    all_kl_cmd<-paste(all_kl_cmd,'}\n',collapse ='')
    # 
    # eval(parse(text=all_kl_cmd))
    # 
    # dat_kl[,-1]%>%
    #   kbl(format='html',caption = paste('District ',unique(dat_sum_long$district)))%>%
    #   kable_styling('striped', full_width = F)%>%
    #   column_spec(8,background='#94a323')%>%
    #   pack_rows('weekly_hospitalised_cases',1,6)%>%
    #   pack_rows('population',7,12)%>%
    #   pack_rows('rainsum',13,18)%>%
    #   pack_rows('meantemperature',19,24)
    
    ## use flextable
    
    
    #use tabulator and flextable
    names(dat_kl)
    
    dat_kl1<-dat_kl |> 
      dplyr::mutate(Year=as.character(Year))
    
    dat_kl_Long<-reshape2::melt(dat_kl1,c("Variable","Year"))
    
    names(dat_kl_Long)
    
    tab_Dat<-tabulator(x=dat_kl_Long,
                       rows=c("Variable","Year"),
                       columns=c("variable"),
                       ystats=as_paragraph(value)
    )
    
    
    #bright <- khroma::color("bright")
    #bright(7)
    #scales::show_col(bright(7))
    
    #?font
    
    border_prop<-officer::fp_border(width=0.5)
    
    tab_Dat |> 
      as_flextable() |> 
      #flextable::autofit(add_w=0,add_h=10,unit='mm',part='body') |> 
      flextable::set_caption(as_paragraph(paste('District ',unique(dat_sum_long$district))),
                             fp_p=officer::fp_par(text.align = "left")) |> 
      flextable::fit_to_width(max_width =12,unit='in') |> 
      flextable::fontsize(part='header',size=14) |> 
      flextable::fontsize(part='body',size=10) |> 
      flextable::font(part='all',fontname ="Courier") |> 
      flextable::padding(part="body",padding = 4) |> 
      flextable::bold(i=1,part ="header") |> 
      flextable::border_inner_h(border =border_prop) |> 
      flextable::color(i=1,color="#CCBB44",part="header") |> 
      flextable::bg(j=16,bg="#94a323") |> 
      flextable::align(part='all',align ="left")
    
    ## Lag Selection
    
    #Lag_dic_comp1
    
    #names(Lag_dic_comp1)
    levels_Rank<-sort(str_pad(unique(Lag_dic_comp1$Rank),width=2,pad=0,side="left"))
    levels_Var<-Lag_dic_comp1$var
    
    
    Lag_dic_comp2<-Lag_dic_comp1 |> 
      dplyr::mutate(DIC=round(DIC,3),
                    coeff=round(coeff,3),
                    low=round(low,3),
                    high=round(high,3),
                    logcpo=round(logcpo,3),
                    Rank=str_pad(Rank,width=2,pad=0,side="left"),
                    var=factor(var,levels=levels_Var))
    
    Lag_dic_comp1_Long<-reshape2::melt(Lag_dic_comp2,c("var","Variable")) 
    #names(Lag_dic_comp1_Long)
    #?tabulator
    
    tab_Dat_lag<-tabulator(x=Lag_dic_comp1_Long,
                           rows=c("Variable","var"),
                           columns=c("variable"),
                           ystats=as_paragraph(value)
    )
    
    rows_01<-which(Lag_dic_comp2$Rank=="01")
    
    tab_Dat_lag |> 
      as_flextable() |> 
      #flextable::autofit(add_w=0,add_h=10,unit='mm',part='body') |> 
      flextable::set_caption(as_paragraph(paste('District ',unique(dat_sum_long$district))),
                             fp_p=officer::fp_par(text.align = "left")) |> 
      flextable::fit_to_width(max_width =12,unit='in') |> 
      flextable::fontsize(part='header',size=12) |> 
      flextable::fontsize(part='body',size=8) |> 
      flextable::font(part='all',fontname ="Courier") |> 
      flextable::padding(part="body",padding = 4) |> 
      flextable::bold(i=1,part ="header") |> 
      flextable::border_inner_h(border =border_prop) |> 
      flextable::color(i=1,color="#CCBB44",part="header") |> 
      flextable::bg(i=rows_01,j=2:16,bg="#4477AA",part="body") |> 
      flextable::align(part='all',align ="left")
    
    
    #p<-1
    plot_desc<-function(p){
      
      
      data_n<-data_one[,c("year","week",vars_get_summary[p])] 
      names(data_n)[3]<-'var'
      
      beg.year<-min(data_n$year)
      end.year<-max(data_n$year)
      
      plo1<-ggplot(data=data_n)+
        #geom_raster(aes(x=week,y=year,fill=var))+
        geom_tile(aes(x=week,y=year,fill=var))+
        #coord_fixed()+
        scale_fill_gradientn(name =vars_get_summary[p], colours = rev(brewer.pal(11, "RdBu"))) + 
        scale_y_continuous(breaks =beg.year:end.year,expand =c(0,0))+
        scale_x_continuous(breaks=seq(0,52,4),expand =c(0,0))+
        #coord_fixed()+
        ylab("Year")+
        xlab("Week")+
        theme_bw()+
        ggtitle(paste('District:',unique(data_one$district)))+
        theme(legend.position ="bottom",
              legend.title =element_text(face="italic"))+
        guides(fill=guide_colorbar(title =vars_get_summary[p],
                                   title.hjust =0.5,
                                   barwidth=grid::unit(6,'cm'),
                                   barheight=grid::unit(0.3,'cm'),
                                   title.position ="top"))
      list(plo1)
      
    }
    #?guide_colorbar 
    plot_List0<-foreach(a=1:length(vars_get_summary),.combine =c)%do% plot_desc(a)
    cat(paste("Summary variables ::\n"),paste(vars_get_summary,collapse =','),'\n\n')
    #prrrrr<<-plot_List
    ## render Plots in a loop
    
    all_vars<-c(base_vars,number_of_cases,pop.var.dat,alarm_indicators)
    
    
    dat_Sel<-data_augmented[,all_vars]
    
    
    melted_dat<-reshape2::melt(dat_Sel,base_vars) |> 
      mutate(year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2))) |> 
      dplyr::select(district,year_week,variable,value)
    
    wide_for_dygraph<-melted_dat |> 
      dplyr::group_by(variable,year_week) |> 
      tidyr::spread(district,value)
    
    dates_s<-seq.Date(as.Date(paste0(beg.year,'-01-01')),
                      as.Date(paste0(end.year,'-12-31')),
                      by='day')
    
    
    
    data_Weeks<-data.frame(date=dates_s,
                           year_week=format.Date(dates_s,"%Y_%W"),
                           year=year(dates_s),
                           stringsAsFactors =F,
                           week=week(dates_s)) %>% 
      mutate(Week=str_split_fixed(year_week,pattern ='_',n=2)[,2]) %>% 
      dplyr::filter(as.numeric(Week)%in% 1:52)
    
    weeks.in.data<-data_augmented0 %>% 
      dplyr::mutate(year_week=paste0(year,'-',str_pad(week,side ="left",pad =0,width =2))) 
    
    year_week_S<-data_Weeks %>% dplyr::group_by(year,Week) %>% 
      dplyr::summarise(.groups="drop",date_Beg=min(date)) %>% 
      dplyr::mutate(year_week=format.Date(date_Beg,"%Y-%W"))%>% 
      dplyr::filter(year_week %in% weeks.in.data$year_week)
    
    get_xts_dat<-function(p){
      dat_n<-wide_for_dygraph %>% dplyr::filter(variable==covar_to_Plot[p])
      dat_n<-dat_n[,-2]
      dat_n1<-dat_n[,-1]
      dat_n2<-xts(dat_n1,order.by =as.Date(as.character(year_week_S$date_Beg)),
                  frequency=52)
      plo<-dygraph(dat_n2,xlab ="Year week",ylab=covar_to_Plot[p]) %>%
        #dyMultiColumn()
        dySeries() %>% 
        dyRangeSelector() %>% 
        dyLegend(show = "follow") %>% 
        dyHighlight(highlightCircleSize =2, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = T)
      aa<-list(plo)
      names(aa)<-names_cov_Plot[p]
      aa
    }
    
    all_xts_Plots<-foreach(a=1:length(covar_to_Plot),.combine =c)%do% get_xts_dat(a)
    
    
    ## get spatial_Plots
    melted_dat_wide<-melted_dat %>% 
      dplyr::group_by(district,variable) %>% 
      tidyr::spread(year_week,value)
    
    get_Spatial_poly_dat<-function(p){
      dat_n<-melted_dat_wide %>% dplyr::filter(variable==covar_to_Plot[p])
      merge_Poly<-merge(boundary_file,dat_n,by="district",sort=F,all.x=T)
      aa<-list(merge_Poly)
      names(aa)<-names_cov_Plot[p]
      aa
    }
    
    all_Plot_Poly<-foreach(a=1:length(covar_to_Plot),.combine =c)%do% get_Spatial_poly_dat(a)
    
    
    var_p<-names_cov_Plot
    cat(paste0('\nvar_p:\n'),paste(var_p,sep=' '),'\n\n')
    
    new_model_Year_plot<-input$new_model_Year_plot
    
    yr_week<-paste0(new_model_Year_plot,'_',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    yr_week1<-paste0(new_model_Year_plot,':',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    
    yr_week_input<-paste0(new_model_Year_plot,":",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    yr_week_input1<-paste0(new_model_Year_plot,"_",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    
    
    cat(paste("\nfrom input ::",yr_week_input,'\n\n'))
    
    p<-1
    
    plot_Func<-function(p){
      #browser()
      plot_Now<-all_Plot_Poly[[var_p[p]]]
      week.idx<-which(names(plot_Now)==yr_week)
      week_slice<-plot_Now[,c("district",yr_week)]
      
      lng1<-as.numeric(week_slice@bbox[,1][1])
      lat1<-as.numeric(week_slice@bbox[,1][2])
      lng2<-as.numeric(week_slice@bbox[,2][1])
      lat2<-as.numeric(week_slice@bbox[,2][2])
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g",
        week_slice$district, eval(parse(text=paste0("week_slice$`",yr_week,"`")))
      ) %>% lapply(htmltools::HTML)
      
      
      legend_title<-sprintf(
        "<strong>%s</strong><br/>%s",
        var_p[p],yr_week1 
      ) %>% lapply(htmltools::HTML)
      
      id.summ<-str_detect(names(week_slice),"[:number:]+_[:number:]+")
      
      dom_comp<-unique(as.numeric(unlist((week_slice[,id.summ]@data))))
      
      len.dom<-length(dom_comp)
      if(len.dom==1){
        if(is.na(dom_comp)){
          dom_range<-c(eval(parse(text=paste0("week_slice$`",yr_week,"`"))),1)
          
        }else{
          dom_range<-c(eval(parse(text=paste0("week_slice$`",yr_week,"`"))))
          
        }
      }else{
        dom_range<-eval(parse(text=paste0("week_slice$`",yr_week,"`")))
      }
      pal <- colorNumeric("YlOrRd", 
                          domain =dom_range,
                          reverse=F) 
      plo1<-leaflet(week_slice[,yr_week]) %>% 
        leaflet::addTiles() %>% 
        leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
        leaflet::fitBounds(lng1,lat1,lng2,lat2) %>% 
        #addPolylines() %>% 
        leaflet::addPolygons(fillColor = eval(parse(text=paste0("~pal(`",yr_week,"`)"))),
                             color = "black",weight =0.8,
                             dashArray = " ",
                             fillOpacity = 0.9,
                             highlight = highlightOptions(
                               weight = 5,
                               color = "green",
                               dashArray = "2",
                               fillOpacity = 0.7,
                               bringToFront = TRUE),
                             label = labels,
                             labelOptions = labelOptions(
                               style = list("font-weight" = "normal", padding = "3px 8px"),
                               textsize = "15px",
                               direction = "auto")) %>% 
        leaflet::addLegend(pal = pal, values = eval(parse(text=paste0("~`",yr_week,"`"))), 
                           opacity = 0.7, title = legend_title,
                           position = "bottomright") 
      list(plo1)
      
    }
    
    plot_List<-foreach(a=1:length(var_p),.combine =c)%do% plot_Func(a)
    
    
    #plot SIR
    
    #model_final_ns
    
    SIR_dat<-data_augmented[,base_vars] %>% 
      dplyr::filter(district==District_Now) |> 
      mutate(Fitted_cases=model_final_ns$summary.fitted.values$mean,
             year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2)))%>% 
      dplyr::select(district,year_week,Fitted_cases)
    
    SIR_wide<-SIR_dat %>% 
      dplyr::group_by(district) %>% 
      tidyr::spread(year_week,Fitted_cases)
    
    ##merge to polygons for plotting
    SIR_Poly<-merge(boundary_file,SIR_wide,by="district",sort=F,all.x=T)
    
    
    yr_week<-paste0(new_model_Year_plot,'_',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    yr_week1<-paste0(new_model_Year_plot,':',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    
    yr_week_input<-paste0(new_model_Year_plot,":",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    yr_week_input1<-paste0(new_model_Year_plot,"_",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
    
    
    #print(paste("from input ::",yr_week_input))
    names_Plots_s<-paste(names(SIR_Poly),collapse =" ")
    
    first_pos_SIR<-which(stringr::str_detect(names(SIR_Poly),'[:number:]+_[:number:]+'))[1]
    
    first_YR_week<-stringr::str_extract(names(SIR_Poly)[first_pos_SIR],'[:number:]+_[:number:]+')
    first_YR_week1<-str_replace(first_YR_week,'_',":")
    
    if(stringr::str_detect(names_Plots_s,yr_week_input1)==FALSE){
      yr_week1<-first_YR_week1
      yr_week<-first_YR_week
      
    }else{
      yr_week1<-yr_week_input
      yr_week<-yr_week_input1
    }
    
    
    
    ## Extract weekly effects
    #district_seas<-Shiny_Input$district_seas
    
    weekly_effects <- data.table(cbind(rep(District_Now, each = 52),
                                       model_final_ns$summary.random$week))
    names(weekly_effects)[1:2] <- c("district", "Week")
    
    weekly_effects_check<-weekly_effects
    weekly_effects_sub<-weekly_effects %>% 
      dplyr::filter(district ==District_Now)
    #dplyr::filter(district ==23)
    plot.seas<-weekly_effects_sub %>% 
      ggplot() + 
      geom_ribbon(aes(x = Week, ymin = `0.025quant`, ymax = `0.975quant`), 
                  fill = "cadetblue4", alpha = 0.5) + 
      geom_line(aes(x = Week, y = `mean`), col = "cadetblue4") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      #facet_wrap(~district,ncol =4)+
      xlab("Week") +
      ggtitle(paste("District:",District_Now))+
      ylab("Contribution to log(DIR)") +
      scale_y_continuous() +
      scale_x_continuous(breaks = seq(0,52,5)) +
      theme_bw()+
      theme(axis.text =element_text(size=14),
            axis.title =element_text(size=16))
    
    
    ## do the lag plots
    nlag<-input$nlag
    
    Dat_mod_for_dlnn<-data_augmented |> 
      dplyr::left_join(Dist_IDS,by="district") |> 
      dplyr::left_join(Year_IDS,by="year") |> 
      dplyr::filter(district==District_Now) |> 
      dplyr::arrange(district,year,week)
    N_LAG<-input$nlags
    
    #all_basis_vars<-foreach(a=alarm_vars,.combine =c)%do% get_cross_basis(a,data_b=Dat_mod_for_dlnn,nlag=nlag)
    all_basis_vars<-foreach(a=alarm_vars,.combine =c)%do% get_cross_basis(a,data_b=Dat_mod_for_dlnn,nlag=N_LAG)
    
    n_district <- length(unique(Dat_mod_for_dlnn$district))
    
    ## create district index 
    
    Dat_mod_Selected1<-Dat_mod_Selected
    
    fixe_alarm_vars<-input$other_alarm_indicators_New_model
    
    add.var<-fixe_alarm_vars[which(!fixe_alarm_vars%in% alarm_vars)]
    
    vars_Base1
    cat(paste(paste0('"',vars_Base1,'"'),collapse =','))
    
    Var_for_Dlnm<-c("district","ID_spat",
                    "year","ID_year","week",
                    "date","Cases",
                    "Pop","log_Pop")
    
    names(data_augmented)
    
    if(length(add.var)>0){
      sel_mod.vars<-c(Var_for_Dlnm,alarm_vars,add.var)
    }else{
      sel_mod.vars<-c(Var_for_Dlnm,alarm_vars)
      
    }
    #cat(names(data_augmented),sep='..\n')
    #stop("xxx")
    
    
    Data_dlnm<-Dat_mod_for_dlnn[,sel_mod.vars] |> 
      dplyr::arrange(district,year,week)
    
    basis_var_n<-paste0('all_basis_vars$',names(all_basis_vars))
    
    baseformula<- "Cases ~ 1 +
      f(ID_spat,model='iid',replicate=ID_year)+
      f(week,model='rw1',cyclic=T,scale.model =T)"
    
    ## assign basis var to objects
    
    for (bb in 1:length(basis_var_n)){
      assign(paste0("basis_var",bb),all_basis_vars[[bb]])
    }
    
    
    
    #basis_var_Comb<-paste0(add.var,basis_var_n,collapse ="+")
    
    basis_var_Comb<-paste0("basis_var",1:length(basis_var_n),collapse ="+")
    
    form_base_part<-c("Cases ~ 1","f(ID_spat,model='iid',replicate=ID_year)","f(week,model='rw1',cyclic=T,scale.model =T)")
    

    #formula0.2<-paste0(c(form_base_part,basis_var_Comb),collapse ='+')

    #cat(paste0(baseformula,paste0(add.var,basis_var_n,collapse ="+")))
    
    #add.var<-"temp"
    #rm(add.var)
    #add.var<-NULL
    
    if(length(add.var)>0){
      formula0.2<-as.formula(paste0(c(form_base_part,basis_var_Comb,add.var),collapse ='+'))
    }else{
      formula0.2<-as.formula(paste0(c(form_base_part,basis_var_Comb),collapse ='+'))
      
    }
    
    dlnm_Inla<-function(form_mod,mod_Family,mod.Data,start_again) {
      
      mod<-inla(form_mod,
                offset =log_Pop,
                data=mod.Data,
                family =mod_Family,
                control.inla = list(strategy = 'simplified.laplace',
                                    parallel.linesearch=F,
                                    improved.simplified.laplace=F), 
                control.compute = list(dic = TRUE, 
                                       config = T, 
                                       cpo = TRUE, 
                                       waic=T,
                                       #po=T,
                                       return.marginals = F,
                                       smtp="taucs"
                ),
                control.fixed = list(correlation.matrix = TRUE),
                num.threads="2:1",
                control.mode = list(theta = theta_beg$mean, restart = start_again),
                control.predictor = list(link = 1, compute = TRUE), 
                verbose = F)
      mod
    }
    
    
    
    dlnm_Model<-dlnm_Inla(formula0.2,"nbinomial",Data_dlnm,T)
    
    #summary(dlnm_Model)
    
    if(DIC1_run){
      tim_dic1<-time_dic1
    }else{
      tim_dic1<-NA
    }
    
    if(DIC2_run){
      tim_dic2<-time_dic2
    }else{
      tim_dic2<-NA
    }
    
    if(CV_run){
      tim_CV<-time_CV
    }else{
      tim_CV<-NA
    }
    
    district_Objs_save<-list(data_augmented=data_augmented,
                             all_endemic=all_endemic,
                             Dist_IDS=Dist_IDS,
                             Year_IDS=Year_IDS,
                             rows_01=rows_01,
                             summary_combs=summary_combs,
                             vars_Base=vars_Base,
                             Model_data_lags=Model_data_lags,
                             Dat_mod=Dat_mod,
                             Dat_mod_sub=Dat_mod_sub,
                             Model_data_lags_sub=Model_data_lags_sub,
                             form_baseline=form_baseline,
                             baseline_model=baseline_model,
                             theta_beg=theta_beg,
                             id_lag=id_lag,
                             path_dic1=path_dic1,
                             path_dic2=path_dic2,
                             cv_path=cv_path,
                             all_cv=all_cv,
                             y.PREDS=y.PREDS,
                             shiny_obj_pth=shiny_obj_pth,
                             time_dic1=tim_dic1,
                             time_dic2=tim_dic2,
                             Lag_dic_comp=Lag_dic_comp,
                             Var_ext_lag0=Var_ext_lag0,
                             Var_ext_Lag=Var_ext_Lag,
                             Lag_dic_comp1=Lag_dic_comp1,
                             lag_Var_Dat=lag_Var_Dat,
                             Selected_lags=Selected_lags,
                             Lag_combinations_dic=Lag_combinations_dic,
                             Lag_combinations_dic1=Lag_combinations_dic1,
                             Selected_lag_Vars=Selected_lag_Vars,
                             knots_Vars=knots_Vars,
                             vars_Base1=vars_Base1,
                             Vars_Final=Vars_Final,
                             Dat_mod_Selected=Dat_mod_Selected,
                             Select_Lag_Comb=Select_Lag_Comb,
                             Select_Lag_Comb_ns=Select_Lag_Comb_ns,
                             selected_Model_form=selected_Model_form,
                             selected_Model_form_ns=selected_Model_form_ns,
                             model_final_Lin=model_final_Lin,
                             model_final_ns=model_final_ns,
                             work_CV=work_CV,
                             time_CV=tim_CV,
                             all_files_Cv=all_files_Cv,
                             zvalue_sel_Ordered=zvalue_sel_Ordered,
                             selected_zvalue=selected_zvalue,
                             data_one=data_one,
                             tab_Dat=tab_Dat,
                             tab_Dat_lag=tab_Dat_lag,
                             summary_plots=plot_List0,
                             #all_xts_Plots=all_xts_Plots,
                             all_Plot_Poly=all_Plot_Poly,
                             leaflet_plots=plot_List,
                             seasonal_plot=plot.seas,
                             nlag=nlag,
                             all_basis_vars=all_basis_vars,
                             n_district=n_district,
                             Var_for_Dlnm=Var_for_Dlnm,
                             Dat_mod_for_dlnn=Dat_mod_for_dlnn,
                             Data_dlnm=Data_dlnm,
                             basis_var_n=basis_var_n,
                             formula0.2=formula0.2,
                             dlnm_Model=dlnm_Model,
                             #Total_Run_time=Time_one_Dist,
                             dat_kl=dat_kl,
                             all_kl=all_kl,
                             all_kl_cmd=all_kl_cmd,
                             vars_get_summary=vars_get_summary,
                             covar_to_Plot=covar_to_Plot,
                             alarm_vars=alarm_vars,
                             names_cov_Plot=names_cov_Plot,
                             df_spline=df_spline,
                             population_var=population,
                             pop.var.dat=pop.var.dat,
                             other_alarm_indicators=other_alarm_indicators,
                             number_of_cases=number_of_cases,
                             new_model_Year_validation=new_model_Year_validation)
    
    file_name_save<-file.path(shiny_obj_pth,"Shiny_Objs.rds")
    saveRDS(district_Objs_save,file_name_save,compress =T)
    
    
    
  }
})

Time_one_Dist[3]/60



  
  #Save district sepecif objects
  
  

  All_Objs_save<-list(all_endemic=all_endemic,
                      data_augmented=data_augmented,
                           Dist_IDS=Dist_IDS,
                           Year_IDS=Year_IDS,
                           summary_combs=summary_combs,
                           vars_Base=vars_Base,
                           Model_data_lags=Model_data_lags,
                           Dat_mod=Dat_mod,
                           Dat_mod_sub=Dat_mod_sub,
                           Model_data_lags_sub=Model_data_lags_sub,
                           form_baseline=form_baseline,
                           baseline_model=baseline_model,
                           sel_var_endemic=sel_var_endemic,
                           #theta_beg=theta_beg,
                           id_lag=id_lag,
                           #path_dic1=path_dic1,
                           #path_dic2=path_dic2,
                           #cv_path=cv_path,
                           #shiny_obj_pth=shiny_obj_pth,
                           #time_dic1=time_dic1,
                           #time_dic2=time_dic2,
                           #time_dic1=3.9,
                           #Lag_dic_comp=Lag_dic_comp,
                           Var_ext_lag0=Var_ext_lag0,
                           Var_ext_Lag=Var_ext_Lag,
                           #Lag_dic_comp1=Lag_dic_comp1,
                           #lag_Var_Dat=lag_Var_Dat,
                           #Selected_lags=Selected_lags,
                           #Lag_combinations_dic=Lag_combinations_dic,
                           #Lag_combinations_dic1=Lag_combinations_dic1,
                           #Selected_lag_Vars=Selected_lag_Vars,
                           vars_Base1=vars_Base1,
                           Vars_Final=Vars_Final,
                           Dat_mod_Selected=Dat_mod_Selected,
                           #Select_Lag_Comb=Select_Lag_Comb,
                           #Select_Lag_Comb_ns=Select_Lag_Comb_ns,
                           selected_Model_form=selected_Model_form,
                           selected_Model_form_ns=selected_Model_form_ns,
                           #model_final_Lin=model_final_Lin,
                           #model_final_ns=model_final_ns,
                           #work_CV=work_CV,
                           #time_CV=time_CV,
                           all_files_Cv=all_files_Cv,
                           #zvalue_sel_Ordered=zvalue_sel_Ordered,
                           #selected_zvalue=selected_zvalue,
                           #data_one=data_one,
                           #tab_Dat=tab_Dat,
                           #tab_Dat_lag=tab_Dat_lag,
                           #summary_plots=plot_List0,
                           all_xts_Plots=all_xts_Plots,
                           all_Plot_Poly=all_Plot_Poly,
                           leaflet_plots=plot_List,
                           #seasonal_plot=plot.seas,
                           nlag=nlag,
                           all_basis_vars=all_basis_vars,
                           n_district=n_district,
                           Var_for_Dlnm=Var_for_Dlnm,
                           Dat_mod_for_dlnn=Dat_mod_for_dlnn,
                           Data_dlnm=Data_dlnm,
                           basis_var_n=basis_var_n,
                           formula0.2=formula0.2,
                           dlnm_Model=dlnm_Model,
                           #Total_Run_time=Time_one_Dist,
                           #dat_kl=dat_kl,
                           #all_kl=all_kl,
                           #all_kl_cmd=all_kl_cmd,
                           vars_get_summary=vars_get_summary,
                           covar_to_Plot=covar_to_Plot,
                           alarm_vars=alarm_vars,
                           names_cov_Plot=names_cov_Plot,
                           Total_Run_time=Time_one_Dist,
                           population_var=population,
                           pop.var.dat=pop.var.dat,
                           other_alarm_indicators=other_alarm_indicators,
                           number_of_cases=number_of_cases,
                           new_model_Year_validation=new_model_Year_validation
                      
                      
                      )
  
  
  
file_name_save_all<-file.path(all_files_Path,"Shiny_Objs_all.rds")
saveRDS(All_Objs_save,file_name_save_all,compress =T)


