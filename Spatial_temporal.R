
observeEvent(input$run_mod,
             
             {
               
               
               out_Path<-file.path(getwd(),'Outputs')
               
               #folders_Create<-c("Lag_selection","Lag_Comb_selection","Weekly_crossValidations","For_Shiny")
               #folder_Vars<-c("path_dic1","path_dic2","cv_path","shiny_obj_pth")
               shiny_obj_Main_pth<-file.path(out_Path,"For_Shiny")
               all_files_Path<-file.path(out_Path,"For_Shiny","All_district")
               
               
               dat_fl<-var_names_New_model()$dat
               dat_slider<-dat_fl
               boundary_file<-var_names_New_model()$SHP
               shp_data<-boundary_file@data
               cat(paste('Boundary file variables:\n'),paste(names(shp_data),sep=','),'\n\n')
               
               boundary_file$district<-as.numeric(boundary_file$district)
               
               boundary_file_mod<-boundary_file[which(boundary_file$district %in% dat_fl$district),]
               
               
               population<-input$population_New_model
               pop.var.dat<-input$population_New_model
               alarm_indicators<-input$alarm_indicators_New_model
               other_alarm_indicators<-input$other_alarm_indicators_New_model
               number_of_cases<-input$number_of_cases_New_model
               base_vars<-c("district","year","week")
               
               data_augmented0<-var_names_New_model()$data_augmented |> 
                 dplyr::mutate(Cases=.data[[number_of_cases]],
                               Pop=.data[[pop.var.dat]],
                               log_Pop=log(.data[[pop.var.dat]]/1e5))
               
               source("New_model_helper_Functions_v2.R",local =T)
               
               source("Lag_Model_selection_ewars_By_District.R",local =T)
               
               
               alarm_vars<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$alarm_vars
               covar_to_Plot<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$covar_to_Plot
               vars_get_summary<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$vars_get_summary
               
               all_Plot_Poly<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$all_Plot_Poly
               all_xts_Plots<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$all_xts_Plots
               leaflet_plots<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$leaflet_plots
               nlag<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$nlag
               data_augmented<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$data_augmented
               all_endemic<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$all_endemic
               sel_var_endemic<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$sel_var_endemic
               names_cov_Plot<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$names_cov_Plot
               
               dist<-dat_fl$district
               years.dat<-sort(unique(dat_fl$year))
               
               #all_xts_Plots<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$all_xts_Plots
               
               for(i in 1:length(covar_to_Plot)){
                 
                 text_rend1<-paste0('output$time_series_plot_',i,'<-renderDygraph({
                     all_xts_Plots[[',i,']]})'
                 )
                 eval(parse(text=text_rend1))
               }
               
               
               
               ## update the UI lag plots
               alarm.indicators<-alarm_indicators
               
               cat(paste('\nValidation year Beg ..\n',input$new_model_Year_validation),'\n\n')
               
               ## compute lag of environmental variables
               
               dat_A<-var_names_New_model()$dat %>% 
                 dplyr::arrange(district,year,week) %>% 
                 dplyr::filter(district %in% boundary_file$district &!week==53)
               
               ## balance the observations to compute expected cases correctly
               
               beg.year<-min(dat_A$year)
               end.year<-max(dat_A$year)
               
               
               #source("reactive_Model_script.R",local=T)
              
               
               
               ## Descriptive Plots
               observeEvent(input$district_new,{# Output 1
                 #require(input$district_new)
                 
                 ##update the other district inputs
                 
                 updateSelectInput(session,"district_seas",
                                   selected =input$district_new)
                 updateSelectInput(session,"district_validation",
                                   selected =input$district_new)


                 dist_padded<-str_pad(input$district_new,side="left",width=3,pad=0)
                 shiny_obj_pth<-file.path(shiny_obj_Main_pth,paste0("District_",dist_padded))
                 
                 tab_Dat<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$tab_Dat
                 tab_Dat_lag<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$tab_Dat_lag
                 
                 dat_kl<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$dat_kl
                 all_kl_cmd<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$all_kl_cmd
                 all_kl_cmd1<-str_replace(all_kl_cmd,'unique[(]dat_sum_long[$]district[)]','input$district_new')
                 
                 #alarm_vars<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$alarm_vars
                 #covar_to_Plot<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$covar_to_Plot
                 #vars_get_summary<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$vars_get_summary
                 plot_List0<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$summary_plots
                 
                 plot_List<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$plot_List
                 rows_01<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$rows_01
                 
                 
                 cat(paste0('\ncovar_to_Plot:\n'),paste(covar_to_Plot,sep=' '),'\n\n')
                 
                 
                 flex_tab1_tribble<-tribble(~var,
                                            "function(){",
                                            "tab_Dat |> ",
                                            "as_flextable() |>",
                                            "flextable::set_caption(as_paragraph(paste('District ',input$district_new)),",
                                            "fp_p=officer::fp_par(text.align = 'left')) |> ",
                                            "flextable::fit_to_width(max_width =12,unit='in') |> ",
                                            "flextable::fontsize(part='header',size=14) |> ",
                                            "flextable::fontsize(part='body',size=10) |> ",
                                            "flextable::font(part='all',fontname ='Courier') |> ",
                                            "flextable::padding(part='body',padding = 4) |> ",
                                            "flextable::bold(i=1,part ='header') |> ",
                                            "flextable::border_inner_h(border =border_prop) |> ",
                                            "flextable::color(i=1,color='#CCBB44',part='header') |> ",
                                            "flextable::bg(j=16,bg='#94a323') |> ",
                                            "flextable::align(part='all',align ='left') |> ",
                                            "htmltools_value()",
                                            "}"
                 )
                 
               
                 
                 #output$new_model_data_descriptives<-eval(parse(text=all_kl_cmd1))
                 #output$new_model_data_descriptives<-eval(parse(text=flex_tab1_tribble$var))
                 
                 border_prop<-officer::fp_border(width=0.5)
                 
                 
                 output$new_model_data_descriptives<-renderUI({
                   tab_Dat |> 
                     as_flextable() |> 
                     #flextable::autofit(add_w=0,add_h=10,unit='mm',part='body') |> 
                     flextable::set_caption(as_paragraph(paste('District ',input$district_new)),
                                            fp_p=officer::fp_par(text.align = "left")) |> 
                     flextable::fit_to_width(max_width =9,unit='in') |> 
                     flextable::fontsize(part='header',size=12) |> 
                     flextable::fontsize(part='body',size=9) |> 
                     flextable::font(part='all',fontname ="Arial") |> 
                     flextable::padding(part="body",padding = 2) |> 
                     flextable::bold(i=1,part ="header") |> 
                     flextable::border_inner_h(border =border_prop) |> 
                     flextable::color(i=1,color="#CCBB44",part="header") |> 
                     flextable::bg(j=16,bg="#94a323") |> 
                     flextable::align(part='body',align ="center") |> 
                     flextable::border_remove() |> 
                     flextable::hline() |> 
                     flextable::hline_top() |> 
                     flextable::htmltools_value()
                     
                 })
                 
                 border_prop<-officer::fp_border(width=0.5)
                 
                 output$lag_seleCtion_Tab<-renderUI({
                   tab_Dat_lag |> 
                     as_flextable() |> 
                     #flextable::autofit(add_w=0,add_h=10,unit='mm',part='body') |> 
                     flextable::set_caption(as_paragraph(paste('District ',input$district_new)),
                                            fp_p=officer::fp_par(text.align = "left")) |> 
                     flextable::fit_to_width(max_width =12,unit='in') |> 
                     flextable::fontsize(part='header',size=12) |> 
                     flextable::fontsize(part='body',size=9) |> 
                     flextable::font(part='all',fontname ="Arial") |> 
                     flextable::padding(part="body",padding = 2) |> 
                     flextable::bold(i=1,part ="header") |> 
                     flextable::border_inner_h(border =border_prop) |> 
                     flextable::color(i=1,color="#CCBB44",part="header") |> 
                     flextable::bg(i=rows_01,j=2:16,bg="#4477AA",part="body") |> 
                     flextable::border_remove() |> 
                     flextable::align(part='body',align ="center") |> 
                     flextable::hline() |> 
                     flextable::hline_top() |> 
                     flextable::htmltools_value()
                   
                 })
                 
                 
                 
                 cat(paste("Summary variables ::\n"),paste(vars_get_summary,collapse =','),'\n\n')

                 i<-1
                 for(i in 1:length(vars_get_summary)){
                   text_rend0<-paste0('output$descr_plot_',i,'<-renderPlot({
                     print(plot_List0[[',i,']])})'
                   )

                   eval(parse(text=text_rend0))
                 }

                
                 
                 
               })   # end of first observe event for descriptive Plots (# Output 1)
              
               ### Section for the Spatial Plots
               
               observeEvent(input$district_seas,{# Output 1
                 #require(input$district_new)
                 
                 ##update the other district inputs
                 
                 updateSelectInput(session,"district_new",
                                   selected =input$district_seas)
                 updateSelectInput(session,"district_validation",
                                   selected =input$district_seas)
                 
                 
                 dist_padded<-str_pad(input$district_seas,side="left",width=3,pad=0)
                 shiny_obj_pth<-file.path(shiny_obj_Main_pth,paste0("District_",dist_padded))
                 
                 seasonal_plot<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$seasonal_plot
                 
                 output$Seasonality_plot<-renderPlot({ 
                   print(seasonal_plot)
                 })
                 
                 
               })
              
               observeEvent(c(input$new_model_Year_plot,
                              input$new_model_Week_plot_spat),
                            { 
                              new_model_Year_plot<-input$new_model_Year_plot
                              var_p<-names_cov_Plot
                              
                              yr_week<-paste0(new_model_Year_plot,'_',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                              yr_week1<-paste0(new_model_Year_plot,':',str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                              
                              yr_week_input<-paste0(new_model_Year_plot,":",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                              yr_week_input1<-paste0(new_model_Year_plot,"_",str_pad(input$new_model_Week_plot_spat,side ="left",pad =0,width =2))
                              
                              par_text1<-get_UI_update_d(obj.plots=covar_to_Plot,
                                                         panel_Update="Spat_Covariate_Plots_new_Model" ,
                                                         shinyOutputType="leafletOutput",
                                                         cols=2,
                                                         out_ref="spat_cov")
                              
                              
                              output$Spat_Covariate_Plots_new_Model<-renderUI({
                                tagList(eval(par_text1))
                              })
                              cat(paste("\nfrom input ::",yr_week_input,'\n\n'))
                              
                              
                              #p<-1
                              
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
                              
                              
                              for(i in 1:length(covar_to_Plot)){
                                text_rend<-paste0('output$spat_cov_plot_',i,'<-renderLeaflet({
                                           plot_List[[',i,']]})'
                                )
                                eval(parse(text=text_rend))
                              }
                              
                              
                              
                              
                            })# End of Spatial Covariate section
               
               
               ##Lag Plots
               
               for_obs<-foreach(a=1:length(alarm_indicators),.combine =c) %do% paste0('input$',alarm_indicators[a],'_',1:3)
               
               observeEvent(eval(parse(text=paste0('c(',paste0(for_obs,',input$district_new',collapse =','),')'))),{## Lag section
                 
                 dist_padded<-str_pad(input$district_new,side="left",width=3,pad=0)
                 shiny_obj_pth<-file.path(shiny_obj_Main_pth,paste0("District_",dist_padded))
                 data.basis<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$Dat_mod_for_dlnn
                 nlag<-nlag
                 alarm_vars_lag<-input$alarm_indicators_New_model
                 all_basis_vars<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$all_basis_vars
                 basis_var_n<-paste0('all_basis_vars$',names(all_basis_vars))
                 alarm.indicators<-input$alarm_indicators_New_model
                 dat_slider<-data.basis
                 #all_slice<-foreach(a=1:length(alarm_vars_lag))%do% get_fluid_slice_Output(a,'alarm.indicators')
                 
                 #output$var_Slices_Plots<-renderUI(eval(parse(text=paste('tabsetPanel(',paste(unlist(all_slice),collapse =','),')'))))
                 
                 
                 par_lag1<-get_UI_update_d(obj.plots=alarm_vars_lag,
                                           panel_Update="Lag non linear plots" ,
                                           shinyOutputType="plotOutput",
                                           cols=1,
                                           out_ref="contour")
                 
                 
                 output$lag_contour_Plots<-renderUI({
                   tagList(eval(par_lag1))
                 })
                 basis_vars_lag<-names(all_basis_vars)
                 alarm_vars_lag<-input$alarm_indicators_New_model
                 #df1<<-df
                 
                 model_dlnm<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$dlnm_Model

               
                   get_lag_plots<-function(p){
                     
                     
                     if(str_detect(alarm_vars_lag[p],'rain|prec|precipitation')){
                       ylab.text<-paste(alarm_vars_lag[p],'(mm)')
                     }else if(str_detect(alarm_vars_lag[p],'temp')){
                       ylab.text<-paste0('"',alarm_vars_lag[p],'"','~(','~degree~C)')
                     }else if(str_detect(alarm_vars_lag[p],'RH|rh')){
                       ylab.text<-paste(alarm_vars_lag[p],'(%)')
                     }else{
                       ylab.text<-alarm_vars_lag[p]
                     }
                     
                     temp_var<-str_detect(alarm_vars_lag[p],'temp')
                     cat(paste(basis_var_n,'\n\n'))
                     
                     coef <- model_dlnm$summary.fixed$mean
                     vcov <- model_dlnm$misc$lincomb.derived.covariance.matrix
                     indt <- grep(basis_vars_lag[p], model_dlnm$names.fixed)
                     computed_basis<-eval(parse(text=(basis_var_n[p])))
                     #assign("computed_basis",eval(parse(text=(basis_var_n[p]))))
                     
                     centering.val=round(mean(data.basis[,alarm_vars_lag[p]],na.rm=T), 0)
                     
                     min<-round(min(data.basis[,alarm_vars_lag[p]],na.rm=T),0)
                     min.1<-min(data.basis[,alarm_vars_lag[p]],na.rm=T)
                     max<-round(max(data.basis[,alarm_vars_lag[p]],na.rm=T),0)
                     max.1<-max(data.basis[,alarm_vars_lag[p]],na.rm=T)
                     
                     val.slid<-pretty(min:max,50)
                     
                     
                     predt.h <- crosspred(computed_basis, coef = coef[indt], vcov=vcov[indt,indt],
                                          at=val.slid,
                                          model.link = "log", bylag = 0.25, cen =centering.val) 
                     
                     #dat_plot<-data.frame(x=predt.h$predvar,y=seq(0, nlag, 0.25),z=t(predt.h$matRRfit))
                     
                     plot_d<-reshape2::melt(predt.h$matRRfit) %>% 
                       dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                                     y=as.numeric(Var1),
                                     RR=value) %>% 
                       dplyr::select(x,y,RR)
                     
                     plot_d_lci<-reshape2::melt(predt.h$matRRlow) %>% 
                       dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                                     y=as.numeric(Var1),
                                     RR_low=value)%>% 
                       dplyr::select(x,y,RR_low)
                     
                     plot_d_hci<-reshape2::melt(predt.h$matRRhigh) %>% 
                       dplyr::mutate(x=as.numeric(str_remove(Var2,"lag")),
                                     y=as.numeric(Var1),
                                     RR_high=value)%>% 
                       dplyr::select(x,y,RR_high)
                     
                     pal <- rev(brewer.pal(11, "PRGn"))
                     levels <- pretty(predt.h$matRRfit, 20)
                     col1 <- colorRampPalette(pal[1:6])
                     col2 <- colorRampPalette(pal[6:11])
                     cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
                     
                     
                     lag_slices<-plot_d_lci %>% 
                       dplyr::left_join(plot_d,by=c("x","y")) %>% 
                       dplyr::left_join(plot_d_hci,by=c("x","y"))
                     
                     cont_plot<-ggplot(aes(x=x,y=y),data=plot_d)+
                       geom_contour_filled(aes(z=RR),
                                           breaks=levels)+
                       scale_fill_manual(values =cols)+
                       theme_bw()+
                       guides(fill=guide_coloursteps(reverse =F,
                                                     title="RR",
                                                     ticks=F,
                                                     barheight=grid::unit(10,'cm')))+
                       theme(panel.grid =element_blank(),
                             axis.title =element_text(size=16),
                             axis.text =element_text(size=14))+
                       scale_x_continuous(breaks=0:input$nlags,
                                          expand =c(0,0))+
                       scale_y_continuous(breaks=pretty(plot_d$y,15),
                                          expand =c(0,0))+
                       
                       xlab("Lag Weeks")+
                       if(temp_var){
                         ylab(eval(parse(text=ylab.text)))
                       }else{
                         ylab(ylab.text)
                       }
                     
                     pout<-list(cont_plot,lag_slices)
                     names(pout)<-c(paste0("ContPlot_",alarm_vars_lag[p]),paste0("datLagSlices_",alarm_vars_lag[p]))
                     pout
                   }
                   #browser()
                   all_cont_dat_plots<-foreach(a=1:length(alarm_vars),.combine =c)%do% get_lag_plots(a)
                   all_cont_plots<-all_cont_dat_plots[grep('ContPlot',names(all_cont_dat_plots))]
                   all_datLagSlices<-all_cont_dat_plots[grep('datLagSlices',names(all_cont_dat_plots))]
                   
                   ## send to the output
                   #Update  object
                   
                   
                   
                   for(i in 1:length(alarm_vars_lag)){
                     cont_rend<-paste0('output$contour_plot_',i,'<-renderPlot({
                     all_cont_plots[[',i,']]})'
                     )
                     eval(parse(text=cont_rend))
                   }
                   
                   
                   basis_vars_lag<-names(all_basis_vars)
                   alarm_vars_lag<-input$alarm_indicators_New_model
                   #cat("it ran oh!!",sep='\n')
                   
                   
                   get_lag_slice_plots<-function(p){
                     
                     
                     
                     centering.val<-round(mean(data.basis[,alarm_vars_lag[p]],na.rm=T), 0)
                     
                     
                     if(str_detect(alarm_vars_lag[p],'rain|prec|precipitation')){
                       cen.text<-paste0('Reference=',centering.val,'mm')
                     }else if(str_detect(alarm_vars_lag[p],'temp')){
                       cen.text<-paste0("'Reference='~",centering.val,'~degree~C')
                     }else if(str_detect(alarm_vars_lag[p],'RH|rh')){
                       cen.text<-paste0('Reference=',centering.val,'%')
                     }else{
                       cen.text<-paste0('Reference=',centering.val)
                     }
                     
                     temp_var<-str_detect(alarm_vars_lag[p],'temp')
                     
                     ## get values to choose
                     
                     ad.v<-paste0('input$',alarm_vars_lag[p],'_',1:3)
                     
                     cat(paste("selected lag values ..\n\n"),eval(parse(text=paste0('c(',paste0(ad.v,collapse =','),')'))),'\n')
                     
                     sel_sli_p<-eval(parse(text=paste0('c(',paste0(ad.v,collapse =','),')')))
                     
                     paste(input$meantemperature_1)
                     #dat_plot<-data.frame(x=predt.h$predvar,y=seq(0, nlag, 0.25),z=t(predt.h$matRRfit))
                     
                     
                     lag_slices<-all_datLagSlices[[p]]
                     pal <- rev(brewer.pal(11, "PRGn"))
                     levels <- pretty(lag_slices$RR, 20)
                     col1 <- colorRampPalette(pal[1:6])
                     col2 <- colorRampPalette(pal[6:11])
                     cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))
                     
                     
                     #unique(plot_d$y)
                     #lag_slices_see<<-lag_slices
                     dat_some<-lag_slices %>% 
                       dplyr::filter(y%in%  sort(sel_sli_p)) %>% 
                       #dplyr::filter(y%in%  sample(lag_slices$y,3)) %>% 
                       dplyr::mutate(y_fac=as.factor(y),
                                     temp_var=temp_var,
                                     ref=cen.text
                       )
                     
                     
                     names(dat_some)
                     
                     min.y_s<-min(c(dat_some$RR_low,dat_some$RR,dat_some$RR_high))
                     max.y_s<-max(c(dat_some$RR_low,dat_some$RR,dat_some$RR_high))
                     
                     
                     
                     #library(viridis)
                     
                     slice.p<- ggplot(aes(x=x,y=RR),data=dat_some)+
                       geom_line(aes(col=y_fac))+
                       geom_ribbon(aes(ymin=RR_low,ymax=RR_high,fill=y_fac),alpha=0.1,
                                   show.legend =F)+
                       geom_hline(aes(yintercept=1),lty=2,linewidth=0.8)+
                       facet_wrap(~ref,labeller =if(temp_var){label_parsed}else{label_value})+
                       theme_bw()+
                       scale_color_manual(values=viridis::plasma(20)[c(2,14,8)] )+
                       scale_fill_manual(values=viridis::plasma(20)[c(2,14,8)])+
                       scale_x_continuous(breaks=0:input$nlags,
                                          expand =c(0,0))+
                       scale_y_continuous(limits =c(min.y_s,max.y_s*1.5))+
                       xlab("Lag Weeks")+
                       ylab("RR")+
                       guides(col=guide_legend(title=alarm_vars_lag[p]))+
                       theme(panel.grid.minor =element_blank(),
                             strip.text =element_text(size=18,face="bold",colour ="black"),
                             legend.text =element_text(size=18),
                             legend.title =element_text(size=18),
                             axis.text =element_text(size=14),
                             axis.title =element_text(size=16),
                             legend.position ="bottom")
                     
                     pout<-list(slice.p)
                     names(pout)<-alarm_vars_lag[p]
                     pout
                   }
                   
                   all_lag_slice_plots<-foreach(a=1:length(basis_vars_lag),.combine =c)%do% get_lag_slice_plots(a)
                   #all_lag_slice_plots_check<<-all_lag_slice_plots
                   
                   
                   for(i in 1:length(alarm_vars_lag)){
                     out_name<-paste0(alarm_vars_lag[i],'_Slice_plot')
                     cont_rend<-paste0('output$',out_name,'<-renderPlot({
                     all_lag_slice_plots[[',i,']]})'
                     )
                     eval(parse(text=cont_rend))
                   }
                   
                   
                   
                   # })
                 
               })# end of Lag Section
               
               #########----------------------------------------------------------------------------################################
              
               #Model Validations output
               
               observeEvent(c(input$district_validation,
                              input$z_outbreak_new,
                              input$Optimal_z
               ),
               { ## begin of Model Validation
                 
                 
                 updateSelectInput(session,"district_seas",
                                   selected =input$district_validation)
                 updateSelectInput(session,"district_new",
                                   selected =input$district_validation)
                 
                 new_model_Year_validation<-input$new_model_Year_validation
                 district_validation<-input$district_validation
                 dist_padded<-str_pad(input$district_validation,side="left",width=3,pad=0)
                 shiny_obj_pth<-file.path(shiny_obj_Main_pth,paste0("District_",dist_padded))
                 
                 model_final_rw<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$model_final_rw
                 all_cv<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$all_cv
                 y.PREDS<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$y.PREDS
                 
                 selected_zvalue<-readRDS(file.path(shiny_obj_pth,"Shiny_Objs.rds"))$selected_zvalue
                 
                 # if(input$Optimal_z==T){
                 #   Z_Value<-selected_zvalue
                 # }else{
                 #   Z_Value<-input$z_outbreak_new
                 # }
                 
                
                   if(isolate(input$Optimal_z)){
                   updateSliderInput(session,
                                     "z_outbreak_new",
                                     value =selected_zvalue)
                   }
               
                
                 Z_Value<-input$z_outbreak_new
                 
                 cat(paste('Z_value::',Z_Value),'\n\n')
                 
                 for_endemic<-all_endemic |> 
                   dplyr::filter(district==district_validation) |> 
                   dplyr::mutate(threshold_cases=mean_cases+input$z_outbreak_new*(sd_cases),
                                 threshold_rate=mean_rate+Z_Value*(sd_rate))
                 
                 dat_augmented_Sub<-data_augmented |> 
                   dplyr::filter(district==district_validation) |> 
                   dplyr::arrange(district,year,week)
                 
                 dat.4.endemic<-dat_augmented_Sub[,sel_var_endemic]
                 names(dat.4.endemic)<-c(base_vars,"cases","pop")
                 
                 idx_runin<-which(!dat_augmented_Sub$year>=new_model_Year_validation)
                 
                 runin_dat<-dat.4.endemic |> 
                   dplyr::filter(!year>=new_model_Year_validation) |> 
                   dplyr::mutate(observed_cases=cases,
                                 observed_rate=(cases/pop)*1e5,
                                 fitted=model_final_rw$summary.fitted.values$mean[idx_runin],
                                 fitted=(fitted/pop)*1e5,
                                 fittedp25=model_final_rw$summary.fitted.values$`0.025quant`[idx_runin],
                                 fittedp25=(fittedp25/pop)*1e5,
                                 fittedp975=model_final_rw$summary.fitted.values$`0.975quant`[idx_runin],
                                 fittedp975=(fittedp975/pop)*1e5,
                   ) %>% 
                   dplyr::left_join(for_endemic,by=c("district","year","week")) |> 
                   dplyr::mutate(
                     #outbreak=observed_cases>threshold_cases,
                     outbreak=observed_rate>threshold_rate,
                     observed_alarm=case_when(outbreak==1~observed_rate,
                                              TRUE~as.numeric(NA))) |> 
                   dplyr::select(district,year,week,observed_rate,fitted,fittedp25,fittedp975,outbreak,threshold_rate,observed_alarm)
                 
                 cat(paste("district is<<>>",district_validation),'\n')
                 
                 end.runin.year<-new_model_Year_validation-1
                 #date_week_runin<-seq.Date(as.Date(paste0(beg.year,'-01-01')),as.Date(paste0(end.runin.year,'-12-31')),by='week')[-1]
                 date_week_runin<-seq.Date(as.Date(paste0(beg.year,'-01-01')),as.Date(paste0(end.runin.year,'-12-31')),by='week')
                 
                 
                 data_Weeks_Runin<-data.frame(date=date_week_runin,
                                              year_week=format.Date(date_week_runin,"%Y_%W"),
                                              year=year(date_week_runin),
                                              stringsAsFactors =F,
                                              week=week(date_week_runin)) %>% 
                   mutate(Week=str_split_fixed(year_week,pattern ='_',n=2)[,2]) %>% 
                   dplyr::filter(as.numeric(Week)%in% 1:52)
                 
                 weeks.in.data_runin<-data_augmented %>% 
                   dplyr::mutate(year_week=paste0(year,'-',str_pad(week,side ="left",pad =0,width =2))) 
                 
                 year_week_S_runin<-data_Weeks_Runin %>% dplyr::group_by(year,Week) %>% 
                   dplyr::summarise(.groups="drop",date_Beg=min(date)) %>% 
                   dplyr::mutate(year_week=format.Date(date_Beg,"%Y-%W"))%>% 
                   dplyr::filter(year_week %in% weeks.in.data_runin$year_week)
                 
                 
          
                 data_plot_Runin<-runin_dat
                 
                 data_plot_RuninB<-data_plot_Runin |> 
                   dplyr::select(observed_rate,fitted,fittedp25,
                                 fittedp975,outbreak,
                                 threshold_rate,observed_alarm)  |>  
                   dplyr::rename(observed=observed_rate,
                                 threshold=threshold_rate)
                 
                 data_use_Runin_xts<-xts(data_plot_RuninB,order.by =year_week_S_runin$date_Beg,
                                         frequency=7)
                 
                 data_plot_RuninB1<-data_plot_Runin |> 
                   dplyr::mutate(date=year_week_S_runin$date_Beg) |> 
                   dplyr::select(date,observed_rate,fitted,fittedp25,
                                 fittedp975,outbreak,
                                 threshold_rate,observed_alarm)|> 
                   dplyr::rename(observed=observed_rate,
                                 threshold=threshold_rate)
                 
                 ## render the Plots
                 
                 plot.runin<-ggplot(aes(x=date),data=data_plot_RuninB1)+
                   geom_line(aes(x=date,y=observed,col="Observed"),linewidth=0.82)+
                   geom_line(aes(x=date,y=fitted,col='Predicted'),linewidth=1.2,lty=1)+
                   geom_line(aes(x=date,y=threshold),linewidth=1.1,lty=1,col='blue',show.legend =F)+
                   geom_area(aes(x=date,y=threshold,fill='Endemic'),alpha=0.7)+
                   geom_point(aes(x=date,y=observed_alarm,col="Outbreak"),size=3)+
                   geom_ribbon(aes(ymin=fittedp25,ymax=fittedp975,
                                   fill="95 % CI"),alpha=0.3)+
                   scale_color_manual(values=c("Observed"='#00336FFF',
                                               "Predicted"='#A72197FF',
                                               #"Endemic"='lightblue2',
                                               "Outbreak"='orange2'))+
                   scale_fill_manual(values=c("Endemic"='lightblue2',
                                              "95 % CI"=grey(0.3)))+
                   
                   scale_x_date(date_breaks ="52 weeks")+
                   theme_bw()+
                   guides(col=guide_legend(title =NULL),
                          fill=guide_legend(title =NULL))+
                   ylab("Incidence Rate\n per 100000")+
                   theme_bw()
                 
                 output$runin_ggplot_New_model<-renderPlot({
                   print(plot.runin)
                   
                 })
                 output$runin_interactive_New_model<-renderDygraph({
                   dygraph(data_use_Runin_xts,xlab ="Week",ylab="Incidence Rate\n per 100000")%>%
                     dySeries("observed",fillGraph=F,color ="grey") %>% 
                     dySeries("fitted",fillGraph=F,color ="blue2",label="Fitted") %>% 
                     dySeries("fittedp25",fillGraph=F,color ="orange2",label="Fitted p2.5") %>% 
                     dySeries("fittedp975",fillGraph=F,color ="orange3",label="Fitted p97.25") %>% 
                     dySeries("threshold",fillGraph=T,color ="purple",label ="Endemic channel") %>% 
                     dySeries("observed_alarm",fillGraph=T,color ="red",label ="Alarm",
                              drawPoints=T,pointSize =2,pointShape="circle") %>% 
                     dyRangeSelector() %>% 
                     dyLegend(show = "onmouseover") %>% 
                     dyHighlight(highlightCircleSize =2, 
                                 highlightSeriesBackgroundAlpha = 0.2,
                                 hideOnMouseOut = F)
                   
                   
                 })
                 
                 ## Validation Period Plots
                 dat_Sensitivity<-all_cv |> 
                   dplyr::select(district,year,week,Cases,mean_rate,sd_rate,week_Interval,Pop,mean,lci,uci) |> 
                   dplyr::mutate(obs_rate=(Cases/Pop)*1e5,
                                 p25=(lci/Pop)*1e5,
                                 p975=(uci/Pop)*1e5,
                                 predicted=(mean/Pop)*1e5,
                                 Threshold=mean_rate+(sd_rate*Z_Value)
                   ) |> 
                   dplyr::filter(week_Interval=="04")
                 
                 #names(y.PREDS)
                 
                 Combined_sensitivy<-dat_Sensitivity |> 
                   dplyr::left_join(y.PREDS,by=c("year","week","week_Interval")) |> 
                   dplyr::rename(pred_Cases=value) |> 
                   dplyr::mutate(pred_rate=(pred_Cases/Pop)*1e5,
                                 outbreak=as.numeric(obs_rate>Threshold),
                                 exceed=as.numeric(pred_rate>Threshold)) 
                 
                 ## compute probabilities
                 
                 
                 probs_Exceed<-Combined_sensitivy |> 
                   dplyr::group_by(district,year,week,week_Interval) |> 
                   dplyr::summarise(.groups ="drop",
                                    outbreak=mean(outbreak),
                                    total=n(),
                                    total_Exceed=sum(exceed),
                                    exceed_prob=mean(exceed))
                 
                 
                 
                 data_use_<-dat_Sensitivity |> 
                   dplyr::left_join(probs_Exceed,by=c("district","year","week"))|> 
                   dplyr::mutate(observed1=(Cases/Pop)*1e5,
                                 observed=case_when(is.na(observed1)~0,
                                                    TRUE~observed1),
                                 observed_alarm=case_when(outbreak==1~observed,
                                                          TRUE~as.numeric(NA)))
                 
                 cat("computed probs \n")
                 print(data_use_$exceed_prob)
                 idx.comp<-which(!is.na(data_use_$outbreak))
                 
                 roc_try<-try(reportROC(gold=as.numeric(data_use_$outbreak)[idx.comp],
                                        predictor=data_use_$exceed_prob[idx.comp]),outFile =warning("ROC_error.txt"))
                 
                 roc_tab_names<-c("Cutoff","AUC","AUC.SE","AUC.low","AUC.up","P","ACC",
                                  "ACC.low","ACC.up","SEN","SEN.low","SEN.up",
                                  "SPE","SPE.low","SPE.up","PLR","PLR.low",
                                  "PLR.up","NLR","NLR.low","NLR.up","PPV",
                                  "PPV.low","PPV.up","NPV","NPV.low","NPV.up")
                 
                 
                 
                 kdd<-data.frame(t(rep(as.character(NA),length(roc_tab_names))))
                 names(kdd)<-roc_tab_names
                 
                 if(class(roc_try) %in% c("NULL","try-error")){
                   roc_report<-kdd
                 }else{
                   roc_report<-reportROC(gold=as.numeric(data_use_$outbreak)[idx.comp],
                                         predictor=data_use_$exceed_prob[idx.comp])
                 }
                 
                 if(roc_report$Cutoff%in% c(NA,-Inf,NaN,Inf)){
                   sens_ppv<-tribble(~var,~val,~CI_Lower,~CI_Upper,
                                     "Cutoff probability",roc_report$Cutoff,NA,NA,
                                     "Area under the Curve (AUC)",roc_report$AUC,roc_report$AUC.low,roc_report$AUC.up,
                                     "Accuracy",roc_report$ACC ,roc_report$ACC.low,roc_report$ACC.up,
                                     "Sensitivity",roc_report$SEN,roc_report$SEN.low,roc_report$SEN.up,
                                     "Specificity",roc_report$SPE,roc_report$SPE.low,roc_report$SPE.up,
                                     "Positive Predictive Value (PPV)",roc_report$PPV,roc_report$PPV.low,roc_report$PPV.up,
                                     "Negative Predictive Value (NPV)",roc_report$NPV,roc_report$NPV.low,roc_report$NPV.up)
                   
                   data_use_a<-data_use_ %>% 
                     dplyr::mutate(prob_exceed=exceed_prob,
                                   cutoff=NA,
                                   validation_alarm=as.numeric(NA))
                   
                 }else{
                   sens_ppv<-tribble(~var,~val,~CI_Lower,~CI_Upper,
                                     "Cutoff probability",roc_report$Cutoff,NA,NA,
                                     "Area under the Curve (AUC)",roc_report$AUC,roc_report$AUC.low,roc_report$AUC.up,
                                     "Accuracy",roc_report$ACC ,roc_report$ACC.low,roc_report$ACC.up,
                                     "Sensitivity",roc_report$SEN,roc_report$SEN.low,roc_report$SEN.up,
                                     "Specificity",roc_report$SPE,roc_report$SPE.low,roc_report$SPE.up,
                                     "Positive Predictive Value (PPV)",roc_report$PPV,roc_report$PPV.low,roc_report$PPV.up,
                                     "Negative Predictive Value (NPV)",roc_report$NPV,roc_report$NPV.low,roc_report$NPV.up)  
                   
                   data_use_a<-data_use_ %>% 
                     dplyr::mutate(prob_exceed=exceed_prob,
                                   cutoff=as.numeric(roc_report$Cutoff),
                                   validation_alarm=case_when((prob_exceed>=cutoff)~prob_exceed,
                                                              TRUE~as.numeric(NA)))
                 }
                 
                 #data_test_ggplot<<-data_use_a
                 ratio_scale<-max(data_use_a$p975,na.rm =T)/max(data_use_a$prob_exceed,na.rm =T)
                 
                 
                 data_use_AA<-data_use_a %>% 
                   dplyr::mutate(Trend=1:n(),
                                 lab_week=paste0(year,'_',str_pad(week,width = 2,side="left",pad='0'))) %>% 
                   dplyr::rename(threshold=Threshold)
                 
                 Num_YYears<-length(unique(data_use_AA$year))
                 breaks_p<-seq(1,nrow(data_use_AA),4*Num_YYears)
                 

                 val.plot1<-ggplot(aes(x=Trend),data=data_use_AA)+
                   geom_line(aes(x=Trend,y=observed,col="Observed"),linewidth=0.82)+
                   geom_line(aes(x=Trend,y=predicted,col='Predicted'),linewidth=1.2,lty=1)+
                   geom_line(aes(x=Trend,y=threshold),linewidth=0.5,lty=1,col="blue",show.legend =F)+
                   geom_area(aes(x=Trend,y=threshold,fill='Endemic Channel'),alpha=0.6)+
                   geom_point(aes(x=Trend,y=observed_alarm,col="Outbreak"),size=3)+
                   geom_ribbon(aes(ymin=p25,ymax=p975,
                                   fill="95 % CI"),alpha=0.2)+
                   geom_point(aes(x=Trend,y=validation_alarm*ratio_scale,col="Alarm"),size=3)+
                   geom_line(aes(x=Trend,y=prob_exceed*ratio_scale,col="Excedance Probability"),linewidth=1)+
                   
                   geom_line(aes(x=Trend,y=cutoff*ratio_scale,col="Cutoff Prob"),size=1.2)+
                   scale_y_continuous(name = "Incidence Rate\n per 100000",sec.axis =sec_axis(~ . /ratio_scale,name="Probability"))+
                   scale_x_continuous(breaks=breaks_p,labels =data_use_AA$lab_week[breaks_p])+
                   
                   scale_color_manual(values=c("Observed"='#00336FFF',
                                               "Predicted"='#A72197FF',
                                               "Cutoff Prob"="red",
                                               "Excedance Probability"="yellowgreen",
                                               #"Endemic"='lightblue2',
                                               "Outbreak"='orange2',
                                               "Alarm"='blue'))+
                   scale_fill_manual(values=c("Endemic Channel"='lightblue2',
                                              "95 % CI"=grey(0.3)))+
                   ggtitle(paste('District:',district_validation))+
                   guides(col=guide_legend(title =NULL),
                          fill=guide_legend(title =NULL))+
                   ylab("DIR")+
                   xlab("Year Week")+
                   theme_bw()
                 
                 output$validation_ggplot_New_model<-renderPlot({
                   print(val.plot1)
                 })
                 
                 year_VAL<-sort(unique(data_use_AA$year))
                 date_week<-seq.Date(as.Date(paste0(min(year_VAL),'-01-01')),as.Date(paste0(max(year_VAL),'-12-31')),by='week')[-1]
                 
                 data_use_b<-data_use_a %>% 
                   dplyr::select(predicted,observed,Threshold,
                                 cutoff,prob_exceed,validation_alarm)%>% 
                   dplyr::rename(threshold=Threshold)
                 
                 data_use_xts<-xts(data_use_b,order.by =date_week,
                                   frequency=7)
                 
                 output$validation_interactive_New_model<-renderDygraph({
                   dygraph(data_use_xts,xlab ="Week",ylab="Incidence Rate\n per 100000")%>%
                     dySeries("prob_exceed",col="blue",stepPlot = F,axis="y2",label="Prob exceed") %>% 
                     dySeries("cutoff",col="red",stepPlot = F,axis="y2",label="Cutoff") %>% 
                     dySeries("observed",fillGraph=F,color ="yellowgreen") %>% 
                     dySeries("validation_alarm",fillGraph=F,color ="orange4",drawPoints=T,
                              pointSize =2,pointShape ="circle",axis="y2") %>% 
                     dySeries("threshold",fillGraph=T,color ="purple",label ="Endemic channel") %>% 
                     dyRangeSelector() %>% 
                     dyLegend(show = "onmouseover") %>% 
                     dyHighlight(highlightCircleSize =2, 
                                 highlightSeriesBackgroundAlpha = 0.2,
                                 hideOnMouseOut = F)
                 })
                 
                
                 ## Sensitivity Table
                 
                 distr_n<-district_validation
                 sen_spec_cmd<-paste(c("function() { sens_ppv","kbl(format='html',caption = paste('District ',distr_n))","kable_styling('striped', full_width = F)",
                                       "column_spec(2,background='#94a323')"),collapse ='%>%\n')
                 
                 
                 sen_spec_cmd<-paste(sen_spec_cmd,'}\n',collapse ='')
                 
                 border_prop<-officer::fp_border(width=0.5)
                 
                 
                 #output$sen_spec_table_New_model<-eval(parse(text=sen_spec_cmd))
                 output$sen_spec_table_New_model<-renderUI(sens_ppv |> 
                                                             flextable::qflextable() |> 
                                                             flextable::font(part = "all", fontname = "Arial") |> 
                                                             flextable::border_inner_h(border =border_prop) |> 
                                                             flextable::padding(part="body",padding = 2) |> 
                                                             flextable::set_header_labels(var="",
                                                                                          val="",
                                                                                          CI_Lower="Lower",
                                                                                          CI_Upper="Upper") |> 
                                                             flextable::add_header_row(values=c(NA,"CI"),colwidths =c(2,2)) |> 
                                                             flextable::align(align ="center",part ="header") |> 
                                                             flextable::align(j=2:4,align ="center",part ="body") |> 
                                                             flextable::bg(i=c(1,2,4,5),j=2,bg="#CCBB44") |> 
                                                             flextable::bg(i=1:2,part="header",bg='grey90') |> 
                                                             flextable::border_remove() |> 
                                                             flextable::hline() |> 
                                                             flextable::hline_top() |> 
                                                             flextable::htmltools_value())
                 
              
                 

               }) ## end of validation
               
               
              
               
             })# end of Code

