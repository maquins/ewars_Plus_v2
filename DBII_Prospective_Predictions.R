## Update dashboard II for the new model

observeEvent(input$run_Pred,{
#eventReactive(input$dat_prospective,{
    
  
  #req(input$dat_prospective)
  source("New_model_helper_Functions_v2.R",local =T)
  
  ## read in DB1 variables
  
  out_Path<-file.path(getwd(),'Outputs')
  
  #folders_Create<-c("Lag_selection","Lag_Comb_selection","Weekly_crossValidations","For_Shiny")
  #folder_Vars<-c("path_dic1","path_dic2","cv_path","shiny_obj_pth")
  shiny_obj_Main_pth<-file.path(out_Path,"For_Shiny")
  shinyDBII_obj_Main_pth<-file.path(out_Path,"For_Shiny_DBII")
  
  all_files_Path<-file.path(out_Path,"For_Shiny","All_district")
  
  alarm_vars<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$alarm_vars
  population_var<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$population_var
  number_of_cases<-readRDS(file.path(all_files_Path,"Shiny_Objs_all.rds"))$number_of_cases
 
  
  
  
  Prospective_Data<-Uploaded_dat_DBII()$data |> 
    dplyr::mutate(Cases=.data[[number_of_cases]],
                  Pop=.data[[population_var]],
                  log_Pop=log(.data[[population_var]]/1e5),
                  pop_offset=log_Pop,
                  observed_rate=(Cases/Pop)*1e5)
  
  #pop_offset=log(population/1e5)
  
  cat(unique(Prospective_Data$district),sep='\n')
 
  ## update the districts Tab on DBII

  
  source("DBII_with_Weights_by_District.R",local=T)
  
  ## Update district pros with loaded prospective data
  
  observeEvent(c(input$district_prospective,
                 input$z_outbreak_new),{
    
                   dist_padded<-str_pad(input$district_prospective,side="left",width=3,pad=0)
                   shiny_obj_pth<-file.path(shiny_obj_Main_pth,paste0("District_",dist_padded))
                   shinyDBII_obj_pth<-file.path(shinyDBII_obj_Main_pth,paste0("District_",dist_padded))
                   #pred_weights_pth<-file.path(pred_weights_Main_pth,paste0("District_",dist_padded))
                   Plot_pros<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$Plot_pros
                   meta_Prediction_wide<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$meta_Prediction_wide
                   meta_Prediction<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$meta_Prediction
                   table_DB_Out_pros<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$table_Out_pros
                   Plot_obs_rate<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$Plot_obs_rate
                   ratio_DB2<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$ratio_DB2
                   dat_lab<-readRDS(file.path(shinyDBII_obj_pth,"Shiny_DBII_Objs.rds"))$dat_lab
                   cat(class(table_DB_Out_pros))
                   border_prop<-officer::fp_border(width=0.5)
                   
                   dist_Pred<-as.numeric(str_extract(Plot_pros$pred_Distance[1],'[:number:]+'))
                   
                   Plot_pros1<-Plot_pros |> 
                     dplyr::mutate(response_cat_a=lead(response_cat,dist_Pred)) |> 
                     dplyr::select(-response_cat) |> 
                     dplyr::rename(response_cat=response_cat_a)
                   
                   output$out_break_prob_plot_pros<-renderPlot({
                     ggplot(aes(x=week_seq,y=outbreak_moving_limit),data=Plot_pros)+
                       geom_area(aes(fill="Endemic channel"),alpha=0.6)+
                       geom_ribbon(aes(ymin=predicted_rate_lci,ymax=predicted_rate_uci,
                                       fill="Prediction Interval"),alpha=0.5)+
                       facet_wrap(~pred_Distance)+
                       geom_line(aes(y=outbreak,col="Predicted"),linewidth=0.3)+
                       geom_point(aes(y=outbreak,col="Predicted"),size=2.5,pch=15)+
                       
                       geom_line(aes(y=observed_rate,col="Observed rate"),linewidth=0.7,data=Plot_obs_rate)+
                       
                       geom_line(aes(x=week_seq,y=outbreak_probability*ratio_DB2,col="Outbreak probability"),linewidth=0.3,data=Plot_pros)+
                       geom_point(aes(x=week_seq,y=outbreak_probability*ratio_DB2,col="Outbreak probability"),size=2.5,pch=15,data=Plot_pros)+
                       geom_point(aes(x=week_seq,y=outbreak_probability_Signal*ratio_DB2,col="Alarm signal"),size=2.5,pch=8,data=Plot_pros)+
                       geom_line(aes(x=week_seq,y=alarm_threshold*ratio_DB2,col="Alarm threshold"),linewidth=0.5,data=Plot_pros)+
                       theme_bw()+
                       scale_fill_manual(values=c("Endemic channel"='#90CCE3',
                                                  "Prediction Interval"="#9C7BAF"))+
                       scale_color_manual(values =c("Predicted"='red1',
                                                    "Observed rate"='#46353A',
                                                    "Alarm signal"='#AA3377',
                                                    "Outbreak probability"='blue',
                                                    "Alarm threshold"="forest green"))+
                       scale_y_continuous(name = "Incidence Rate\n per 100000",sec.axis =sec_axis(~ . /ratio_DB2,name="Probability"))+
                       scale_x_continuous(breaks=Plot_pros$week_seq,label=Plot_pros$plot_lab)+
                       theme(panel.grid.major.x =element_blank(),
                             panel.grid.minor.x =element_blank(),
                             panel.grid.major.y =element_line(linetype=2),
                             panel.grid.minor.y =element_blank(),
                             axis.line.x.top =element_blank(),
                             strip.text =element_text(size=20,face='bold'),
                             strip.background =element_rect(fill="#CCBB44"),
                             axis.text.x.bottom =element_text(angle =90,size=14,vjust=0.5),
                             panel.border =element_blank(),
                             axis.line.y =element_line(linetype=1,colour="grey",linewidth=0.4,lineend="butt"),
                             axis.line.x =element_line(linetype=1,colour="grey",linewidth=0.4,lineend="butt"),
                             legend.position ="top",
                             axis.title =element_text(size=18),
                             axis.text.y =element_text(size=20,face='bold'),
                             legend.text =element_text(size=14)
                             
                       )+
                       guides(fill=guide_legend(title =NULL,nrow=2),
                              color=guide_legend(title =NULL,nrow=2))+
                       xlab("Prediction \nYear_Week")
                   })
                   
                   
                   dat_lab<-data.frame(response_cat=c("No response",
                                                      "Initial response",
                                                      "Early response",
                                                      "Late/emergency response"),
                                       x=-20,y=seq(0.65,2.5,0.5))
                   
                   output$response_plot_pros<-renderPlot({
                     
                     ggplot(aes(x=week_seq,y=response_cat),data=Plot_pros1)+geom_point(pch=21,size=2.5)+
                       geom_hline(yintercept =0.5,col="yellowgreen",linewidth=0.8)+
                       geom_hline(yintercept =1,col="orange",linewidth=0.8)+
                       geom_hline(yintercept =1.5,col="brown",linewidth=0.8)+
                       geom_hline(yintercept =2,col="red",linewidth=0.8)+
                       geom_text(aes(x=x,y=y,label=response_cat,
                                     size=8,
                                     col=response_cat),data=dat_lab,
                                 show.legend =F,hjust=0,nudge_x =0.2)+
                       theme_bw()+
                       #facet_wrap(~pred_Distance)+
                       
                       
                       scale_x_continuous(breaks=Plot_pros$week_seq,label=Plot_pros$plot_lab)+
                       
                       scale_color_manual(values=c("No response"='yellowgreen',
                                                   "Initial response"='orange',
                                                   "Early response"='brown',
                                                   "Late/emergency response"='red'))+
                       
                       theme(panel.grid.minor.y =element_blank(),
                             panel.grid.major.y =element_blank(),
                             panel.grid.major.x =element_blank(),
                             panel.grid.minor.x =element_blank(),
                             panel.border =element_blank(),
                             strip.background =element_rect(fill="#CCBB44"),
                             axis.line.x =element_line(linetype=1,
                                                       colour="grey",
                                                       linewidth=0.4,
                                                       lineend="butt"),
                             axis.text.x.bottom =element_text(angle =90,size=14,vjust=0.5),
                             axis.title.y =element_blank(),
                             strip.text =element_text(size=20,face='bold'),
                             axis.text.y=element_blank(),
                             axis.ticks.y =element_blank(),
                             axis.title =element_text(size=18),
                             legend.text =element_text(size=14))+
                       coord_fixed(6,ylim =c(0.3,3),xlim = c(-20,52))+
                       xlab("Prediction \nYear_Week")
                     
                   }) 
                   
                   ##ouput the table
                   output$prediction_tab_pros<-renderDataTable({data.table(table_DB_Out_pros)})
                   
                   output$pros_meta1<-renderUI({meta_Prediction_wide[,1] |> 
                       qflextable() |> 
                       flextable::fit_to_width(max_width =12,unit='in') |> 
                       flextable::fontsize(part='header',size=14) |> 
                       flextable::fontsize(part='body',size=12) |> 
                       flextable::font(part='all',fontname ="Arial") |> 
                       flextable::padding(part="body",padding = 2) |> 
                       flextable::bold(i=1,part ="header") |> 
                       flextable::border_inner_h(border =border_prop) |> 
                       flextable::color(i=1,color="#875C7A",part="header") |> 
                       #flextable::color(i=1,j=5,color="#EE6677",part="body") |> 
                       flextable::bg(i=1,bg="grey",part="header") |> 
                       #flextable::border_remove() |> 
                       flextable::align(part='all',align ="center") |> 
                       flextable::hline() |> 
                       flextable::vline() |> 
                       flextable::vline_left() |> 
                       flextable::hline_top() |> 
                       flextable::width(j=1,3,unit ='in') |> 
                       flextable::htmltools_value()})
                   
                   
                   output$pros_meta2<-renderUI({meta_Prediction_wide[,2] |> 
                       qflextable() |> 
                       flextable::fit_to_width(max_width =12,unit='in') |> 
                       flextable::fontsize(part='header',size=14) |> 
                       flextable::fontsize(part='body',size=12) |> 
                       flextable::font(part='all',fontname ="Arial") |> 
                       flextable::padding(part="body",padding = 2) |> 
                       flextable::bold(i=1,part ="header") |> 
                       flextable::border_inner_h(border =border_prop) |> 
                       flextable::color(i=1,color="#875C7A",part="header") |> 
                       #flextable::color(i=1,j=5,color="#EE6677",part="body") |> 
                       flextable::bg(i=1,bg="grey",part="header") |> 
                       #flextable::border_remove() |> 
                       flextable::align(part='all',align ="center") |> 
                       flextable::hline() |> 
                       flextable::vline() |> 
                       flextable::vline_left() |> 
                       flextable::hline_top() |> 
                       flextable::width(j=1,3,unit ='in') |> 
                       flextable::htmltools_value()})
                   
                   output$pros_meta3<-renderUI({meta_Prediction_wide[,3] |> 
                       qflextable() |> 
                       flextable::fit_to_width(max_width =12,unit='in') |> 
                       flextable::fontsize(part='header',size=14) |> 
                       flextable::fontsize(part='body',size=12) |> 
                       flextable::font(part='all',fontname ="Arial") |> 
                       flextable::padding(part="body",padding = 2) |> 
                       flextable::bold(i=1,part ="header") |> 
                       flextable::border_inner_h(border =border_prop) |> 
                       flextable::color(i=1,color="#875C7A",part="header") |> 
                       #flextable::color(i=1,j=5,color="#EE6677",part="body") |> 
                       flextable::bg(i=1,bg="grey",part="header") |> 
                       #flextable::border_remove() |> 
                       flextable::align(part='all',align ="center") |> 
                       flextable::hline() |> 
                       flextable::vline() |> 
                       flextable::vline_left() |> 
                       flextable::hline_top() |> 
                       flextable::width(j=1,3,unit ='in') |> 
                       flextable::htmltools_value()})
                   
                   output$pros_meta4<-renderUI({meta_Prediction_wide[,4] |> 
                       qflextable() |> 
                       flextable::fit_to_width(max_width =12,unit='in') |> 
                       flextable::fontsize(part='header',size=14) |> 
                       flextable::fontsize(part='body',size=12) |> 
                       flextable::font(part='all',fontname ="Arial") |> 
                       flextable::padding(part="body",padding = 2) |> 
                       flextable::bold(i=1,part ="header") |> 
                       flextable::border_inner_h(border =border_prop) |> 
                       flextable::color(i=1,color="#875C7A",part="header") |> 
                       #flextable::color(i=1,j=5,color="#EE6677",part="body") |> 
                       flextable::bg(i=1,bg="grey",part="header") |> 
                       #flextable::border_remove() |> 
                       flextable::align(part='all',align ="center") |> 
                       flextable::hline() |> 
                       flextable::vline() |> 
                       flextable::vline_left() |> 
                       flextable::hline_top() |> 
                       flextable::width(j=1,3,unit ='in') |> 
                       flextable::htmltools_value()})
                   
                   output$pros_meta5<-renderUI({meta_Prediction_wide[,5] |> 
                       qflextable() |> 
                       flextable::fit_to_width(max_width =12,unit='in') |> 
                       flextable::fontsize(part='header',size=14) |> 
                       flextable::fontsize(part='body',size=12) |> 
                       flextable::font(part='all',fontname ="Arial") |> 
                       flextable::padding(part="body",padding = 2) |> 
                       flextable::bold(i=1,part ="header") |> 
                       flextable::border_inner_h(border =border_prop) |> 
                       flextable::color(i=1,color="#875C7A",part="header") |> 
                       flextable::color(i=1,color="#EE6677",part="body") |> 
                       flextable::bg(i=1,bg="grey",part="header") |> 
                       #flextable::border_remove() |> 
                       flextable::align(part='all',align ="center") |> 
                       flextable::hline() |> 
                       flextable::vline() |> 
                       flextable::vline_left() |> 
                       flextable::hline_top() |> 
                       flextable::width(j=1,3,unit ='in') |> 
                       flextable::htmltools_value()})
                
    
  })
  
  
  
  
})
