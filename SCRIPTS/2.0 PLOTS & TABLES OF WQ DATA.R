
# Load data ----

load(file = "DATA/ZetaPaperData.Rdata")

# Time series plots---- 

plotsigs<-sigs%>%#filter(is.na(doc_rem) == FALSE)%>%
          dplyr::select(obstime, datasplit, zetap, clr_uv_abs, coagulated_water_ph,
                        comb_filt_turb,  raw_uv_abs, 
                        raw_water_turb,  sprntnt_turb,
                        raw_water_ph,raw_col,comb_filt_al,
                        #pacl_pump, pacl_pumpspeed,
                        #pacl_pumpstroke, pacl_level, tank, 
                        flow, glad_prop,
                        est_sup_ret,datasplit,
                        clr_turb, hrs#, cbhl, hl_acc_rate
                        )%>%
          #mutate(pacl_pump = as.numeric(pacl_pump))%>%
          dplyr::rename(#`Coagulant pump in service` = pacl_pump,
                    #`Coagulant pump speed` = pacl_pumpspeed,
                    #`Coagulant pump stroke` = pacl_pumpstroke,
                    #`Coagulant tank in service` = tank,
                    `Coagulated water flow (l/s)` = flow,
                    `Proportion source B` = glad_prop,
                    `Return water (l/s)` = est_sup_ret)%>%
          dplyr::rename(`Zeta Potential (mV)` = zetap,
                        `Clarified UV254 (abs/m)` = clr_uv_abs,
                        `Raw UV254 (abs/m)` = raw_uv_abs,
                        `Coagulated pH` = coagulated_water_ph,
                        `Raw pH` = raw_water_ph,
                        `Raw Turbidity (NTU)` = raw_water_turb,
                        `Combined filtered turbidity (NTU)` = comb_filt_turb,
                        `Combined filtered Al (mgl)` = comb_filt_al,
                        #`Raw water temperature (C)` = temp,
                        `Raw colour (Hazen)`  = raw_col,
                        `Clarified turbdity (NTU)` = clr_turb,
                        `Return water turbidity (NTU)` = sprntnt_turb,
                        `Filter run time (hrs)` = hrs#,
                        #`Clean bed head loss (m)` = cbhl,
                        #`Head loss rate (m/hr)` = hl_acc_rate
                        )%>%
          gather(key = variable, value = val, -obstime,-datasplit)%>%
          dplyr::filter(obstime<"2018-04-05 00:00:00")



plotsigs_imputed<-sigs_imputed%>%#filter(is.na(doc_rem) == FALSE)%>%
          dplyr::select(obstime, datasplit, zetap, clr_uv_abs, coagulated_water_ph,
                        comb_filt_turb,  raw_uv_abs, 
                        raw_water_turb,  sprntnt_turb,
                        raw_water_ph, raw_col,comb_filt_al,
                        #pacl_pump, pacl_pumpspeed,
                        #pacl_pumpstroke, pacl_level, tank, 
                        flow, glad_prop, 
                        est_sup_ret,datasplit,
                        clr_turb, hrs#, cbhl, hl_acc_rate
                        )%>%
          #mutate(pacl_pump = as.numeric(pacl_pump))%>%
          dplyr::rename(#`Coagulant pump in service` = pacl_pump,
                    #`Coagulant pump speed` = pacl_pumpspeed,
                    #`Coagulant pump stroke` = pacl_pumpstroke,
                    #`Coagulant tank in service` = tank,
                    `Coagulated water flow (l/s)` = flow,
                    `Proportion source B` = glad_prop,
                    `Return water (l/s)` = est_sup_ret)%>%
          dplyr::rename(`Zeta Potential (mV)` = zetap,
                        `Clarified UV254 (abs/m)` = clr_uv_abs,
                        `Raw UV254 (abs/m)` = raw_uv_abs,
                        `Coagulated pH` = coagulated_water_ph,
                        `Raw pH` = raw_water_ph,
                        `Raw Turbidity (NTU)` = raw_water_turb,
                        `Combined filtered turbidity (NTU)` = comb_filt_turb,
                        `Combined filtered Al (mgl)` = comb_filt_al,
                        #`Raw water temperature (C)` = temp,
                        `Raw colour (Hazen)`  = raw_col,
                        `Clarified turbdity (NTU)` = clr_turb,
                        `Return water turbidity (NTU)` = sprntnt_turb,
                        `Filter run time (hrs)` = hrs#,
                        #`Clean bed head loss (m)` = cbhl,
                        #`Head loss rate (m/hr)` = hl_acc_rate
                        )%>%
            gather(key = variable, value = val, -obstime,-datasplit)%>%
          dplyr::filter(obstime<"2018-04-05 00:00:00")


label_y<- plotsigs_imputed%>%
    group_by(variable)%>%
    summarise(yloc = max(val,na.rm = T))%>%
    mutate(yloc2 = c(1.7,9.5,6.6,460,0.1,0.33,43,0.51,46,7.19,18,15,10,3.5))#,7.5,3.5


tsplots<-ggplot(plotsigs_imputed, aes(x = obstime, y = val))+
          geom_rect(aes( xmin = min(obstime) , xmax = ymd_hms("2017-10-20 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Training"), alpha = 0.1)+
          geom_rect(aes( xmin = ymd_hms("2017-10-20 00:00:00"), xmax = ymd_hms("2018-01-24 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 1", colour =  NULL), alpha = 0.1)+
          geom_rect(aes( xmin = ymd_hms("2018-01-24 00:00:00"), xmax = ymd_hms("2018-02-28 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 2", colour =  NULL), alpha = 0.1)+
          geom_rect(aes( xmin = ymd_hms("2018-02-28 00:00:00"), xmax = ymd_hms("2018-03-27 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 3", colour =  NULL), alpha = 0.1)+
          geom_rect(aes( xmin = ymd_hms("2018-03-27 00:00:00"), xmax = max(obstime), ymax = Inf, ymin = -Inf, fill = "Test 4"), alpha = 0.1)+
          geom_point(size = 0.2, alpha = 1, aes(colour = "Imputed value"))+
          geom_point(data = plotsigs, size = 0.2, alpha = 1, aes(colour = "Observation"))+
          theme_minimal()+
          facet_wrap(~variable, ncol =2, scales = "free_y")+
          labs(#title = "Water quality & operational parameters over study period",
               y = "",
               x = "Date")+
          scale_color_brewer(palette = "Set1", name = "Missing data")+
          scale_fill_brewer(palette = "Set3", name = "Data split")+
          theme(legend.position="bottom",legend.direction="horizontal")+
          geom_vline(xintercept = c(min(ts_eg$obstime),max(ts_eg$obstime)), aes(colour = "blue"))+
          geom_text(data = label_y, aes( x = ymd_hms("2016-11-01 00:00:00"), y = yloc2, label = LETTERS[1:15]), colour = "black", size = 5)



ggsave(tsplots,
       filename = "Multi time series plot.png",
       path = "PLOTS",
       device = "png",
       width = 22, height = 30, units = "cm" )


for(i in(1: length(unique(label_y$variable)))){
    
    sub_plot_data<-plotsigs_imputed%>%
        dplyr::filter(variable == label_y$variable[i])
    
    
    sub_plot_label_data<- label_y%>%
        dplyr::filter(variable == label_y$variable[i])
        
    
    
    trend<- ggplot(sub_plot_data, aes(x = obstime, y = val))+
        geom_rect(aes( xmin = min(obstime) , xmax = ymd_hms("2017-10-20 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Training"), alpha = 0.1)+
        geom_rect(aes( xmin = ymd_hms("2017-10-20 00:00:00"), xmax = ymd_hms("2018-01-24 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 1", colour =  NULL), alpha = 0.3)+
        geom_rect(aes( xmin = ymd_hms("2018-01-24 00:00:00"), xmax = ymd_hms("2018-02-28 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 2", colour =  NULL), alpha = 0.3)+
        geom_rect(aes( xmin = ymd_hms("2018-02-28 00:00:00"), xmax = ymd_hms("2018-03-27 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 3", colour =  NULL), alpha = 0.3)+
        geom_rect(aes( xmin = ymd_hms("2018-03-27 00:00:00"), xmax = max(obstime), ymax = Inf, ymin = -Inf, fill = "Test 4"), alpha = 0.1)+
        geom_point(size = 0.2, alpha = 1, aes(colour = "Imputed value"))+
        geom_point(data = sub_plot_data, size = 0.2, alpha = 1, aes(colour = "Observation"))+
        theme_minimal()+
        facet_wrap(~variable, ncol =2, scales = "free_y", strip.position = "left")+
        labs(#title = "Water quality & operational parameters over study period",
            y = "",
            x = "Date")+
        scale_color_brewer(palette = "Set1", name = "Missing data")+
        scale_fill_brewer(palette = "Set3", name = "Data split")+
        theme(legend.position="bottom",legend.direction="vertical", strip.placement = "outside")+
        geom_vline(xintercept = c(min(ts_eg$obstime),max(ts_eg$obstime)), aes(colour = "blue"))+
        geom_text(data = sub_plot_label_data, aes( x = ymd_hms("2016-11-01 00:00:00"), y = yloc, label = LETTERS[i]), colour = "black", size = 5)
    
    ggsave(trend, filename = paste0("PLOTS/Single time series plot",LETTERS[i],".png"), width = 18, height = 15, units = "cm")
    
}





# summary stats table from raw water----


stargazer(ros_rwq_summary, summary = FALSE,rownames =FALSE,
          type = "html",
          title = "Summary of raw water quality data", 
          out = "TABLES/Raw Wate Quality Data.doc",
          label = "tab:rwq", font.size= "tiny", column.sep.width = "1pt",
          digits = 2)








# Long term xyplots----




ds_plotsigs<-sigs%>%
          mutate(fl_fct = cut(flow, breaks = c(325,375,425)),
                 temp_fct = cut(temp, breaks = c(0,5,10,20), labels = c("< 5°C", "5 - 10°C","> 10°C")),
                 zeta_fct = cut(zetap, breaks = c(-7,-3,-1,1,3,7), labels = c("-7 to -3","-3 to -1","-1 to +1", "+1 to +3", "+3 to +7")),
                 rel_delta_uv= delta_uv/raw_uv_abs)%>%
          dplyr::filter(glad_prop_fact =="40% B"|
                                  glad_prop_fact =="45% B"|
                                  glad_prop_fact == "50% B"|
                                  glad_prop_fact =="55% B")%>%
          dplyr::filter(flow>350, flow<450)%>%
          mutate(glad_prop_fact = droplevels(glad_prop_fact),
                 temp_prop = interaction(temp_fct, glad_prop_fact),
                 temp_prop = droplevels(temp_prop))

# downstream performance plots



# XY Plots to show downstream influence of zeta potential----

plt_doc_rem<-ggplot(ds_plotsigs
                    , aes(x = zetap, y = rel_delta_uv))+
          geom_point(aes (size = flow, colour = temp, group = temp_fct))+
          #geom_boxplot(aes(x = zetap, group = cut(zetap, breaks = c(-7,-3,-1,1,3,7))), alpha = 0.1)+
          stat_smooth(method = "lm", formula = y~poly(x,2))+
          theme_minimal()+
          labs(title = paste0("Relative ", expression(Delta)," UVabs between raw & clarified water"),
               y = "(Raw - Clarified) / Raw UVabs",
               x = "Zeta potential")+
          scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
          scale_shape_discrete(name = "Proportion source B")+
          scale_size_continuous(name = "Flow l/s", range = c(1,5))+
          theme(legend.position="bottom",legend.direction="horizontal")#+
#facet_grid(glad_prop_fact~temp_fct)
#facet_wrap(~glad_prop_fact)

plt_doc_rem


ggsave(plt_doc_rem, filename = "doc_rem.png", path = "PLOTS", width = 15, height = 15, units = "cm")




plt_clr_turb<-ggplot(ds_plotsigs
                     , aes(x = zetap, y = clr_turb))+
          geom_point(aes( size = flow, colour = temp))+
          #geom_boxplot(aes(x = zetap, group = cut(zetap, breaks = c(-7,-3,-1,1,3,7))), alpha = 0.1)+
          stat_smooth(method = "lm", formula = y~poly(x,2))+
          theme_minimal()+
          labs(title = "Clarified turbidity over zeta potential range",
               y = "Clarified turbidity (NTU)",
               x = "Zeta potential")+
          scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
          scale_size_continuous(name = "Flow l/s", range = c(1,5))+
          theme(legend.position="bottom",legend.direction="horizontal")+
          ylim(0.5,1.75)


ggsave(plt_clr_turb, filename = "clr_turb.png", path = "PLOTS", width = 15, height = 15, units = "cm")




plt_flt_turb<-ggplot(ds_plotsigs
                     , aes(x = zetap, y = comb_filt_turb))+
          geom_point(aes(size = flow, colour = temp))+
          #geom_boxplot(aes(x = zetap, group = cut(zetap, breaks = c(-7,-3,-1,1,3,7))), alpha = 0.1)+
          stat_smooth(method = "lm", formula = y~poly(x,2))+
          theme_minimal()+
          scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
          scale_size_continuous(name = "Flow l/s", range = c(1,5))+
          theme(legend.position="bottom",legend.direction="horizontal")+
          labs(title = "Filtered turbdity over zeta potential range",
               y = "Filtered turbidity (NTU)",
               x = "Zeta potential")+ 
          scale_y_log10(breaks=c(0.03,0.05,.1,.2,.3),labels=c(0.03,.05,.1,.2,.3), limits = c(0.03,0.3))+
          annotation_logticks(sides  = "l")

#facet_grid(glad_prop_fact~temp_fct)
#facet_wrap(~glad_prop_fact)

#plt_flt_turb


ggsave(plt_flt_turb, filename = "flt_turb.png", path = "PLOTS", width = 15, height = 15, units = "cm")








plt_flt_al<-ggplot(ds_plotsigs
                   , aes(x = zetap, y = comb_filt_al))+
          geom_point(aes(size = flow, colour = temp))+
          #geom_boxplot(aes(x = zetap, group = cut(zetap, breaks = c(-7,-3,-1,1,3,7))), alpha = 0.1)+
          stat_smooth(method = "lm", formula = y~poly(x,2))+
          theme_minimal()+
          scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
          scale_size_continuous(name = "Flow l/s", range = c(1,5))+
          theme(legend.position="bottom",legend.direction="horizontal")+
          labs(title = "Residual Al over zeta potential range",
               y = "Al mg/l",
               x = "Zeta potential")+
          scale_y_log10(limits = c(0.001,0.2))+
          annotation_logticks(sides  = "l")




ggsave(plt_flt_al, filename = "flt_al.png", path = "PLOTS", width = 15, height = 15, units = "cm")


library(cowplot)


ds_cowleg <- get_legend(plt_doc_rem + theme(legend.position="bottom"))

multiplot_downstream_nl<-plot_grid(plt_doc_rem+theme(legend.position="none"), 
                                   plt_clr_turb+theme(legend.position="none"), 
                                   plt_flt_al+theme(legend.position="none"), 
                                   plt_flt_turb+theme(legend.position="none"),
                                   align = 'vh',
                                   labels = c("A", "B", "C", "D"),
                                   hjust = -1,
                                   nrow = 2,
                                   #as.table = FALSE,
                                   ncol = 2)


multiplot_downstream<-plot_grid( multiplot_downstream_nl, ds_cowleg, ncol = 1, rel_heights = c(1, .15))


ggsave(filename = "Treated water quality XYplot.png", plot = multiplot_downstream, path = "PLOTS/", width = 30, height = 30, units = "cm")


#multiplot_downstream


# BW Plots to show downstream influence of zeta potential----

bwplt_doc_rem<-ggplot(ds_plotsigs%>%dplyr::filter(is.na(zeta_fct)==F)
                    , aes( y = rel_delta_uv,x = zeta_fct))+
    geom_boxplot()+
    theme_minimal()+
    labs(title = paste0("Relative ", expression(Delta)," UVabs between raw & clarified water"),
         y = "(Raw - Clarified) / Raw UVabs",
         x = "Zeta potential")+
    theme(legend.position="bottom",legend.direction="horizontal")


bwplt_doc_rem


#ggsave(bwplt_doc_rem, filename = "doc_rem.png", path = "PLOTS", width = 15, height = 15, units = "cm")




bwplt_clr_turb<-ggplot(ds_plotsigs%>%dplyr::filter(is.na(zeta_fct)==F)
                     , aes(x = zeta_fct, y = clr_turb))+
    geom_boxplot()+
    theme_minimal()+
    labs(title = "Clarified turbidity over zeta potential range",
         y = "Clarified turbidity (NTU)",
         x = "Zeta potential")+
    theme(legend.position="bottom",legend.direction="horizontal")+
    ylim(0.5,1.75)

#ggsave(bwplt_clr_turb, filename = "clr_turb.png", path = "PLOTS", width = 15, height = 15, units = "cm")




bwplt_flt_turb<-ggplot(ds_plotsigs%>%dplyr::filter(is.na(zeta_fct)==F)
                     , aes(x = zeta_fct, y = comb_filt_turb))+
    geom_boxplot()+
    theme_minimal()+
    scale_size_continuous(name = "Flow l/s", range = c(1,5))+
    theme(legend.position="bottom",legend.direction="horizontal")+
    labs(title = "Filtered turbdity over zeta potential range",
         y = "Filtered turbidity (NTU)",
         x = "Zeta potential")+ 
    scale_y_log10(breaks=c(0.03,0.05,.1,.2,.3),labels=c(0.03,.05,.1,.2,.3), limits = c(0.03,0.3))+
    annotation_logticks(sides  = "l")



#bwplt_flt_turb


#ggsave(bwplt_flt_turb, filename = "flt_turb.png", path = "PLOTS", width = 15, height = 15, units = "cm")








bwplt_flt_al<-ggplot(ds_plotsigs%>%dplyr::filter(is.na(zeta_fct)==F)
                   , aes(x = zeta_fct, y = comb_filt_al))+
    geom_boxplot()+
    theme_minimal()+
    scale_size_continuous(name = "Flow l/s", range = c(1,5))+
    theme(legend.position="bottom",legend.direction="horizontal")+
    labs(title = "Residual Al over zeta potential range",
         y = "Al mg/l",
         x = "Zeta potential")+
    scale_y_log10(limits = c(0.001,0.2))+
    annotation_logticks(sides  = "l")




#ggsave(bwplt_flt_al, filename = "flt_al.png", path = "PLOTS", width = 15, height = 15, units = "cm")


library(cowplot)



multiplot_downstream_nl_bw<-plot_grid(bwplt_doc_rem+theme(legend.position="none"), 
                                   bwplt_clr_turb+theme(legend.position="none"), 
                                   bwplt_flt_al+theme(legend.position="none"), 
                                   bwplt_flt_turb+theme(legend.position="none"),
                                   align = 'vh',
                                   labels = c("A", "B", "C", "D"),
                                   hjust = -1,
                                   nrow = 2,
                                   #as.table = FALSE,
                                   ncol = 2)


multiplot_downstream<-plot_grid( multiplot_downstream_nl_bw,  ncol = 1, rel_heights = c(1, .15))


ggsave(filename = "Treated water quality plot BW.png", plot = multiplot_downstream, path = "PLOTS/", width = 30, height = 30, units = "cm")




# Plots to show downstream impact of pH ----
# 
# 
# 
# plt_doc_rem_ph<-ggplot(ds_plotsigs
#                     , aes(x = coagulated_water_ph, y = rel_delta_uv))+
#           geom_point(aes(size = flow, colour = temp, group = temp_fct))+
#           stat_smooth(method = "lm", formula = y~poly(x,2))+
#           theme_minimal()+
#           labs(title = paste0("Relative ", expression(Delta)," UVabs between raw & clarified water"),
#                y = "(Raw - Clarified) / Raw UVabs",
#                x = "Coagulated pH")+
#           scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
#           scale_shape_discrete(name = "Proportion source B")+
#           scale_size_continuous(name = "Flow l/s", range = c(1,5))+
#           theme(legend.position="bottom",legend.direction="horizontal")#+
# #facet_grid(glad_prop_fact~temp_fct)
# #facet_wrap(~glad_prop_fact)
# 
# #plt_doc_rem
# 
# 
# 
# 
# 
# 
# plt_clr_turb_ph<-ggplot(ds_plotsigs
#                      , aes(x = coagulated_water_ph, y = clr_turb))+
#           geom_point(aes( size = flow, colour = temp))+
#           stat_smooth(method = "lm", formula = y~poly(x,2))+
#           theme_minimal()+
#           labs(title = "Clarified turbidity over zeta potential range",
#                y = "Clarified turbidity (NTU)",
#                x = "Coagulated pH")+
#           scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
#           scale_size_continuous(name = "Flow l/s", range = c(1,5))+
#           theme(legend.position="bottom",legend.direction="horizontal")+
#           #scale_y_log10()+
#           ylim(0.25,1.25)#+
# #facet_grid(glad_prop_fact~temp_fct)
# #facet_wrap(~glad_prop_fact)
# 
# #plt_clr_turb
# 
# 
# #ggsave(plt_clr_turb, filename = "clr_turb.png", path = "ZETA1/PLOTS", width = 15, height = 15, units = "cm")
# 
# 
# 
# 
# plt_flt_turb_ph<-ggplot(ds_plotsigs
#                      , aes(x = coagulated_water_ph, y = comb_filt_turb))+
#           geom_point(aes(size = flow, colour = temp))+
#           stat_smooth(method = "lm", formula = y~poly(x,1))+
#           theme_minimal()+
#           scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
#           scale_size_continuous(name = "Flow l/s", range = c(1,5))+
#           theme(legend.position="bottom",legend.direction="horizontal")+
#           labs(title = "Filtered turbdity over zeta potential range",
#                y = "Filtered turbidity (NTU)",
#                x = "Coagulated pH")+
#           scale_y_log10()#+
# #facet_grid(glad_prop_fact~temp_fct)
# #facet_wrap(~glad_prop_fact)
# 
# #plt_flt_turb
# 
# 
# #ggsave(flt_turb, filename = "flt_turb.png", path = "ZETA1/PLOTS", width = 15, height = 15, units = "cm")
# 
# 
# 
# 
# 
# 
# 
# 
# plt_flt_al_ph<-ggplot(ds_plotsigs
#                    , aes(x = coagulated_water_ph, y = comb_filt_al))+
#           geom_point(aes(size = flow, colour = temp))+
#           stat_smooth(method = "lm", formula = y~poly(x,2))+
#           theme_minimal()+
#           scale_color_distiller(palette = "RdBu", name = expression("Temperature " ( degree~C)))+
#           scale_size_continuous(name = "Flow l/s", range = c(1,5))+
#           theme(legend.position="bottom",legend.direction="horizontal")+
#           labs(title = "Residual Al over zeta potential range",
#                y = "Al mg/l",
#                x = "Coagulated pH")+
#           scale_y_log10()#+
# #facet_grid(glad_prop_fact~temp_fct)
# #facet_wrap(~glad_prop_fact)
# 
# 
# 
# 
# #ggsave(flt_al, filename = "flt_al.png", path = "ZETA1/PLOTS", width = 15, height = 15, units = "cm")
# 
# 
# library(cowplot)
# 
# 
# ds_cowleg_ph <- get_legend(plt_doc_rem_ph + theme(legend.position="bottom"))
# 
# multiplot_downstream_nl_ph<-plot_grid(plt_doc_rem_ph+theme(legend.position="none"), 
#                                    plt_clr_turb_ph+theme(legend.position="none"), 
#                                    plt_flt_al_ph+theme(legend.position="none"), 
#                                    plt_flt_turb_ph+theme(legend.position="none"),
#                                    align = 'vh',
#                                    labels = c("A", "B", "C", "D"),
#                                    hjust = -1,
#                                    nrow = 2,
#                                    #as.table = FALSE,
#                                    ncol = 2)
# 
# 
# multiplot_downstream_ph<-plot_grid( multiplot_downstream_nl_ph, ds_cowleg_ph, ncol = 1, rel_heights = c(1, .15))
# 
# 
# ggsave(filename = "downstream_qualplot_ph.png", plot = multiplot_downstream_ph, path = "ZETA1/PLOTS", width = 30, height = 30, units = "cm")
# 
# 
RPushbullet::pbPost("note", "Script complete", "Initial figures complete")
# 
# 
rm(list=ls())
# 
# 
