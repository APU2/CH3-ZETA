load(file = "DATA/ZetaPaperData.Rdata")

# Short term time series example ----


verticals<- ts_eg%>%dplyr::filter(signal == "pacl_pumpstroke")%>%
    mutate(obstime = floor_date(obstime, unit = "2 hours"))%>%
    group_by(obstime)%>%
    summarise(value = mean(value, na.rm=T))%>%
    mutate(strokechange = diff(c(NA,value)))%>%
    dplyr::filter(abs(strokechange)>2)%>%
    arrange(obstime)


# plot_ts_eg<-ts_eg%>%
#     mutate(signal_lab = gsub("pacl_pumpstroke", "Coag pump stroke (%)", signal_lab),
#            signal_lab = gsub("comb_filt_turb", "Combined filter turbidity (NTU)", signal_lab),
#            signal_lab = gsub("comb_filt_al", "Combined filter Al (mgl)", signal_lab),
#            signal_lab = gsub("clr_turb", "Clarified turbidity (NTU)", signal_lab),
#            signal_lab = gsub("coagulated_water_ph", "Coagulated pH", signal_lab),
#            signal_lab = gsub("UV254 abs/cm", "Clarified UV254 (abs/cm)", signal_lab),
#            signal_lab = gsub("raw_uv_abs", "Raw UV254 (abs/cm)", signal_lab),
#            signal_lab = gsub("Zeta mV", "Zeta potential (mV)", signal_lab))

plot_ts_eg<- ts_eg%>%
    mutate(signal_lab = factor(signal_lab, 
                               levels = c("raw_uv_abs",
                                          "pacl_pumpstroke","coagulated_water_ph","Zeta mV",
                                          "UV254 abs/cm", "clr_turb",
                                          "comb_filt_turb","comb_filt_al"),
                               labels = c("Raw UV254 (abs/m)", 
                                          "Coag pump stroke (%)", "Coagulated pH", "Zeta potential (mV)",
                                          "Clarified UV254 (abs/m)", "Clarified turbidity (NTU)",
                                          "Combined filter turbidity (NTU)","Combined filter Al (mgl)" )))


eg_ts_tsplot<-ggplot(plot_ts_eg#%>%dplyr::filter(signal != "raw_uv_abs",
                     #                signal != "pacl_pumpstroke")
                     , aes(x = obstime, y = value))+
    geom_point()+
    geom_line(data = plot_ts_eg%>%#dplyr::filter(signal != "raw_uv_abs",
                  #             signal != "pacl_pumpstroke")%>%
                  mutate(obstime = round_date(obstime, unit = "3 hours"))%>%
                  group_by(obstime,signal_lab)%>%
                  summarise(value = mean(value, na.rm = TRUE)), colour = "blue", size = 2)+
    facet_wrap(~signal_lab, scales = "free_y", ncol = 2, strip.position = "left")+
    theme_minimal()+
    geom_vline(data = verticals, xintercept = verticals$obstime[-1])+
    labs(title = "Zeta potential change example timeseries",
         y = "",
         x = "Date")+
    theme(strip.placement = "outside")





ts_eg_wide<- plot_ts_eg%>%
    mutate(obstime = round_date(obstime, unit = "3 hours"))%>%
    group_by(obstime,signal_lab)%>%
    summarise(value = mean(value, na.rm = TRUE))%>%
    spread(key = signal_lab, value = value)

ts_eg_zetacomp<- ts_eg_wide%>%
    gather(key = "signal", value = "value", -obstime, -`Zeta potential (mV)`, -`Raw UV254 (abs/m)`)




lm_ts_eg_uv_zeta<-lm(`Clarified UV254 (abs/m)`~poly(`Zeta potential (mV)`,2), data=ts_eg_wide)


summary(lm_ts_eg_uv_zeta)



#summary(eg_lm)

eg_lm_dw<-dwtest(lm_ts_eg_uv_zeta)

eg_lm_acf<- Acf(residuals(lm_ts_eg_uv_zeta), plot = F)




ts_eg_zetacomp_plot<-ggplot(ts_eg_zetacomp, aes(x = `Zeta potential (mV)`, y = value))+
    geom_point()+
    facet_wrap(~signal, scales = "free_y", strip.position = "left", switch = "y")+
    stat_smooth( method = "lm", formula = y~poly(x,2))+
    labs(title = "Zeta potential change example xyplots",
         y = "",
         x = "Zeta potential (mV)")+
    theme_minimal()+
    theme(strip.placement = "outside")


ts_eg_phcomp<- ts_eg_wide%>%
    gather(key = "signal", value = "value", -obstime, -`Coagulated pH`, -`Raw UV254 (abs/m)`)


ts_eg_phcomp_plot<-ggplot(ts_eg_phcomp, aes(x = `Coagulated pH`, y = value))+
    geom_point()+
    facet_wrap(~signal, scales = "free_y")+
    stat_smooth( method = "lm", formula = y~poly(x,2))+
    labs(title = "Zeta potential change example xyplots",
         y = "",
         x = "Coagulated pH")+
    theme_minimal()+
    theme(strip.placement = "outside")






multiplot_eg<-plot_grid(eg_ts_tsplot,ts_eg_zetacomp_plot,
                        align = 'h',
                        labels = c("A", "B"),
                        hjust = -1,
                        nrow = 1,
                        #as.table = FALSE,
                        ncol = 2)


ggsave(filename = "multiplot_eg.png", plot = multiplot_eg, path = "PLOTS", width = 45, height = 25, units = "cm")




RPushbullet::pbPost("note", "Script complete", "Example time series figs & tabs")
rm(list=ls())
