

# load explanatory models data

load(file = "DATA/EXPLANATORY_GAMM_MODELS-2WAY INTERACTIONS DOC GOOD.Rdata")


ggplot(sigs_imputed%>%
           dplyr::filter(datasplit=="train",
                         year(obstime)==2017), aes(x = hrs))+
    geom_histogram()


# Fit diagnostics for supplementary material ----


# SAVE DIAGNOSTIC PLOTS ####

# UV254
summary(expl_gamdocrem)

png(filename = "PLOTS/PCA GAM DOC REM MDL.png",  width = 900, height = 500)
plot(expl_gamdocrem, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(expl_gamdocrem)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAM DOC REM DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(expl_gamdocrem)
dev.off()

# clr TURB
summary(expl_gamclr_turb)

png(filename = "PLOTS/PCA GAM CLR TURB MDL.png",  width = 900, height = 500)
plot(expl_gamclr_turb, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(expl_gamclr_turb)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAM CLR TURB DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(expl_gamclr_turb)
dev.off()


# filt turb
summary(expl_gamcomb_filt_turb)

png(filename = "PLOTS/PCA GAM FILT TURB MDL.png",  width = 900, height = 500)
plot(expl_gamcomb_filt_turb, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(expl_gamcomb_filt_turb)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAM FILT TURB DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(expl_gamcomb_filt_turb)
dev.off()


#filt Al
summary(expl_gamcomb_filt_al)

png(filename = "PLOTS/PCA GAM FILT AL MDL.png",  width = 900, height = 500)
plot(expl_gamcomb_filt_al, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(expl_gamcomb_filt_al)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAM FILT AL DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(expl_gamcomb_filt_al)
dev.off()





# SAVE DIAGNOSTIC PLOTS GAMM ####

# UV254
summary(gamm_docrem$gam)
anova(gamm_docrem$gam)

png(filename = "PLOTS/PCA GAMM DOC REM MDL.png",  width = 1200, height = 900)
plot(gamm_docrem$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_docrem$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM DOC REM DIAG.png",  width = 1200, height = 900)
par(mfrow = c(2,2))
gam.check(gamm_docrem$gam)
dev.off()

# clr TURB
summary(gamm_clr_turb$gam)

png(filename = "PLOTS/PCA GAMM CLR TURB MDL.png", width = 1200, height = 900)
plot(gamm_clr_turb$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_clr_turb$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM CLR TURB DIAG.png", width = 1200, height = 900)
par(mfrow = c(2,2))
gam.check(gamm_clr_turb$gam)
dev.off()


# filt turb
summary(gamm_comb_filt_turb$gam)

png(filename = "PLOTS/PCA GAMM FILT TURB MDL.png",  width = 1200, height = 900)
plot(gamm_comb_filt_turb$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_comb_filt_turb$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM FILT TURB DIAG.png",  width = 1200, height = 900)
par(mfrow = c(2,2))
gam.check(gamm_comb_filt_turb$gam)
dev.off()


#filt Al
summary(gamm_comb_filt_al$gam)

png(filename = "PLOTS/PCA GAMM FILT AL MDL.png", width = 1200, height = 900)
plot(gamm_comb_filt_al$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_comb_filt_al$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM FILT AL DIAG.png", width = 1200, height = 900)
par(mfrow = c(2,2))
gam.check(gamm_comb_filt_al$gam)
dev.off()


# acf plot of residuals


png(filename = "PLOTS/ACFPlots.png",  width = 1024, height = 500)
par(mfrow = c(2,4))
acf(residuals(expl_gamdocrem, type = "response" ), main = "GAM CLAR UV254")
acf(residuals(gamm_docrem$lme, type = "normalized"), main = "GAMM AR-1 CLAR UV254", col = "green")


acf(residuals(expl_gamclr_turb, type = "scaled.pearson" ), main = "GAM CLAR TURB")
acf(residuals(gamm_clr_turb$lme, type = "normalized"), main = "GAMM AR-1 CLAR TURB", col = "green")

acf(residuals(expl_gamcomb_filt_turb, type = "pearson" ), main = "GAM FILT TURB")
acf(residuals(gamm_comb_filt_turb$lme, type = "normalized"), main = "GAMM AR-1 FILT TURB", col = "green")

acf(residuals(expl_gamcomb_filt_al, type = "pearson" ), main = "GAM FILT Al")
acf(residuals(gamm_comb_filt_al$lme, type = "normalized"), main = "GAMM AR-1 FILT Al", col = "green")
dev.off()


# residuals pairs

residuals_plotting<-sigs_imputed_pre_processed%>%
    dplyr::filter(datasplit == "train")%>%
    dplyr::select(contains("PC"))%>%
    dplyr::mutate(resid_clr_uv = residuals(gamm_docrem$gam),
           resid_clr_turb = residuals(gamm_clr_turb$gam),
           resid_filt_turb = residuals(gamm_comb_filt_turb$gam),
           resid_filt_al = residuals(gamm_comb_filt_al$gam))


png(filename = "PLOTS/PAIRS PLOTS RESIDUALS & PRCOMPS.png",  width = 1500, height = 1200)
pairs(residuals_plotting,
      upper.panel = panel.cor, diag.panel = panel.hist)
dev.off()


## regression model tables----



stargazer(#expl_gamclr_turb,
          gamm_clr_turb$lme,
          #expl_gamdocrem,
          gamm_docrem$lme,
          #expl_gamcomb_filt_turb,
          gamm_comb_filt_turb$lme,
          #expl_gamcomb_filt_al,
          gamm_comb_filt_al$lme,
          dep.var.caption = "Downstream water quality models",
          dep.var.labels = c( "loge Clarified Turbidity (NTU)","Clarified UV254 (abs/m)",
                              "loge Filtered Turbidity (NTU)", "loge Filtered Al (mgl)"),
          type = "html", 
          title = "Summary of PC-GAMM models",
          out = "TABLES/ds_wq_mdl_tab_add.doc",
          align = T,
          model.names = T,
          model.numbers = T,
          column.labels = c( "loge Clarified Turbidity (NTU)","Clarified UV254 (abs/m)",
                             "loge Filtered Turbidity (NTU)", "loge Filtered Al (mgl)")
)






# Plot prediction results ----

outcome_lab<- data.frame(outcome = levels(prediction_results$outcome),
                         outcome_lab = c("Clarified Turbidity (NTU)",
                                         "Clarified UV254 (abs/m)",
                                         "Filtered water Al (mg/l)",
                                         "Combined filtered turbidity (NTU)"#,
                                         #"(Raw - Clarified) / Raw UVabs"
                         ))

# prediction_results<-bind_rows(expl_prediction_results%>%mutate(PC = "GAM"),
#                               pc_prediction_results%>%mutate(PC = "PC-GAM"))


plot_predictions<-prediction_results%>%
    ungroup()%>%
    dplyr::select(-error)%>%
    left_join(outcome_lab)%>%
    mutate(series = ifelse(cor=="COR", "GAMM (AR1)", "GAM"),
           value = predicted)%>%
    dplyr::select(obstime,model_names, outcome,series, value)%>%
    bind_rows(., prediction_results%>%
                  ungroup()%>%
                  mutate(series = "observed",
                         value = observed)%>%
                  dplyr::select(obstime,model_names,outcome,series,value))%>%
    left_join(., outcome_lab)%>%
    #dplyr::filter(series != "GAM")%>%
    mutate(series = factor(series, levels = c("GAM","GAMM (AR1)","observed"),ordered = T))

unique(plot_predictions$series)

label_y<- plot_predictions%>%
    group_by(outcome_lab)%>%
    summarise(yloc = quantile(value,0.99,na.rm = T))%>%
    mutate(series = "GAMM")
        #yloc2 = c(1.7,9,0.44,6.6,460,0.1,0.33,43,0.021,0.51,46,7.19,2.1,17.5,13,7.5,3.5))


prediction_ts<-ggplot(plot_predictions%>%
                          dplyr::filter(series != "GAM")
                      , 
                      aes(x = obstime,y = value,  order = series))+
    geom_rect(aes( xmin = min(obstime) , xmax = ymd_hms("2017-10-20 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Training"), alpha = 0.1)+
    geom_rect(aes( xmin = ymd_hms("2017-10-20 00:00:00"), xmax = ymd_hms("2018-01-24 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 1", colour =  NULL), alpha = 0.1)+
    geom_rect(aes( xmin = ymd_hms("2018-01-24 00:00:00"), xmax = ymd_hms("2018-02-28 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 2", colour =  NULL), alpha = 0.1)+
    geom_rect(aes( xmin = ymd_hms("2018-02-28 00:00:00"), xmax = ymd_hms("2018-03-27 00:00:00"), ymax = Inf, ymin = -Inf, fill = "Test 3", colour =  NULL), alpha = 0.1)+
    geom_rect(aes( xmin = ymd_hms("2018-03-27 00:00:00"), xmax = max(obstime), ymax = Inf, ymin = -Inf, fill = "Test 4"), alpha = 0.1)+
    geom_point(aes(col = series),size = 0.5)+
    facet_wrap(~outcome_lab,
               scales = "free", strip.position = "l")+
    theme_minimal()+
    labs(title = "Observed and predicted down-stream water quality",
         x = "Date",
         y = "Water quality value (NTU,mg/l,UVabs/m,NTU)")+
    scale_color_brewer(palette = "Dark2", name = "Series")+
    scale_fill_brewer(palette = "Set3", name = "Data split")+
    theme(legend.position="bottom",legend.direction="horizontal", strip.placement = "outside")+
    geom_text(data = label_y, aes( x = ymd_hms("2016-11-01 00:00:00"), y = yloc, label = LETTERS[1:4]), colour = "black", size = 5)

#prediction_ts

ggsave(filename = "prediction_test_ts.png", plot = prediction_ts, path = "PLOTS", width = 30, height = 20, units = "cm")



## table of test set statistics 1 ----


colnames(prediction_summary)


# prediction_summary<-#bind_rows(
#                     expl_prediction_summary%>%mutate(PC = "GAM")#,
#                               #pc_prediction_summary%>%mutate(PC = "PC-GAM"))

arranged_pred_summary<-prediction_summary%>%
    mutate(MAPE = round(MAPE))%>%
    gather(key = "stat", value = value, -model_names,-outcome,-zeta_used,-datasplit,  -cor)%>%
    unite(col = test, datasplit,stat,sep = " ")%>%
    spread(key = test, value = value,convert = T)%>%
    ungroup()%>%
    arrange(outcome)%>%
    ungroup()%>%
    left_join(outcome_lab)%>%
    dplyr::filter(#PC == "GAM",
        cor=="COR")


library(htmlTable)
prediction_results_summary_table<-htmlTable(arranged_pred_summary%>%
                                                mutate(corstruc = ifelse(cor=="COR", "AR1", " - "))%>%
                                                dplyr::select( corstruc, `train RMSE`, `train RSQ`, 
                                                               `train MAPE`,
                                                               `test1 RMSE`, `test1 RSQ`, 
                                                               `test1 MAPE`,
                                                               `test2 RMSE`, `test2 RSQ`, 
                                                               `test2 MAPE`,
                                                               `test3 RMSE`, `test3 RSQ`, 
                                                               `test3 MAPE`,
                                                               `test4 RMSE`, `test4 RSQ`, 
                                                               `test4 MAPE`)
                                            ,
                                            header =  c("Error Structure","RMSE","RSQ","MAPE",
                                                        "RMSE","RSQ","MAPE",
                                                        "RMSE","RSQ","MAPE",
                                                        "RMSE","RSQ","MAPE",
                                                        "RMSE","RSQ","MAPE"),
                                            rnames = arranged_pred_summary$outcome_lab,
                                            #rgroup = unique(arranged_pred_summary$outcome_lab),
                                            #n.rgroup = c(1,1,1,1),
                                            cgroup = c("Model", "Training set", "Test set 1", "Test set 2", "Test set 3", "Test set 4"),
                                            n.cgroup = c(1,3,3,3,3,3), 
                                            caption="Training & test set model performance statistics"#,
                                            #tfoot="&dagger; A table footer commment"
)








plot(gamm_docrem$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = T)
plot(gamm_clr_turb$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = T)
plot(gamm_comb_filt_turb$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = T)
plot(gamm_comb_filt_al$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = T)


gam.check(gamm_docrem$gam)
gam.check(gamm_clr_turb$gam)
gam.check(gamm_comb_filt_turb$gam)
gam.check(gamm_comb_filt_al$gam)

summary(gamm_docrem$gam)


## plotting of GAMMs #### 

library(mgcViz)
gamm_docrem_viz<-getViz(gamm_docrem$gam)
gamm_clr_turb_viz<-getViz(gamm_clr_turb$gam)
gamm_comb_filt_turb_viz<-getViz(gamm_comb_filt_turb$gam)
gamm_comb_filt_al_viz<-getViz(gamm_comb_filt_al$gam)


gam_plotting_data<- crossing(model = c("clr_uv","clr_turb","filt_turb","filt_al"),
                             )


gamm_docrem_viz_sm1<-sm(gamm_docrem_viz,1)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\ndayn smooth")
gamm_docrem_viz_sm2<-sm(gamm_docrem_viz,2)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nZeta potential smooth")
gamm_docrem_viz_sm3<-sm(gamm_docrem_viz,3)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nCoagulation pH smooth")
gamm_docrem_viz_sm4<-sm(gamm_docrem_viz,4)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nFlow smooth")
gamm_docrem_viz_sm5<-sm(gamm_docrem_viz,5)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nTemp smooth")
gamm_docrem_viz_sm6<-sm(gamm_docrem_viz,6)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nRaw UVabs smooth")
gamm_docrem_viz_sm7<-sm(gamm_docrem_viz,7)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nRaw water turb")
gamm_docrem_viz_sm8<-sm(gamm_docrem_viz,8)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nReturn water flow smooth")
gamm_docrem_viz_sm9<-sm(gamm_docrem_viz,9)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nReturn water turbidity smooth")
gamm_docrem_viz_sm10<-sm(gamm_docrem_viz,10)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "GAMM (UVin-UVout)/UVin\nSource 2 proportion smooth")
gamm_docrem_viz_sm11<-sm(gamm_docrem_viz,11)%>%plotSlice(.,fix = list("blend" = c(0.4,0.45,0.5,0.55)))+ l_fitRaster() + l_fitContour() + l_points()+ labs(title = "GAMM (UVin-UVout)/UVin\nZeta pH interaction")



gamm_clr_turb_viz_sm1<-sm(gamm_clr_turb_viz,1)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\ndayn smooth")
gamm_clr_turb_viz_sm2<-sm(gamm_clr_turb_viz,2)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nZeta potential smooth")
gamm_clr_turb_viz_sm3<-sm(gamm_clr_turb_viz,3)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nCoagulation pH smooth")
gamm_clr_turb_viz_sm4<-sm(gamm_clr_turb_viz,4)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nFlow smooth")
gamm_clr_turb_viz_sm5<-sm(gamm_clr_turb_viz,5)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nTemp smooth")
gamm_clr_turb_viz_sm6<-sm(gamm_clr_turb_viz,6)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nRaw UVabs smooth")
gamm_clr_turb_viz_sm7<-sm(gamm_clr_turb_viz,7)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nRaw water turb")
gamm_clr_turb_viz_sm8<-sm(gamm_clr_turb_viz,8)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nReturn water flow smooth")
gamm_clr_turb_viz_sm9<-sm(gamm_clr_turb_viz,9)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nReturn water turbidity smooth")
gamm_clr_turb_viz_sm10<-sm(gamm_clr_turb_viz,10)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Clarified turbidity\nSource 2 proportion smooth")
gamm_clr_turb_viz_sm11<-sm(gamm_clr_turb_viz,11)%>%plotSlice(.,fix = list("blend" = c(0.4,0.45,0.5,0.55)))+ l_fitRaster() + l_fitContour() + l_points()+ labs(title = "Clarified turbidity\nZeta pH interaction")



formula(gamm_comb_filt_turb$gam)

gamm_comb_filt_turb_viz_sm1<-sm(gamm_comb_filt_turb_viz,1)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\ndayn smooth")
gamm_comb_filt_turb_viz_sm2<-sm(gamm_comb_filt_turb_viz,2)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nZeta potential smooth")
gamm_comb_filt_turb_viz_sm3<-sm(gamm_comb_filt_turb_viz,3)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nCoagulation pH smooth")
gamm_comb_filt_turb_viz_sm4<-sm(gamm_comb_filt_turb_viz,4)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nFlow smooth")
gamm_comb_filt_turb_viz_sm5<-sm(gamm_comb_filt_turb_viz,5)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nTemp smooth")
gamm_comb_filt_turb_viz_sm6<-sm(gamm_comb_filt_turb_viz,6)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nRaw UVabs smooth")
gamm_comb_filt_turb_viz_sm7<-sm(gamm_comb_filt_turb_viz,7)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nRaw water turb")
gamm_comb_filt_turb_viz_sm8<-sm(gamm_comb_filt_turb_viz,8)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nReturn water flow smooth")
gamm_comb_filt_turb_viz_sm9<-sm(gamm_comb_filt_turb_viz,9)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nReturn water turbidity smooth")
gamm_comb_filt_turb_viz_sm10<-sm(gamm_comb_filt_turb_viz,10)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered turbidity\nSource 2 proportion smooth")
gamm_comb_filt_turb_viz_sm11<-sm(gamm_comb_filt_turb_viz,11)%>%plotSlice(.,fix = list("blend" = c(0.4,0.45,0.5,0.55)))+ l_fitRaster() + l_fitContour() + l_points()+ labs(title = "Filtered turbidity\nZeta pH interaction")


formula(gamm_comb_filt_al$gam)

gamm_comb_filt_al_viz_sm1<-sm(gamm_comb_filt_al_viz,1)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\ndayn smooth")
gamm_comb_filt_al_viz_sm2<-sm(gamm_comb_filt_al_viz,2)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nZeta potential smooth")
gamm_comb_filt_al_viz_sm3<-sm(gamm_comb_filt_al_viz,3)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nCoagulation pH smooth")
gamm_comb_filt_al_viz_sm4<-sm(gamm_comb_filt_al_viz,4)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nFlow smooth")
gamm_comb_filt_al_viz_sm5<-sm(gamm_comb_filt_al_viz,5)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nTemp smooth")
gamm_comb_filt_al_viz_sm6<-sm(gamm_comb_filt_al_viz,6)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nRaw UVabs smooth")
gamm_comb_filt_al_viz_sm7<-sm(gamm_comb_filt_al_viz,7)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nRaw water turb")
gamm_comb_filt_al_viz_sm8<-sm(gamm_comb_filt_al_viz,8)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nReturn water flow smooth")
gamm_comb_filt_al_viz_sm9<-sm(gamm_comb_filt_al_viz,9)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nReturn water turbidity smooth")
gamm_comb_filt_al_viz_sm10<-sm(gamm_comb_filt_al_viz,10)%>%plot()+l_points()+ l_fitLine() + l_ciLine()+ labs(title = "Filtered aluminium\nSource 2 proportion smooth")
gamm_comb_filt_al_viz_sm11<-sm(gamm_comb_filt_al_viz,11)%>%plotSlice(.,fix = list("blend" = c(0.4,0.45,0.5,0.55)))+ l_fitRaster() + l_fitContour() + l_points()+ labs(title = "Filtered aluminium\nZeta pH interaction")




gamm_docrem_smooth_plot<- gridPrint(grobs = list(gamm_docrem_viz_sm1,
                                                 gamm_docrem_viz_sm2,
                                                 gamm_docrem_viz_sm3,
                                                 gamm_docrem_viz_sm4,
                                                 gamm_docrem_viz_sm5,
                                                 gamm_docrem_viz_sm6,
                                                 gamm_docrem_viz_sm7,
                                                 gamm_docrem_viz_sm8,
                                                 gamm_docrem_viz_sm9,
                                                 gamm_docrem_viz_sm10,
                                                 gamm_docrem_viz_sm11),
                                    ncol = 4)

ggsave(filename = "gamm_docrem_smooth_plot.png", plot = gamm_docrem_smooth_plot, path = "PLOTS", width = 45, height = 30, units = "cm")


gamm_clr_turb_smooth_plot<-gridPrint(grobs = list(gamm_clr_turb_viz_sm1,
                                                  gamm_clr_turb_viz_sm2,
                                                  gamm_clr_turb_viz_sm3,
                                                  gamm_clr_turb_viz_sm4,
                                                  gamm_clr_turb_viz_sm5,
                                                  gamm_clr_turb_viz_sm6,
                                                  gamm_clr_turb_viz_sm7,
                                                  gamm_clr_turb_viz_sm8,
                                                  gamm_clr_turb_viz_sm9,
                                                  gamm_clr_turb_viz_sm10,
                                                  gamm_clr_turb_viz_sm11),
                                     ncol = 4)

ggsave(filename = "gamm_clr_turb_smooth_plot.png", plot = gamm_clr_turb_smooth_plot, path = "PLOTS", width = 45, height = 30, units = "cm")


gamm_comb_filt_turb_smooth_plot<-gridPrint(grobs = list(gamm_comb_filt_turb_viz_sm1,
                                                        gamm_comb_filt_turb_viz_sm2,
                                                        gamm_comb_filt_turb_viz_sm3,
                                                        gamm_comb_filt_turb_viz_sm4,
                                                        gamm_comb_filt_turb_viz_sm5,
                                                        gamm_comb_filt_turb_viz_sm6,
                                                        gamm_comb_filt_turb_viz_sm7,
                                                        gamm_comb_filt_turb_viz_sm8,
                                                        gamm_comb_filt_turb_viz_sm9,
                                                        gamm_comb_filt_turb_viz_sm10,
                                                        gamm_comb_filt_turb_viz_sm11),
                                           ncol = 4)
ggsave(filename = "gamm_comb_filt_turb_smooth_plot.png", plot = gamm_comb_filt_turb_smooth_plot, path = "PLOTS", width = 45, height = 30, units = "cm")


gamm_comb_filt_al_smooth_plot<-gridPrint(grobs = list(gamm_comb_filt_al_viz_sm1,
                                                      gamm_comb_filt_al_viz_sm2,
                                                      gamm_comb_filt_al_viz_sm3,
                                                      gamm_comb_filt_al_viz_sm4,
                                                      gamm_comb_filt_al_viz_sm5,
                                                      gamm_comb_filt_al_viz_sm6,
                                                      gamm_comb_filt_al_viz_sm7,
                                                      gamm_comb_filt_al_viz_sm8,
                                                      gamm_comb_filt_al_viz_sm9,
                                                      gamm_comb_filt_al_viz_sm10,
                                                      gamm_comb_filt_al_viz_sm11),
                                         ncol = 4)

ggsave(filename = "gamm_comb_filt_al_smooth_plot.png", plot = gamm_comb_filt_al_smooth_plot, path = "PLOTS", width = 45, height = 30, units = "cm")


max(sigs$obstime)


# simulation from PCA_GAMM models----



# simulation 1 ----


ggplot(sigs_imputed,
       aes(x = coagulated_water_ph, y = raw_water_ph))+
    geom_point()


sim1<-expand.grid(coagulated_water_ph = c(6.2,6.5),
                  zetap = seq(-6,3, 0.2),
                  temp = c(4,14),
                  raw_water_turb = c(0.8,1.2),
                  est_sup_ret = 15,
                  sprntnt_turb = 3.5,
                  raw_uv_abs = c(12,15),
                  flow = c(350,380),
                  hrs = 30,
                  raw_water_ph = 6.9)%>%
    ungroup()


sim1PC<- predict(sigs_imputed_preprocess,sim1)



sim1$`Clarified UV (abs/m)`<- predict(gamm_docrem$gam, newdata = sim1PC)
sim1$`Clarified Turb (NTU)`<- predict(gamm_clr_turb$gam, newdata = sim1PC)%>%exp()
sim1$`Filtered Turb (NTU)`<- predict(gamm_comb_filt_turb$gam, newdata = sim1PC)%>%exp()
sim1$`Filtered Al (mg/l)`<- predict(gamm_comb_filt_al$gam, newdata = sim1PC)%>%exp()


sim1$clr_uv_cnfint<- predict(gamm_docrem$gam, newdata = sim1PC,se.fit = T)$se.fit*2


sim1$clr_turb_cnfintL<- (predict(gamm_clr_turb$gam, newdata = sim1PC)-predict(gamm_clr_turb$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#
sim1$clr_turb_cnfintH<- (predict(gamm_clr_turb$gam, newdata = sim1PC)+predict(gamm_clr_turb$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#

sim1$flt_turb_cnfintL<- (predict(gamm_comb_filt_turb$gam, newdata = sim1PC)-predict(gamm_comb_filt_turb$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#
sim1$flt_turb_cnfintH<- (predict(gamm_comb_filt_turb$gam, newdata = sim1PC)+predict(gamm_comb_filt_turb$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#

sim1$flt_al_cnfintL<- (predict(gamm_comb_filt_al$gam, newdata = sim1PC)-predict(gamm_comb_filt_al$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#
sim1$flt_al_cnfintH<- (predict(gamm_comb_filt_al$gam, newdata = sim1PC)+predict(gamm_comb_filt_al$gam, newdata = sim1PC,se.fit = T)$se.fit*2)%>%exp()#



sim1plotdata<- sim1%>%
    mutate(raw_water_turb = paste("Raw turb", raw_water_turb," (NTU)"),
           raw_uv_abs = paste("Raw UV", raw_uv_abs, " (abs/m)"),
           temp = paste("Temp", str_pad(temp, width=2, pad="0"), " (°C)"),
           flow = paste("Flow", flow, " (l/s)"))


sim1plotdatalabel<-sim1plotdata%>%distinct(raw_water_turb,raw_uv_abs,flow,temp)%>%
    arrange(raw_water_turb,raw_uv_abs,flow,temp)%>%
    mutate(lab = LETTERS[1:16])



gg_sim_zeta1<-ggplot(sim1plotdata, aes(x = zetap, y = `Clarified UV (abs/m)`))+
    geom_ribbon(aes(ymin = `Clarified UV (abs/m)`-clr_uv_cnfint, ymax = `Clarified UV (abs/m)`+clr_uv_cnfint, col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                    #linetype = factor(coagulated_water_ph),
                    group = interaction(coagulated_water_ph,temp, hrs)), alpha = 0.3)+    
    geom_line(size = 1,aes( col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                          #linetype = factor(coagulated_water_ph),
                          group = interaction(coagulated_water_ph,temp, hrs)))+
    facet_grid(raw_water_turb+raw_uv_abs~flow+temp)+
    theme_minimal()+
    labs(#title = "Simulated response to zeta potential change from GAMM models",
         x = "Zeta potential (mV)")+
    scale_color_brewer( name = "Coag pH", palette = "Dark2", aesthetics = c("colour","fill"))+
    scale_linetype(name = "Coag pH")+
    coord_cartesian(ylim = c(6,8))+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    geom_text(data = sim1plotdatalabel,aes(x = -5.5, y = 8, label = lab), colour = "black")



gg_sim_zeta1

ggsave(filename = "simulation_zeta1.png", plot = gg_sim_zeta1, path = "PLOTS", width = 20, height = 20, units = "cm")



gg_sim_zeta2<-ggplot(sim1plotdata, aes(x = zetap, y = `Clarified Turb (NTU)`))+
    geom_ribbon(aes(ymin = clr_turb_cnfintL, ymax = clr_turb_cnfintH, col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                    #linetype = factor(coagulated_water_ph),
                    group = interaction(coagulated_water_ph,temp, hrs)), alpha = 0.3)+    
    geom_line(size = 1,aes( col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                            #linetype = factor(coagulated_water_ph),
                            group = interaction(coagulated_water_ph,temp, hrs)))+    facet_grid(raw_water_turb+raw_uv_abs~flow+temp)+
    theme_minimal()+
    labs(#title = "Simulated response to zeta potential change from GAMM models",
        x = "Zeta potential (mV)")+
    scale_color_brewer( name = "Coag pH", palette = "Dark2", aesthetics = c("colour","fill"))+
    scale_linetype(name = "Coag pH")+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    geom_text(data = sim1plotdatalabel,aes(x = -5.5, y = 1.2, label = lab), colour = "black")



#gg_sim_zeta2

ggsave(filename = "simulation_zeta2.png", plot = gg_sim_zeta2, path = "PLOTS", width = 20, height = 20, units = "cm")


gg_sim_zeta3<-ggplot(sim1plotdata, aes(x = zetap, y = `Filtered Turb (NTU)`))+
    geom_ribbon(aes(ymin = flt_turb_cnfintL, ymax = flt_turb_cnfintH, col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                    #linetype = factor(coagulated_water_ph),
                    group = interaction(coagulated_water_ph,temp, hrs)), alpha = 0.3)+    
    geom_line(size = 1,aes( col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                            #linetype = factor(coagulated_water_ph),
                            group = interaction(coagulated_water_ph,temp, hrs)))+    facet_grid(raw_water_turb+raw_uv_abs~flow+temp)+
    theme_minimal()+
    labs(#title = "Simulated response to zeta potential change from GAMM models",
        x = "Zeta potential (mV)")+
    scale_color_brewer( name = "Coag pH", palette = "Dark2", aesthetics = c("colour","fill"))+
    scale_linetype(name = "Coag pH")+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    geom_text(data = sim1plotdatalabel,aes(x = -5.5, y = 0.08, label = lab), colour = "black")



#gg_sim_zeta3

ggsave(filename = "simulation_zeta3.png", plot = gg_sim_zeta3, path = "PLOTS", width = 20, height = 20, units = "cm")



gg_sim_zeta4<-ggplot(sim1plotdata, aes(x = zetap, y = `Filtered Al (mg/l)`))+
    geom_ribbon(aes(ymin = flt_al_cnfintL, ymax = flt_al_cnfintH, col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                    #linetype = factor(coagulated_water_ph),
                    group = interaction(coagulated_water_ph,temp, hrs)), alpha = 0.3)+    
    geom_line(size = 1,aes( col = factor(coagulated_water_ph),fill = factor(coagulated_water_ph),
                            #linetype = factor(coagulated_water_ph),
                            group = interaction(coagulated_water_ph,temp, hrs)))+    facet_grid(raw_water_turb+raw_uv_abs~flow+temp)+
    theme_minimal()+
    labs(#title = "Simulated response to zeta potential change from GAMM models",
        x = "Zeta potential (mV)")+
    scale_color_brewer( name = "Coag pH", palette = "Dark2", aesthetics = c("colour","fill"))+
    scale_linetype(name = "Coag pH")+
    theme(legend.position = "bottom", legend.direction = "horizontal")+
    geom_text(data = sim1plotdatalabel,aes(x = -5.5, y = 0.015, label = lab), colour = "black")+
    coord_cartesian(ylim = c(0,0.015))



gg_sim_zeta4

ggsave(filename = "simulation_zeta4.png", plot = gg_sim_zeta4, path = "PLOTS", width = 20, height = 20, units = "cm")



# 
# ## simulation 2 ----
# 
# 
# sim2<-expand.grid(coagulated_water_ph = c(6.2,6.4,6.6,6.8),
#                   zetap = seq(-6,3, 0.2),
#                   temp = c(4,6,10,14),
#                   raw_water_turb = 1.2,
#                   est_sup_ret = 26,
#                   sprntnt_turb = 4,
#                   raw_uv_abs = c(12,15),
#                   flow = c(300,350,400),
#                   hrs = 24,
#                   raw_water_ph = 7)
# 
# 
# sim2PC<- predict(sigs_imputed_preprocess,sim2)
# 
# 
# 
# sim2$`Clarified UV (abs/m)`<- predict(gamm_docrem$gam, newdata = sim2PC)
# sim2$`Clarified Turb (NTU)`<- predict(gamm_clr_turb$gam, newdata = sim2PC)%>%exp()
# sim2$`Filtered Turb (NTU)`<- predict(gamm_comb_filt_turb$gam, newdata = sim2PC)%>%exp()
# sim2$`Filtered Al (mg/l)`<- predict(gamm_comb_filt_al$gam, newdata = sim2PC)%>%exp()
# 
# 
# 
# gg_sim_zeta2<-ggplot(sim2, aes(x = zetap, y = `Clarified UV (abs/m)`, colour = temp, linetype = factor(raw_uv_abs),
#                                group = interaction(temp,raw_uv_abs)))+
#     geom_line()+
#     #facet_grid(outcome~flowfactor, scales = "free_y")+
#     facet_grid(flow~coagulated_water_ph, scales = "free")+
#     theme_minimal()+
#     labs(title = "Simulated response to zeta potential change from GAMM models",
#          x = "Coag pH",
#          y = "Treated water quality")+
#     scale_color_distiller( name = "Temp", palette = "RdBu")+#raw_uv_abs, direction = -1
#     scale_linetype(name = "Raw UV")#+
#     #geom_smooth()
# 
# 
# 
# gg_sim_zeta2
# 
# ggsave(filename = "simulation_zeta2.png", plot = gg_sim_zeta2, path = "PLOTS", width = 20, height = 15, units = "cm")
# 
# 
# ## simulation 3 ----
# 
