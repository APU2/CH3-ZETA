# Script to run gam models

# load data ----

load("DATA/ZetaPaperData.Rdata") # loads the workspace



sigs_imputed<-sigs_imputed%>%
    dplyr::filter(obstime<"2018-04-05 00:00:00")%>%
    mutate(flow = ifelse(flow>450,450,flow))



## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

png(filename = "PLOTS/PAIRS PLOTS OF PREDICTOR VARIABLES.png",  width = 1020, height = 800)
pairs(sigs_imputed%>%
          dplyr::filter(datasplit =="train")%>%
          mutate(dayn = as.numeric(obstime))%>%
          dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
              zetap, temp, raw_uv_abs, flow,
              raw_water_ph, coagulated_water_ph,
              raw_water_turb,hrs, raw_water_turb),
      upper.panel = panel.cor, diag.panel = panel.hist)
dev.off()

sigs_imputed_preprocess<-sigs_imputed%>%
    dplyr::filter(datasplit =="train")%>%
    mutate(dayn = as.numeric(obstime))%>%
    dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
                  zetap, temp, raw_uv_abs, flow,
                  raw_water_ph, coagulated_water_ph,
                  raw_water_turb,hrs, raw_water_turb, 
                  est_sup_ret, sprntnt_turb)%>%
    preProcess(method = c("YeoJohnson", "scale", "pca" ), rangeBounds = c(1,2), thresh = 0.9)



sigs_imputed_pre_processed<-sigs_imputed%>%
    dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
        zetap, temp, raw_uv_abs, flow,
        raw_water_ph, coagulated_water_ph,
        raw_water_turb,hrs, raw_water_turb, 
        est_sup_ret, sprntnt_turb)%>%
        predict(sigs_imputed_preprocess, .)%>%
    bind_cols(. ,sigs_imputed%>%
                  dplyr::select(obstime,datasplit, 
                    clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al))



# pairs(sigs_imputed_pre_processed%>%
#           dplyr::filter(datasplit =="train")%>%
#           mutate(dayn = as.numeric(obstime))%>%
#           dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
#               PC1, PC2, PC3, PC4),
#       upper.panel = panel.cor, diag.panel = panel.hist)
# 
# 



investigate_pcomp<-sigs_imputed%>%
    dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
        zetap, temp, raw_uv_abs, flow,
        raw_water_ph, coagulated_water_ph,
        raw_water_turb,hrs, raw_water_turb, 
        est_sup_ret, sprntnt_turb)%>%
    predict(sigs_imputed_preprocess, .)%>%
    bind_cols(. ,sigs_imputed%>%
                  dplyr::select(datasplit,
                                zetap, temp, raw_uv_abs, flow,
                                raw_water_ph, coagulated_water_ph,
                                raw_water_turb,hrs, raw_water_turb, 
                                est_sup_ret, sprntnt_turb))


png(filename = "PLOTS/PAIRS PLOTS OF PREDICTOR VARIABLES & PCA COMPONENTS.png",  width = 1500, height = 1200)
pairs(investigate_pcomp%>%
          dplyr::filter(datasplit =="train")%>%
          dplyr::select(-datasplit),
      upper.panel = panel.cor, diag.panel = panel.hist)
dev.off()




investigate_pcomp2<-sigs_imputed%>%
    dplyr::select(#clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al,
        zetap, temp, raw_uv_abs, flow,
        raw_water_ph, coagulated_water_ph,
        raw_water_turb,hrs, raw_water_turb, 
        est_sup_ret, sprntnt_turb)%>%
    predict(sigs_imputed_preprocess, .)%>%
    bind_cols(. ,sigs_imputed%>%
                  dplyr::select(datasplit,
                                clr_uv_abs,clr_turb,comb_filt_turb,comb_filt_al))


png(filename = "PLOTS/PAIRS PLOTS OF OUTCOME VARIABLES & PCA COMPONENTS.png",  width = 1500, height = 1200)
pairs(investigate_pcomp2%>%
          dplyr::filter(datasplit =="train")%>%
          dplyr::select(-datasplit),
      upper.panel = panel.cor, diag.panel = panel.hist)
dev.off()
# prep for gam models ----



## set gamma

gamGamma <- log(nrow(sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train")))/2


# define formula for models####

form_docrem<-as.formula("clr_uv_abs~s(PC1, k = 7)+s(PC2, k = 7)+s(PC3, k = 7)+s(PC4, k = 7)+s(PC5, k = 7)+s(PC6, k = 7)+
                        ti(PC1,PC2, k = 7)+ti(PC1,PC3, k = 7)+ti(PC1,PC4, k = 7)+ti(PC1,PC5, k = 7)+ti(PC1,PC6, k = 7)+
                        ti(PC2,PC3, k = 7)+ti(PC2,PC4, k = 7)+ti(PC2,PC5, k = 7)+ti(PC2,PC6, k = 7)+
                        ti(PC3,PC4, k = 7)+ti(PC3,PC5, k = 7)+ti(PC3,PC6, k = 7)+
                        ti(PC4,PC5, k = 7)+ti(PC4,PC6, k = 7)+
                        ti(PC5,PC6, k = 7)") 

form_clr_turb<-as.formula("log(clr_turb)~s(PC1, k = 7)+s(PC2, k = 7)+s(PC3, k = 7)+s(PC4, k = 7)+s(PC5, k = 7)+s(PC6, k = 7)+
                        ti(PC1,PC2, k = 7)+ti(PC1,PC3, k = 7)+ti(PC1,PC4, k = 7)+ti(PC1,PC5, k = 7)+ti(PC1,PC6, k = 7)+
                        ti(PC2,PC3, k = 7)+ti(PC2,PC4, k = 7)+ti(PC2,PC5, k = 7)+ti(PC2,PC6, k = 7)+
                        ti(PC3,PC4, k = 7)+ti(PC3,PC5, k = 7)+ti(PC3,PC6, k = 7)+
                        ti(PC4,PC5, k = 7)+ti(PC4,PC6, k = 7)+
                        ti(PC5,PC6, k = 7)")

form_filt_turb<-as.formula("log(comb_filt_turb)~s(PC1, k = 7)+s(PC2, k = 7)+s(PC3, k = 7)+s(PC4, k = 7)+s(PC5, k = 7)+s(PC6, k = 7)+
                        ti(PC1,PC2, k = 7)+ti(PC1,PC3, k = 7)+ti(PC1,PC4, k = 7)+ti(PC1,PC5, k = 7)+ti(PC1,PC6, k = 7)+
                           ti(PC2,PC3, k = 7)+ti(PC2,PC4, k = 7)+ti(PC2,PC5, k = 7)+ti(PC2,PC6, k = 7)+
                           ti(PC3,PC4, k = 7)+ti(PC3,PC5, k = 7)+ti(PC3,PC6, k = 7)+
                           ti(PC4,PC5, k = 7)+ti(PC4,PC6, k = 7)+
                           ti(PC5,PC6, k = 7)")

form_filt_al<-as.formula("log(comb_filt_al)~s(PC1, k = 7)+s(PC2, k = 7)+s(PC3, k = 7)+s(PC4, k = 7)+s(PC5, k = 7)+s(PC6, k = 7)+
                        ti(PC1,PC2, k = 7)+ti(PC1,PC3, k = 7)+ti(PC1,PC4, k = 7)+ti(PC1,PC5, k = 7)+ti(PC1,PC6, k = 7)+
                         ti(PC2,PC3, k = 7)+ti(PC2,PC4, k = 7)+ti(PC2,PC5, k = 7)+ti(PC2,PC6, k = 7)+
                         ti(PC3,PC4, k = 7)+ti(PC3,PC5, k = 7)+ti(PC3,PC6, k = 7)+
                         ti(PC4,PC5, k = 7)+ti(PC4,PC6, k = 7)+
                         ti(PC5,PC6, k = 7)")

# for PCA 80% thresh this formula gives acceptable predictions "~s(PC1,PC2,PC3)+s(PC4)+s(PC5)+ti(PC1,PC5)+ti(PC2,PC5)+ti(PC3,PC5)"
# so does this one but residuals are patterned ~s(PC1)+s(PC2)+s(PC3)+s(PC4)+s(PC5)+s(PC6)+ti(PC1,PC2,PC3,PC4)+ti(PC1,PC5)+ti(PC2,PC5)+ti(PC3,PC5)+ti(PC1,PC6)+ti(PC2,PC6)+ti(PC3,PC6)"
# run explanatory gam models ----

# doc removal with zeta


expl_gamdocrem<- gam(form_docrem
                         ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                     select = TRUE,
                     verbose = TRUE,
                     gamma = gamGamma
                     #family = "quasibinomial"
                     )

# Clarified turbidity model
expl_gamclr_turb<- gam(formula = form_clr_turb
                       ,
                       data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                       select = TRUE,
                       verbose = TRUE,
                       gamma = gamGamma)

## combined filtered turbidity


expl_gamcomb_filt_turb<-gam(formula = form_filt_turb
                            ,
                            data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                            select = TRUE,
                            verbose = TRUE,
                            gamma = gamGamma)


# combined filtered al 
expl_gamcomb_filt_al<- gam(formula = form_filt_al
                           ,
                           data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                           select = TRUE,
                           verbose = TRUE,
                           gamma = gamGamma)



# pull models together



expl_gamdf<- data.frame(model_names = c("expl_gamdocrem",
                                        "expl_gamclr_turb",
                                        "expl_gamcomb_filt_turb",
                                        "expl_gamcomb_filt_al"#,
),
outcome = c("clr_uv_abs",
            "clr_turb",
            "comb_filt_turb",
            "comb_filt_al"#,
),
zeta_used = c(rep("with zeta",4)),
log = c("n","y","y","y")
)%>%
    mutate(observed = list(sigs_imputed_pre_processed%>%.$clr_uv_abs,
                           sigs_imputed_pre_processed%>%.$clr_turb,
                           sigs_imputed_pre_processed%>%.$comb_filt_turb,
                           sigs_imputed_pre_processed%>%.$comb_filt_al#,
    ),
    
    models=list(expl_gamdocrem,
                expl_gamclr_turb,
                expl_gamcomb_filt_turb,
                expl_gamcomb_filt_al#,
    ),
    obstime = list(sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime#,
    )
    )%>%
    mutate(predicted=  map(models, .f = predict,type = "response", 
                           newdata = sigs_imputed_pre_processed))




expl_prediction_results<- expl_gamdf%>%
    dplyr::select(model_names,
                  outcome,
                  #datasplit,
                  zeta_used,
                  obstime,
                  predicted,
                  observed,
                  log)%>%
    unnest()%>%
    #mutate(cor = ifelse(grepl("gamm", model_names)==T, "COR", "NOCOR"))%>%
    group_by(model_names)%>%
    #left_join(., outcome_reprocess_lookup)%>%
    #mutate(predicted = predicted*std*mean)%>%
    mutate(predicted = ifelse(log == "y", exp(predicted), predicted),
           error = predicted - observed)%>%
    mutate(datasplit = "train",
           datasplit = ifelse(obstime>"2017-10-20 00:00:00", "test", datasplit),
           datasplit = ifelse(obstime>"2018-02-28 00:00:00", "test2", datasplit))


expl_prediction_results_summary<- expl_prediction_results%>%
    group_by(model_names,outcome,datasplit)%>%
    summarise(mean_pred = mean(predicted),
              mean_obs = mean(observed))



expl_prediction_summary<- expl_prediction_results%>%
    group_by(model_names,outcome,zeta_used,datasplit)%>%
    summarise(#count_pred = length(predicted),
        #count_obs = length(observed),
        `RMSE` = round(postResample(predicted, observed)[1],2),
        `RSQ`= round(postResample(predicted, observed)[2],2),
        `MAE` = round(postResample(predicted, observed)[3],2),
        `MAPE` = round(median((abs(predicted-observed))/observed)*100,2))




# ggplot(expl_prediction_results#%>%dplyr::filter(datasplit =="test")
#        ,aes(x = obstime))+
#     geom_point(aes(y = predicted))+
#     geom_point(aes(y = observed, colour = "obs"))+
#     facet_grid(outcome~zeta_used,
#                scales = "free_y")+
#     geom_vline(xintercept = ymd_hms("2017-10-20 00:00:00"))

# gamm _model viz----
library(mgcViz)





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


RPushbullet::pbPost("note", "Script complete", "gam models now fit")


save.image(file = "DATA/EXPLANATORY_GAM_MODELS.Rdata")

rm(list=ls())

