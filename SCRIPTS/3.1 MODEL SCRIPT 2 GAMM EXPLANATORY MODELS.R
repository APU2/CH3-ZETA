# Script to expand explanatory gams to gamms

#load the workspace
load("DATA/EXPLANATORY_GAM_MODELS.Rdata")




gamm_docrem<- gamm(formula = form_docrem
                   ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                   correlation = corAR1(form=~1)
                   ,control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
                   #,family = "quasibinomial"
                   #,family = betar(theta = 0.5,link = "logit", eps = 0.001)
)



gamm_clr_turb<- gamm(formula = form_clr_turb
                     ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                     correlation = corAR1(form=~1)
                     ,control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
)




gamm_comb_filt_turb<- gamm(formula = form_filt_turb
                           ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                           correlation = corAR1(form=~1)
                           ,control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
)



gamm_comb_filt_al<- gamm(formula =  form_filt_al
                         ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                         correlation = corAR1(form=~1)
                         ,control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
)






# create dataframe of gamm models with correlation

gamm_cor_df<- data.frame(model_names = c("gamm_docrem",
                                              "gamm_clr_turb",
                                              "gamm_comb_filt_turb",
                                              "gamm_comb_filt_al"#,
),
outcome = c("clr_uv_abs",
            "clr_turb",
            "comb_filt_turb",
            "comb_filt_al"#,
),
zeta_used = c(rep("with zeta",4)),
log = c(rep("n",1), rep("y", 3))
)%>%
    mutate(observed = list(sigs_imputed_pre_processed%>%.$clr_uv_abs,
                           sigs_imputed_pre_processed%>%.$clr_turb,
                           sigs_imputed_pre_processed%>%.$comb_filt_turb,
                           sigs_imputed_pre_processed%>%.$comb_filt_al#,
    ),
    models=list(gamm_docrem$gam,
                gamm_clr_turb$gam,
                gamm_comb_filt_turb$gam,
                gamm_comb_filt_al$gam#,
    ),
    obstime = list(sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime,
                   sigs_imputed_pre_processed%>%.$obstime#,
    ))%>%
    mutate(predicted= map(models, .f = predict, type = "response", 
                          newdata = sigs_imputed_pre_processed))



additive_models_df<- bind_rows(expl_gamdf,gamm_cor_df )


#stargazer(gamm_cor_df$cor_models, type = "html", out = "TABLES/gamm_arma_downstream.doc")                        

prediction_results<- additive_models_df%>%
    dplyr::select(model_names,
                  outcome, 
                  #datasplit,
                  zeta_used,
                  obstime,
                  predicted,
                  observed,
                  log)%>%
    unnest()%>%
    mutate(cor = ifelse(grepl("gamm", model_names)==T, "COR", "NOCOR"))%>%
    group_by(model_names,cor)%>%
    #left_join(., outcome_reprocess_lookup)%>%
    #mutate(predicted = predicted*std*mean)%>%
    mutate(predicted = ifelse(log == "y", exp(predicted), predicted),
           error = predicted - observed)%>%
    #dplyr::filter(is.na(observed)==F)%>%
    mutate(datasplit = "train",
           datasplit = ifelse(obstime>"2017-10-20 00:00:00", "test1", datasplit),
           datasplit = ifelse(obstime>"2018-01-24 00:00:00", "test2", datasplit),
           datasplit = ifelse(obstime>"2018-02-28 00:00:00", "test3", datasplit),
           datasplit = ifelse(obstime>"2018-03-27 00:00:00", "test4", datasplit))




prediction_results_summary<- prediction_results%>%
    group_by(model_names,outcome)%>%
    summarise(mean_pred = mean(predicted),
              mean_obs = mean(observed))



prediction_summary<- prediction_results%>%
    group_by(model_names,outcome,zeta_used, cor,datasplit)%>%
    summarise(#count_pred = length(predicted),
        #count_obs = length(observed),
        `RMSE` = round(postResample(predicted, observed)[1],2),
        `RSQ`= round(postResample(predicted, observed)[2],2),
        `MAE` = round(postResample(predicted, observed)[3],2),
        `MAPE` = round(median((abs(predicted-observed))/observed)*100,2))





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

png(filename = "PLOTS/PCA GAMM FILT TURB MDL.png",  width = 600, height = 500)
plot(gamm_comb_filt_turb$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_comb_filt_turb$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM FILT TURB DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(gamm_comb_filt_turb$gam)
dev.off()


#filt Al
summary(gamm_comb_filt_al$gam)

png(filename = "PLOTS/PCA GAMM FILT AL MDL.png",  width = 600, height = 500)
plot(gamm_comb_filt_al$gam, pages = 1, residuals = T, scheme = 2, rug = T, scale =-1, shift = coef(gamm_comb_filt_al$gam)[1], seWithMean = TRUE)
dev.off()

png(filename = "PLOTS/PCA GAMM FILT AL DIAG.png",  width = 500, height = 500)
par(mfrow = c(2,2))
gam.check(gamm_comb_filt_al$gam)
dev.off()



save.image(file = "DATA/EXPLANATORY_GAMM_MODELS.Rdata")
RPushbullet::pbPost("note", "Script complete", "gamm models now fit")

rm(list=ls())
