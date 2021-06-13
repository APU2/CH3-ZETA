# Script to run gam models

# load data ----

load("DATA/ZetaPaperData.Rdata") # loads the workspace



sigs_imputed<-sigs_imputed%>%
    dplyr::filter(obstime<"2018-04-05 00:00:00")


sigs_imputed_preprocess<-sigs_imputed%>%
    dplyr::filter(datasplit =="train")%>%
    mutate(dayn = as.numeric(obstime))%>%
    dplyr::select(#rel_delta_uv,clr_turb,comb_filt_turb,comb_filt_al,
                  zetap, temp, raw_uv_abs, flow,
                  raw_water_ph, coagulated_water_ph,
                  raw_water_turb,hrs, raw_water_turb)%>%
    preProcess(method = c("range" ), rangeBounds = c(1,2))


sigs_imputed_pre_processed<-sigs_imputed%>%
    dplyr::select(#rel_delta_uv,clr_turb,comb_filt_turb,comb_filt_al,
        zetap, temp, raw_uv_abs, flow,
        raw_water_ph, coagulated_water_ph,
        raw_water_turb,hrs, raw_water_turb)%>%
        predict(sigs_imputed_preprocess, .)%>%
    bind_cols(. ,sigs_imputed%>%
                  dplyr::select(obstime,datasplit, 
                    rel_delta_uv,clr_turb,comb_filt_turb,comb_filt_al))




# prep for gam models ----



## set gamma

gamGamma <- log(nrow(sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train")))/2


# define formula for models####

form_docrem<-as.formula("rel_delta_uv~
                        s(zetap, k = 12)+s(temp, k = 12)+s(raw_uv_abs, k = 12)+
                        s(flow, k = 3)+s(raw_water_ph, k = 12)+
                        s(coagulated_water_ph, k = 12)+s(raw_water_turb, k = 12)+
                        ti(zetap, temp, raw_uv_abs,flow, k = 4)+
                        ti(raw_water_ph,coagulated_water_ph, k = 4)
                        ") 

form_clr_turb<-as.formula("#log(clr_turb)~
                        clr_turb~
                        s(zetap, k = 12)+s(temp, k = 12)+s(raw_uv_abs, k = 12)+
                          s(flow, k = 3)+s(raw_water_ph, k = 12)+
                          s(coagulated_water_ph, k = 12)+s(raw_water_turb, k = 12)+
                        ti(zetap, temp, raw_uv_abs,flow, k = 4)+
                        ti(raw_water_ph,coagulated_water_ph, k = 4)") 

form_filt_turb<-as.formula("#log(comb_filt_turb)~
                          comb_filt_turb~
                        s(zetap, k = 12)+s(temp, k = 12)+s(raw_uv_abs, k = 12)+
                        s(flow, k = 3)+s(raw_water_ph, k = 12)+
                        s(coagulated_water_ph, k = 12)+s(raw_water_turb, k = 12)+
                        s(hrs, k = 12)+
                        ti(zetap, temp, flow,hrs, k = 4)+
                           ti(raw_water_ph,coagulated_water_ph, k = 4)")


form_filt_al<-as.formula("#log(comb_filt_al)~
                          comb_filt_al~
                         s(zetap, k = 12)+s(temp, k = 12)+s(raw_uv_abs, k = 12)+
                         s(flow, k = 3)+s(raw_water_ph, k = 12)+
                         s(coagulated_water_ph, k = 12)+s(raw_water_turb, k = 12)+
                         s(hrs, k = 12)+
                        ti(zetap, temp,flow, hrs, k = 4)+
                         ti(raw_water_ph,coagulated_water_ph, k = 4)")# coag chemistry effect



# run explanatory gam models ----

# doc removal with zeta


expl_gamdocrem<- gam(form_docrem
                         ,data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                     select = TRUE,
                     verbose = TRUE,
                     gamma = gamGamma#,
                     #family = "quasibinomial"
                     )

summary(expl_gamdocrem)

plot(expl_gamdocrem, pages = 1, residuals = T, scheme = 2, rug = T, shift = T)
gam.check(expl_gamdocrem)
plot(expl_gamdocrem, select = 1, residuals = T, scheme = 2, rug = T, scale =0, shift = T)

plot(sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train")%>%.$coagulated_water_ph,residuals(expl_gamdocrem))

# Clarified turbidity model
expl_gamclr_turb<- gam(formula = form_clr_turb
                       ,
                       data =sigs_imputed_pre_processed%>%dplyr::filter(datasplit =="train"),
                       select = TRUE,
                       verbose = TRUE,
                       gamma = gamGamma)

## combined filtered turbidity

plot(sigs_imputed_pre_processed$comb_filt_al)
#plot(BoxCox(sigs_imputed_pre_processed$comb_filt_al, lambda = BoxCox.lambda(sigs_imputed_pre_processed$comb_filt_al)))

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

plot(expl_gamcomb_filt_al, residuals = T, pages = 1)


# pull models together



expl_gamdf<- data.frame(model_names = c("expl_gamdocrem",
                                        "expl_gamclr_turb",
                                        "expl_gamcomb_filt_turb",
                                        "expl_gamcomb_filt_al"#,
),
outcome = c("rel_delta_uv",
            "clr_turb",
            "comb_filt_turb",
            "comb_filt_al"#,
),
zeta_used = c(rep("with zeta",4)),
log = c("n","n","n","n")
)%>%
    mutate(observed = list(sigs_imputed_pre_processed%>%.$rel_delta_uv,
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




RPushbullet::pbPost("note", "Script complete", "gam models now fit")


save.image(file = "DATA/EXPLANATORY_GAM_MODELS.Rdata")

rm(list=ls())

