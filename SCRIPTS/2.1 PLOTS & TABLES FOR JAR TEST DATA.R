



# Jar testing plots ----
source("SCRIPTS/1.2 PREP JAR TEST DATA.R", echo = FALSE)





# plot dose setting jars####

dose_setting_jars<-ggplot(initial_jars_zeta%>%
                              dplyr::filter(Source == "Source A"), aes(x = Dose, y = ZP..mV.,colour = Source))+
    geom_point( )+
    #facet_wrap(~Source)+
    geom_smooth(method = "lm", formula = y~poly(x,3))+
    theme_minimal()+
    scale_color_brewer(palette = "Dark2")+
    geom_hline(yintercept = 0)+
    scale_x_continuous(limits = c(0.7,2), breaks = seq(0.8,2,.2))+
    labs(x = "Dose (mg/l) as al",
         y = "Zeta potential (mV)")

ggsave(dose_setting_jars, filename = "Dose zeta curves for inital jar tests.png",path = "PLOTS",
       width = 20, height = 15, units = "cm")





# Plot jar zeta by mixing ####

masterjars%>%
    dplyr::filter(coagok == 1)%>%
    ggplot(.,aes(x = rapidmix, y = zeta))+
    geom_boxplot()+
    facet_wrap(~temp_label, ncol = 1)+
    coord_flip()+
    geom_point()


masterjars%>%
    dplyr::filter(coagok == 1)%>%
    ggplot(.,aes(x = rapidmix, y = pH))+
    geom_boxplot()+
    facet_wrap(~temp_label, ncol = 1)+
    coord_flip()+
    geom_point()


ggplot(masterjars,
       aes(x = zeta, y = pH))+
    geom_point()+
    geom_label(aes(label = masterjar))


zeta_mix<-lm(zeta~ temp_code, data = masterjars%>%
       dplyr::filter(coagok == 1), na.action = "na.omit")


summary(zeta_mix)

pH_mix<-lm(pH~ temp_code+rapidmix, data = masterjars%>%
                 dplyr::filter(coagok == 1), na.action = "na.omit")

summary(pH_mix)


# plot & model jar zeta


label_zeta<- masterjars_stddata%>%
    #dplyr::filter(jartime < 15 )%>%
    dplyr::filter(is.na(Dx..50.)==F,
                jartime == max(jartime))%>%
    dplyr::filter(  coagok ==1, flocok == 1)




jarsplot<- ggplot(masterjars_stddata%>%filter(jartime >0, flocok == 1), 
                  aes(x = jartime, y = Dx..50., colour = Source,
                      group= interaction(temp_code,rpm,source,rapidmix, masterjar)))+
    #geom_ribbon(aes(ymin =Dx..10., ymax = Dx..90. ))+
    geom_line( size = 1.5, alpha = 0.7)+
    geom_point()+
    facet_grid(temp_label ~ mix_label)+
    scale_color_brewer(name = "Zeta", palette = "Dark2")+
    labs(title = "Floc formation in jar tests of source waters",
         x = "Time from dose (min)",
         y = expression("D50" (µ~m)))+
    theme_minimal()+
    theme(legend.position="bottom",legend.direction="horizontal")


jarsplot

ggsave(plot = jarsplot, path = "PLOTS", filename = "Floc growth curves .png", width = 13, height = 13, units = "cm")



# 
median_growth_aggregate<- masterjars_stddata%>%
    filter(jartime >=0, jartime <=20, flocok == 1)%>%#, coagok ==1
    mutate(jarmin = round(jartime/2)*2)%>%
    group_by(temp_code,temp_label,rapidmix,rpm,jarmin, mix_label, Source)%>%
    summarise(D50 = median(Dx..50., na.rm = T),
              low = min(Dx..50., na.rm = T),
              hi = max(Dx..50., na.rm = T),
              cnt = length(unique(masterjar)))%>%
    dplyr::filter(jarmin >= 0,
                  cnt >2)



median_growth_jar<- masterjars_stddata%>%
    filter(jartime >=0, jartime <=20, flocok == 1)%>%#, coagok ==1
    mutate(jarmin = round(jartime/2)*2)%>%
    group_by(temp_code,temp_label,rapidmix,rpm,jarmin, masterjar, mix_label, Source)%>%
    summarise(D50 = median(Dx..50., na.rm = T))%>%
    dplyr::filter(jarmin >= 0)


growth_curves<-ggplot(median_growth_aggregate, 
       aes(x = jarmin, y = D50 ,
           fill = mix_label, colour = mix_label,  group = mix_label))+
    facet_wrap(Source~temp_label)+
    theme_minimal()+
    geom_ribbon(aes(ymin = low, ymax = hi), alpha = 0.3)+
    geom_line(size = 2)+
    labs(x = "Time from dosing (min)", y = "D50 floc size (µm)")+
    scale_color_brewer(name = "Mixing", palette = "Dark2", aesthetics = c("colour","fill"))+
    theme(legend.position="bottom",legend.direction="vertical")
    

growth_curves

ggsave(plot = growth_curves, path = "PLOTS", 
       filename = "Combined floc growth curves.png", 
       width = 14, height = 18, units = "cm")


unique(masterjars_stddata$Source)

## data for floc size modelling

floc_model_measurements<- masterjars_stddata%>%
    filter( flocok == 1)%>%
    mutate(jarmin = round(jartime))%>%
    dplyr::filter(jarmin %in% c(5,10,15,20))%>%
    group_by(masterjar, jarmin, Source)%>%
    summarise(D50 = median(Dx..50., na.rm=T))%>%
    ungroup()


floc_model_data<- crossing(masterjar = unique(floc_model_measurements$masterjar),
                             jarmin = c(5,10,15,20))%>%
                    mutate(jarmin = as.numeric(jarmin))%>%
    left_join(.,floc_model_measurements)%>%
    left_join(.,masterjars%>%dplyr::select(masterjar,Source, temp_label, rapidmix, rpm_label))%>%
    group_by(masterjar)%>%
    mutate(D50 = na.locf(D50),
           Source = na.locf(Source),
           temp_label = na.locf(temp_label),
           rapidmix = na.locf(rapidmix),
           rpm_label = na.locf(rpm_label))%>%
    ungroup()



 floc_growth_05minA<-lm(D50~temp_label+rapidmix+rpm_label+
                            #temp_label:rpm_label+
                            rapidmix:rpm_label, 
                      data = floc_model_data%>%dplyr::filter(jarmin==5, Source =="Source A"))
 floc_growth_10minA<-lm(D50~temp_label+rapidmix+rpm_label+
                            temp_label:rpm_label#+rapidmix:rpm_label
                        ,
                      data = floc_model_data%>%dplyr::filter(jarmin==10, Source =="Source A"))
 floc_growth_15minA<-lm(D50~temp_label+rapidmix+rpm_label+
                            temp_label:rpm_label#+rapidmix:rpm_label
                        , 
                       data = floc_model_data%>%dplyr::filter(jarmin==15, Source =="Source A"))
 floc_growth_20minA<-lm(D50~temp_label+rapidmix+rpm_label+
                            temp_label:rpm_label#+rapidmix:rpm_label
                        ,
                       data = floc_model_data%>%dplyr::filter(jarmin==20, Source =="Source A"))


 floc_growth_05minB<-lm(D50~temp_label+rapidmix+rpm_label, 
                        data = floc_model_data%>%dplyr::filter(jarmin==5, Source =="Source B"))
 floc_growth_10minB<-lm(D50~temp_label+rapidmix+rpm_label,
                        data = floc_model_data%>%dplyr::filter(jarmin==10, Source =="Source B"))
 floc_growth_15minB<-lm(D50~temp_label+rapidmix+rpm_label, 
                        data = floc_model_data%>%dplyr::filter(jarmin==15, Source =="Source B"))
 floc_growth_20minB<-lm(D50~temp_label+rapidmix+rpm_label,
                        data = floc_model_data%>%dplyr::filter(jarmin==20, Source =="Source B"))
 
 
 par(mfrow=c(2,2))
 plot(floc_growth_05minA)
 summary(floc_growth_05minA)
 
 
 
stargazer(floc_growth_05minA, floc_growth_10minA, floc_growth_15minA, floc_growth_20minA ,type = "html",
          title = "Summary of models explaining jar test data for source A", 
          out = "TABLES/D50 5MIN intervals A.doc",
          label = "lm_jars", font.size= "tiny", column.sep.width = "1pt",single.row = TRUE ,
          column.labels = c("5 minutes","10 minutes","15 minutes","20 minutes"),
          model.numbers = T)


stargazer(floc_growth_05minB, floc_growth_10minB, floc_growth_15minB, #floc_growth_20minB ,
          type = "html",
          title = "Summary of models explaining jar test data source B", 
          out = "TABLES/D50 5MIN intervals B.doc",
          label = "lm_jars", font.size= "tiny", column.sep.width = "1pt",single.row = TRUE ,
          column.labels = c("5 minutes","10 minutes","15 minutes","20 minutes"),
          model.numbers = T)

19.8-11.1


# analyse results

# select data over range of potential flocculation times

library(lme4)

gam_jars<-gam(D50~ s(jarmin, by = interaction(temp_code, rapidmix, rpm), k = 7),
               data = median_growth_jar#,
               #correlation = corAR1(form=~1),
               #random = list(masterjar = ~1)
               ) 

par(mfrow= c(2,2))
gam.check(gam_jars$gam)
plot(gam_jars$gam)

summary(gam_jars)


median_growth_jar$PRED<-predict(gam_jars, newdata = median_growth_jar)


ggplot(median_growth_jar, aes(x = jarmin ,linetype = rapidmix,fill = rpm, colour = rpm,  group = interaction(temp_code,temp_label,rapidmix,rpm)))+
    facet_wrap(~temp_label)+
    theme_minimal()+
    geom_line(size = 2, aes(y = PRED))+
    geom_line(size = 2, aes(y = D50), alpha = 0.5)+
    labs(x = "Time from dosing (min)", y = "D50 floc size (µm)")+
    scale_color_distiller(name = "Floculant mixing speed (rpm)", palette = "RdBu")+
    scale_linetype(name = "Rapid mix")





lm_jarsmin<- lm(D50~rapidmix+temp_code*rpm+jartime,
                       data = jarsmin%>%dplyr::filter(source == "Megget"))

summary(lm_jarsmin_megget)



stargazer(lm_jarsmin_bothsources,lm_jarsmin_gladhouse,lm_jarsmin_megget, type = "latex",
          title = "Summary of models explaining jar test data", out = "C:\\Users\\Andy\\OneDrive\\ENGD PROJECT\\ONLINE_ZETA\\TABLES/lm_jar_tab.tex",
          label = "lm_jars", font.size= "tiny", column.sep.width = "1pt",single.row = TRUE ,
          column.labels = c("Both sources","Gladhouse","Megget" ))



fileConn<-file("tex/4results_jars_models.tex")
writeLines(jar_models_description, fileConn)
close(fileConn)

