
#load data

# connect to database----
rosdb<- src_postgres(dbname = "rosebery", host = "localhost", user = "postgres", password = "password")




# ts_example_data----

ts_eg_scadasigs<- tbl(rosdb, "sigs")%>%
          dplyr::filter(obstime > "2017-12-19 00:00:00",
                        obstime < "2017-12-23 00:00:00")%>%
          dplyr::select(obstime,
                        megget_inlet_fl,
                        gladhouse_inlet_fl,
                        sprntnt_turb,
                        raw_water_turb,
                        raw_water_ph,
                        raw_col,
                        alum_dosing_pump_no1_speed,
                        alum_dosing_pump_no1_stroke,
                        alum_dosing_pump_no2_speed,
                        alum_dosing_pump_no2_stroke,
                        daf_str_1_outlet_turb,
                        daf_str_2_outlet_turb,
                        daf_str_3_outlet_turb,
                        comb_filt_turb,
                        comb_filt_al,
                        coagulated_water_ph)%>%
          collect()%>%
          transmute(obstime = obstime,
                    #flow = megget_inlet_fl+gladhouse_inlet_fl,
                    pacl_pumpstroke = ifelse(alum_dosing_pump_no1_speed > alum_dosing_pump_no2_speed, alum_dosing_pump_no1_stroke, alum_dosing_pump_no2_stroke),
                    comb_filt_turb = comb_filt_turb,
                    comb_filt_al = comb_filt_al,
                    clr_turb = daf_str_2_outlet_turb,
                    coagulated_water_ph = coagulated_water_ph)%>%
          dplyr::filter(
                        pacl_pumpstroke > 10)%>%
          gather(key = "signal", value = "value", -obstime)


ts_eg_zeta<- tbl(rosdb, "zeta")%>%
          dplyr::filter(obstime > "2017-12-19 00:00:00",
                        obstime < "2017-12-23 00:00:00")%>%
          collect(n = Inf)%>%
          arrange(obstime)%>%
          mutate(obstime = obstime+minutes(3),
                 phys_sample = ifelse(subsample == "Sub Result 01", 1, 0),
                 phys_sampleb = cumsum(phys_sample))%>%
          dplyr::group_by(phys_sampleb)%>%
          summarise(obstime = first(obstime),
                    zetap = median(zetap)
                    #,mob = median(mob)
                    )%>%
          dplyr::filter(zetap > -30)%>%
          ungroup()%>%
          dplyr::select(obstime, zetap
                        #, mob
                        )%>%
          gather(key = "signal", value = "value", -obstime)





ts_eg_raw_doc<- tbl(rosdb, "raw_doc")%>%
          dplyr::filter(obstime > "2017-12-19 00:00:00",
                        obstime < "2017-12-23 00:00:00")%>%
          collect(n = Inf)%>%
          arrange(obstime)%>%
          transmute(obstime = obstime,
                    signal = "raw_uv_abs",
                    value = raw_doc/1.5)

ts_eg_clr_doc<- tbl(rosdb, "clr_doc")%>%
          dplyr::filter(obstime > "2017-12-18 00:00:00",
                        obstime < "2017-12-24 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = obstime+-hours(1))%>%
          dplyr::filter(obstime > "2017-12-19 00:00:00",
                        obstime < "2017-12-23 00:00:00")%>%
          arrange(obstime)%>%
          mutate(clr_doc = exp(hampel(log(clr_doc), k = 4, t0 = 2)$y))%>%
          transmute(obstime = obstime,
                    signal = "clr_uv_abs",
                    value = clr_doc/1.5)


ts_eg<- bind_rows(ts_eg_scadasigs, ts_eg_clr_doc, ts_eg_raw_doc, ts_eg_zeta)%>%
          mutate(signal_lab = gsub("clr_uv_abs", "UV254 abs/cm", signal),
                 signal_lab = gsub("zetap","Zeta mV", signal_lab))

rm(ts_eg_scadasigs, ts_eg_clr_doc, ts_eg_raw_doc, ts_eg_zeta)



# load washwater data----


washwater<- tbl(rosdb, "sigs")%>%
          dplyr::select(obstime,
                        sldg_sprntnt_return_fl,
                        sldg_sprntnt_return_sump_lvl,
                        sldg_thickener_str_1_inlet_fl,
                        sldg_thickener_str_2_inlet_fl)%>%
          #filter(obstime > "2016-10-20 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise_all(funs(mean), na.rm = TRUE)%>%
          mutate(lag_sump =  lag(sldg_sprntnt_return_sump_lvl),
                 lag_sump2 =  lag(sldg_sprntnt_return_sump_lvl,2),
                 lag_sump3 =  lag(sldg_sprntnt_return_sump_lvl,3),
                 lag_th1 = lag(sldg_thickener_str_1_inlet_fl,1),
                 lag_th1 = lag(sldg_thickener_str_1_inlet_fl,2),
                 lag_th1 = lag(sldg_thickener_str_1_inlet_fl,3),
                 lag_th2 = lag(sldg_thickener_str_2_inlet_fl,1),
                 lag_th2 = lag(sldg_thickener_str_2_inlet_fl,2),
                 lag_th2 = lag(sldg_thickener_str_2_inlet_fl,3))


washwater$sldg_sprntnt_return_fl[washwater$obstime>"2017/06/21 15:00:00"]<-NA


sup_ret_tree<- lm(sldg_sprntnt_return_fl~., data = washwater%>%
                                      na.omit())



washwater<-washwater%>%
          mutate(est_sup_ret = predict(sup_ret_tree, newdata = washwater))%>%
          dplyr::select(obstime,est_sup_ret)



          
# load zeta data----

zeta<- tbl(rosdb, "zeta")%>%collect(n = Inf)%>%arrange(obstime)%>%
          mutate(obstime = obstime+minutes(3),
                 phys_sample = ifelse(subsample == "Sub Result 01", 1, 0),
                 phys_sampleb = cumsum(phys_sample))%>%
          dplyr::group_by(phys_sampleb)%>%
          summarise(obstime = first(obstime),
                    zetap = mean(zetap),
                    qual_factor = mean(qual_factor),
                    kcps = mean(tot_count_rt_kcps))%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise(zetap = mean(zetap, na.rm = TRUE, trim = 0.1),
                    qual_factor = mean(qual_factor),
                    kcps = mean(kcps))%>%
          ungroup()%>%
          mutate(zetap = hampel(zetap, k = 4, t0 = 2)$y)



# load raw doc----

raw_doc<- tbl(rosdb, "raw_doc")%>%
          filter(obstime > "2016-10-20 00:00:00",
                 raw_doc > 0)%>%
          collect(n = Inf)%>%
          arrange(obstime)%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise(raw_doc = median(raw_doc, na.rm = TRUE, trim= 0.1))%>%
          mutate(raw_doc = exp(hampel(log(raw_doc), k = 4, t0 = 2)$y))


# load clr doc----

clr_doc<- tbl(rosdb, "clr_doc")%>%
          filter(obstime > "2016-10-20 00:00:00",
                 clr_doc > 0)%>%
          collect(n = Inf)%>%
          arrange(obstime)%>%
          mutate(obstime = obstime+minutes(20))%>%
          na.omit()%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise(clr_doc = median(clr_doc, na.rm = TRUE, trim= 0.1))%>%
          mutate(clr_doc = exp(hampel(log(clr_doc), k = 4, t0 = 2)$y))




# load raw sigs----

sigs_rawtime<- tbl(rosdb, "sigs")%>%
          dplyr::select(obstime,
                        megget_inlet_fl,
                        gladhouse_inlet_fl,
                        sldg_sprntnt_return_fl,
                        raw_water_inlet_temperature,
                        sprntnt_turb,
                        raw_water_turb,
                        raw_water_ph,
                        raw_col,
                        alum_dosing_pump_no1_speed,
                        alum_dosing_pump_no1_stroke,
                        alum_dosing_pump_no2_speed,
                        alum_dosing_pump_no2_stroke,
                        alum_storage_tank_1_lvl,
                        alum_storage_tank_2_lvl)%>%
          filter(obstime > "2016-10-20 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise_all(funs(mean), na.rm = TRUE, trim = 0.1)%>%
          mutate(megget_inlet_fl = hampel(megget_inlet_fl, k = 4, t0 = 2)$y,
                 gladhouse_inlet_fl = hampel(gladhouse_inlet_fl, k = 4, t0 = 2)$y,
                 sldg_sprntnt_return_fl = hampel(sldg_sprntnt_return_fl, k = 4, t0 = 2)$y,
                 raw_water_inlet_temperature = hampel(raw_water_inlet_temperature, k = 4, t0 = 2)$y,
                 sprntnt_turb = hampel(sprntnt_turb, k = 4, t0 = 2)$y,
                 raw_water_turb = hampel(raw_water_turb, k = 4, t0 = 2)$y,
                 raw_water_ph = hampel(raw_water_ph, k = 4, t0 = 2)$y,
                 raw_col = hampel(raw_col, k = 4, t0 = 2)$y,
                 alum_dosing_pump_no1_speed = alum_dosing_pump_no1_speed,
                 alum_dosing_pump_no1_stroke = alum_dosing_pump_no1_stroke,
                 alum_dosing_pump_no2_speed = alum_dosing_pump_no2_speed,
                 alum_dosing_pump_no2_stroke = alum_dosing_pump_no2_stroke)


# load coagulation signals----

sigs_coagtime<- tbl(rosdb, "sigs")%>%
          dplyr::select(obstime,
                        coagulated_water_ph)%>%
          filter(obstime > "2016-10-20 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = obstime+minutes(3))%>%
          na.omit()%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise_all(funs(median), na.rm = TRUE, trim = 0.1)%>%
          mutate(coagulated_water_ph = hampel(coagulated_water_ph, k = 4, t0 = 2)$y)

sigs_coagtime$coagulated_water_ph[1741:1742]<-NA


#load clarification signals----

sigs_clrtime<- tbl(rosdb, "sigs")%>%
          dplyr::select(obstime,
                        daf_saturator_1_lvl,
                        daf_saturator_2_lvl,
                        daf_saturator_1_prs,
                        daf_saturator_2_prs,
                        daf_str_1_outlet_turb,
                        daf_str_2_outlet_turb,
                        daf_str_3_outlet_turb)%>%
          filter(obstime > "2016-10-20 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = obstime+minutes(20))%>%
          na.omit()%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise_all(funs(median), na.rm = TRUE, trim = 0.1)%>%
          mutate(daf_saturator_1_lvl = hampel(daf_saturator_1_lvl, k = 4, t0 = 2)$y,
                 daf_saturator_2_lvl = hampel(daf_saturator_2_lvl, k = 4, t0 = 2)$y,
                 daf_saturator_1_prs = hampel(daf_saturator_1_prs, k = 4, t0 = 2)$y,
                 daf_saturator_2_prs = hampel(daf_saturator_2_prs, k = 4, t0 = 2)$y,
                 daf_str_1_outlet_turb = hampel(daf_str_1_outlet_turb, k = 4, t0 = 2)$y,
                 daf_str_2_outlet_turb = hampel(daf_str_2_outlet_turb, k = 4, t0 = 2)$y,
                 daf_str_3_outlet_turb = hampel(daf_str_3_outlet_turb, k = 4, t0 = 2)$y)


# load combined filtration signals----

sigs_filttime<- tbl(rosdb, "sigs")%>%
          dplyr::select(obstime,
                        comb_filt_turb,
                        comb_filt_al)%>%
          filter(obstime > "2016-10-20 00:00:00")%>%
          collect(n = Inf)%>%
          mutate(obstime = obstime+minutes(60))%>%
          na.omit()%>%
          mutate(obstime = round_date(obstime, unit = "24 hours"))%>%
          group_by(obstime)%>%
          summarise_all(funs(median), na.rm = TRUE, trim = 0.1)%>%
          mutate(comb_filt_al = hampel(comb_filt_al, k = 4, t0 = 2)$y)%>%
          mutate(comb_filt_turb = hampel(comb_filt_turb, k = 4, t0 = 2)$y)


# remove weird section of filt Al signal
sigs_filttime$comb_filt_al[sigs_filttime$obstime > "2017-10-15 12:00:00"&
                               sigs_filttime$obstime < "2017-10-16 12:00:00" ]<-NA

# bind signals with sensible lagging----

sigs<-left_join(sigs_rawtime, sigs_coagtime)%>%
          left_join(sigs_clrtime)%>%
          left_join(sigs_filttime)%>%
          dplyr::filter(obstime < max(raw_doc$obstime))

rm(sigs_rawtime,sigs_coagtime,sigs_clrtime,sigs_filttime)



# get signals from coaulant tanks----
pacl_tank<- sigs%>%
          transmute(obstime = obstime,
                    pacl_tank1 = hampel(alum_storage_tank_2_lvl, k = 4, t0 = 0.3)$y,
                    pacl_tank2 = hampel(alum_storage_tank_1_lvl, k = 4, t0 = 0.3)$y,
                    pacl_tankd1 = c(NA ,diff(pacl_tank1)),
                    pacl_tankd2 = c(NA ,diff(pacl_tank2)),
                    pacl_delivery = ifelse(pacl_tankd1 > 0.2 & pacl_tankd2 > 0.2, 1, 0),
                    tank = ifelse(pacl_tankd1 < -0.001 & pacl_tankd2 > -0.001, 1,
                                  ifelse(pacl_tankd2 < -0.001 & pacl_tankd1 >-0.001, 2,
                                         NA)),
                    tank = na.locf(tank, na.rm = FALSE),
                    pacl_level = ifelse(tank == 1  ,pacl_tank1,pacl_tank2))%>%
          dplyr::select(obstime, pacl_level, tank)



# get signals from coaulant pumps----


pacl_pump<- sigs%>%
          dplyr::select(obstime,alum_dosing_pump_no1_speed,
                        alum_dosing_pump_no2_speed,
                        alum_dosing_pump_no1_stroke,
                        alum_dosing_pump_no2_stroke)%>%
          group_by(obstime)%>%
          mutate(pacl_pump = ifelse(alum_dosing_pump_no1_speed > alum_dosing_pump_no2_speed, 1, 2),
                 pacl_pumpspeed = ifelse(alum_dosing_pump_no1_speed > alum_dosing_pump_no2_speed, alum_dosing_pump_no1_speed, alum_dosing_pump_no2_speed),
                 pacl_pumpstroke = ifelse(alum_dosing_pump_no1_speed > alum_dosing_pump_no2_speed, alum_dosing_pump_no1_stroke, alum_dosing_pump_no2_stroke))%>%
          dplyr::select(obstime, pacl_pump, pacl_pumpspeed, pacl_pump,pacl_pumpstroke)%>%
          na.omit()%>%
          ungroup()%>%
          mutate(pacl_pumpspeed = hampel(pacl_pumpspeed, k = 4, t0 = 2)$y)
          


# tidy clarified turbdity signals----
daf_turb<- sigs%>%
          dplyr::select(obstime, daf_str_1_outlet_turb, daf_str_3_outlet_turb)%>%
          gather(key = "stream", value = "turb", -obstime)%>%
          group_by(obstime)%>%
          summarise(clr_turb = max(turb))%>%
          mutate(clr_turb = hampel(clr_turb, k = 4, t0 = 2)$y)




# tidy flow data----

flows<- sigs%>%
          dplyr::select(obstime,megget_inlet_fl,
                        gladhouse_inlet_fl,
                        sldg_sprntnt_return_fl)%>%
          group_by(obstime)%>%
          mutate(flow = megget_inlet_fl+gladhouse_inlet_fl,
                 glad_prop = gladhouse_inlet_fl/flow,
                 sup_ret = sldg_sprntnt_return_fl)%>%
          dplyr::select(obstime, flow, glad_prop, sup_ret)%>%
          na.omit()%>%
          ungroup()#%>%
          #mutate(flow = hampel(flow, k = 4, t0 = 2)$y,
           #      glad_prop = hampel(glad_prop, k = 4, t0 = 2)$y)


filter_run_info<-tbl(rosdb, "filt_sigs")%>%
    dplyr::filter(rgf >4 ,
                  inservice == "ONLINE",
                  obstime > "2016-10-20 00:00:00",
                  obstime < "2018-04-05 00:00:00")%>%
    dplyr::select(obstime,timeinrun,runid, hl, fl, rgf)%>%
    collect()%>%
    filter(is.na(runid)== FALSE,
           timeinrun < 252000,
           timeinrun > 7200)%>%
    group_by(rgf, runid)%>%
    filter(max(timeinrun/3600)>5)%>%
    summarise( obstime =  max(obstime),
               hrs = max(timeinrun)/3600,
               terminal_hl = max(hl),
               cbhl = first(hl),
               hl_acc = terminal_hl-cbhl)%>%
    ungroup()%>%
    mutate( day = floor_date(obstime, unit = "day"))%>%
    group_by(day)%>%
    summarise(hrs = median(hrs),
              #terminal_hl = median(terminal_hl),
              cbhl = median(cbhl),
              hl_acc = median(hl_acc))%>%
    mutate(hrs = hampel(hrs, k = 4, t0 = 2)$y,
           cbhl = hampel(cbhl, k = 4, t0 = 2)$y,
           hl_acc = hampel(hl_acc, k = 4, t0 = 2)$y,
           hl_acc_rate = hl_acc/hrs,
           hl_acc_rate = hampel(hl_acc_rate, k = 4, t0 = 2)$y)




# merge signal data----

sigs<- left_join(sigs, zeta)%>%
          left_join(raw_doc)%>%
          left_join(clr_doc)%>%
          left_join(daf_turb)%>%
          dplyr::select(- alum_dosing_pump_no1_speed,-alum_dosing_pump_no1_stroke, -alum_dosing_pump_no2_speed, -alum_dosing_pump_no2_stroke, -alum_storage_tank_1_lvl,-alum_storage_tank_2_lvl)%>%
          left_join(pacl_pump)%>%
          left_join(pacl_tank)%>%
          left_join(flows)%>%
          left_join(washwater)%>%
          mutate( day = floor_date(obstime, unit = "day"))%>%
        left_join(., filter_run_info)


rm(clr_doc, daf_turb, flows, pacl_pump, pacl_tank, raw_doc, washwater, zeta)



sigs<-sigs%>%
          mutate(raw_uv_abs = raw_doc*1.5,
                 clr_uv_abs = clr_doc*1.5,
                 clr_uv_abs = ifelse(clr_uv_abs>10, NA, clr_uv_abs))%>%
          mutate(doc_rem = (raw_doc - clr_doc) / raw_doc,
                 delta_uv = (raw_uv_abs - clr_uv_abs) ,
                 temp = raw_water_inlet_temperature)%>%
          dplyr::select( - megget_inlet_fl, -gladhouse_inlet_fl, -sldg_sprntnt_return_fl,-raw_water_inlet_temperature, -day)%>%
          mutate(raw_water_ph = ifelse(obstime > "2017-03-01 00:00:00" & obstime < "2017-06-04 00:00:00", NA, raw_water_ph ),
                 raw_water_ph = ifelse(obstime > "2017-07-01 00:00:00" & raw_water_ph < 6.85, NA, raw_water_ph ),
                 coagulated_water_ph = ifelse(obstime > "2017-05-09 08:00:00" & obstime < "2017-10-01 00:00:00", NA, coagulated_water_ph ),
                 coagulated_water_ph = ifelse(obstime < "2016-10-27 17:00:00", NA, coagulated_water_ph ),
                 sup_ret =  ifelse(obstime > "2017-06-21 16:00:00" & obstime < "2018-01-01 00:00:00", NA, sup_ret ),
                 comb_filt_al = ifelse(comb_filt_al< 0.001, NA,comb_filt_al),
                 raw_water_turb = ifelse(raw_water_turb>3, NA, raw_water_turb),
                 raw_water_turb = ifelse(raw_water_turb<0.6, NA, raw_water_turb))%>%
          mutate(pacl_lines = ifelse(obstime > "2017-08-13 00:00:00", 2,1),
                 pump2_repair = ifelse(obstime > "2017-10-13 00:00:00", 2,1))%>%
          mutate(datasplit = "train",
                 datasplit = ifelse(obstime>"2017-10-20 00:00:00", "test", datasplit))%>%
          mutate(doc_rem_pct = (raw_doc-clr_doc)/raw_doc*100,
                    glad_prop_fact = factor(paste0(round(sigs$glad_prop,2)*100,"% B")))#%>%
          #dplyr::filter(obstime< "2018-04-15 00:00:00")




sigs<- sigs%>%mutate(sprntnt_turb = ifelse(sprntnt_turb<2.5, NA, sprntnt_turb),
                      raw_col_med =rollapply(raw_col, width = 15, FUN = "median", fill = NA),
                      raw_col_er = raw_col-raw_col_med,
                      raw_col = ifelse(raw_col_er>2, NA, raw_col),
                      raw_uv_abs = ifelse(raw_uv_abs> 20, NA, raw_uv_abs),
                      raw_uv_abs = ifelse(month(obstime)> 10 & year(obstime)==2017 &raw_uv_abs<15, NA, raw_uv_abs),
                      clr_uv_abs = ifelse(clr_uv_abs>12, NA, clr_uv_abs),
                      flow = ifelse(flow<315, NA, flow),
                      glad_prop = ifelse(glad_prop<0.35, NA, glad_prop),
                      est_sup_ret =rollapply(est_sup_ret, width = 11, FUN = "mean", fill = NA, align = "right"),
                      delta_uv = (raw_uv_abs - clr_uv_abs))
                    


sigs_impute_train<-sigs%>%
          dplyr::filter(datasplit == "train")%>%
          dplyr::select(sprntnt_turb,
                                  raw_water_turb,
                                  raw_water_ph,
                                  raw_col,
                                  coagulated_water_ph,
                                  comb_filt_turb,
                                  comb_filt_al,
                                  zetap,
                                  clr_turb,
                                  pacl_pump,
                                  pacl_pumpspeed,
                                  pacl_pumpstroke,
                                  pacl_level,
                                  tank,
                                  flow,
                                  glad_prop,
                                  #sup_ret,
                                  est_sup_ret,
                                  raw_uv_abs,
                                  clr_uv_abs,
                                  temp,
                                  pacl_lines,
                                  pump2_repair,
                                  clr_turb, 
                                  hrs, 
                                  cbhl, 
                                  hl_acc,
                                  hl_acc_rate,
                                    kcps)%>%
          mutate(raw_water_turb = na.locf(raw_water_turb),
                 comb_filt_turb = na.locf(comb_filt_turb),
                 comb_filt_al = na.locf(comb_filt_al),
                 pacl_pump = na.locf(pacl_pump),
                 pacl_pumpspeed = na.locf(pacl_pumpspeed),
                 pacl_pumpstroke = na.locf(pacl_pumpstroke),
                 pacl_level = ifelse(is.na(pacl_level)==TRUE, 2.5, pacl_level),
                 tank = ifelse(is.na(tank)==TRUE, 2, tank),
                 est_sup_ret = ifelse(is.na(est_sup_ret)==TRUE, mean(est_sup_ret, na.rm = TRUE), est_sup_ret))

sigs_impute_test<-sigs%>%
          dplyr::filter(datasplit == "test")%>%
          dplyr::select(sprntnt_turb,
                        raw_water_turb,
                        raw_water_ph,
                        raw_col,
                        coagulated_water_ph,
                        comb_filt_turb,
                        comb_filt_al,
                        zetap,
                        clr_turb,
                        pacl_pump,
                        pacl_pumpspeed,
                        pacl_pumpstroke,
                        pacl_level,
                        tank,
                        flow,
                        glad_prop,
                        #sup_ret,
                        est_sup_ret,
                        raw_uv_abs,
                        clr_uv_abs,
                        temp,
                        pacl_lines,
                        pump2_repair, 
                        hrs, 
                        cbhl, 
                        hl_acc,
                        hl_acc_rate,
                        kcps)%>%
          mutate(raw_water_turb = na.locf(raw_water_turb),
                 comb_filt_turb = na.locf(comb_filt_turb),
                 comb_filt_al = na.locf(comb_filt_al),
                 pacl_pump = na.locf(pacl_pump),
                 pacl_pumpspeed = na.locf(pacl_pumpspeed),
                 pacl_pumpstroke = na.locf(pacl_pumpstroke),
                 pacl_level = ifelse(is.na(pacl_level)==TRUE, 2.5, pacl_level),
                 tank = ifelse(is.na(tank)==TRUE, 2, tank),
                 est_sup_ret = ifelse(is.na(est_sup_ret)==TRUE, mean(est_sup_ret, na.rm = TRUE), est_sup_ret))



sigs_impute_missforest_train<-missForest(sigs_impute_train%>%as.matrix(), maxiter = 10, ntree = 100,
                                   variablewise = T, decreasing = T,
                                   verbose = T,
                                   nodesize = c(50,50))

sigs_impute_missforest_test<-missForest(sigs_impute_test%>%as.matrix(), maxiter = 10, ntree = 100,
                                         variablewise = T, decreasing = T,
                                         verbose = T,
                                         nodesize = c(50,50))



sigs_imputed_train<- bind_cols(sigs%>%
                    dplyr::select(obstime, datasplit)%>%
                    dplyr::filter(datasplit == "train"),
                    sigs_impute_missforest_train$ximp%>%data.frame())%>%
          mutate(delta_uv = raw_uv_abs-clr_uv_abs,
                 rel_delta_uv = delta_uv/raw_uv_abs)


sigs_imputed_test<- bind_cols(sigs%>%dplyr::select(obstime, datasplit)%>%
                                        dplyr::filter(datasplit == "test"),
                              sigs_impute_missforest_test$ximp%>%data.frame())%>%
          mutate(delta_uv = raw_uv_abs-clr_uv_abs,
                 rel_delta_uv = delta_uv/raw_uv_abs)

sigs_imputed<- bind_rows(sigs_imputed_train, sigs_imputed_test)



ros_rwq<- read.csv("DATA/ros_results.csv", stringsAsFactors = F)%>%
          mutate(Sampled.Timestamp = ymd(Sampled.Timestamp))%>%
          dplyr::filter(Analysis.Type == "CHEM_ROUT")%>%
          dplyr::filter(Sample.Point.Code == "ROSEBERY_1_WS"|
                                  Sample.Point.Code == "ROSEBERY_2_WS")%>%
          dplyr::filter(Sampled.Timestamp > ymd("2016-10-20"),
                        Sampled.Timestamp < ymd("2018-03-23"))%>%
          dplyr::select(Sampled.Timestamp,Sample.Point.Code, Determinand, Unit.Of.Measure,Numeric.Result, Result.Number)%>%
          dplyr::filter(Determinand %in% c("Colour","Turbidity", "Hydrogen Ion", "Absorbance", "Uv Transmittance",
                                           "Total Organic Carbon","Total Organic Carbon (Filtered)","Alkalinity",
                                           "Absorbance", "Ammonium", "Nitrate", "Nitrite", "Soluble Reactive Phosphate"))%>%
          dplyr::filter(Determinand != "Turbidity"| Numeric.Result < 5)%>%
          unite(.,col = "Determinand", c("Determinand", "Unit.Of.Measure"), sep = " ")%>%
          distinct()%>%
          dplyr::select(-Result.Number)


ros_rwq_wide<- ros_rwq%>%
          spread(key = Determinand, value = Numeric.Result)%>%
          mutate(`Absorbance UV/cm` = (2-log10(`Uv Transmittance %`)),
                 SUVA = `Absorbance UV/cm`/`Total Organic Carbon mgC/l`*100)

ros_rwq_summary<-ros_rwq_wide%>%
          #select(-Sampled.Timestamp)%>%
          gather(key = "Determinand", value = "Numeric.Result", -Sample.Point.Code, -Sampled.Timestamp)%>%
          group_by(Sample.Point.Code,Determinand)%>%
          summarise(mean = round(mean(Numeric.Result, na.rm = TRUE),2),
                    `Q05` = round(quantile(Numeric.Result, na.rm = TRUE, probs = 0.05),2),
                    `Q25` = round(quantile(Numeric.Result, na.rm = TRUE, probs = 0.25),2),
                    `Q50` = round(quantile(Numeric.Result, na.rm = TRUE, probs = 0.5),2),
                    `Q75` = round(quantile(Numeric.Result, na.rm = TRUE, probs = 0.75),2),
                    `Q95` = round(quantile(Numeric.Result, na.rm = TRUE, probs = 0.95),2),
                    n = length(na.omit(Numeric.Result)))%>%
          mutate(Source = gsub("ROSEBERY_2_WS", "Source A", Sample.Point.Code),
                 Source = gsub("ROSEBERY_1_WS", "Source B", Source))%>%
          ungroup()%>%
          dplyr::select(Source,Determinand, mean, Q05,Q25,Q75,Q95,n )%>%
          arrange(Determinand,Source)







save.image(file = "DATA/ZetaPaperDataDay.Rdata")

RPushbullet::pbPost("note", "Script complete", "Data prep complete")
          
rm(list=ls())
