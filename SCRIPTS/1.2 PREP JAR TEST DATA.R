# results of jar testing


# inital jar tests

initial_jars_zeta<- read.csv("DATA/MASTERJARS/jar_zeta.csv")%>%
          filter(Dose != "")%>%
          mutate(Source = ifelse(Source == "Meggett", "Source A", "Source B"))

mgl_as_al<- seq(0.8,2.6,0.2)
moles_perl_al<-mgl_as_al/(26.981539*1000)
dilution<- (90000/1.4)/mgl_product
jar_vol <- 1
dose_ul_product<- (jar_vol/dilution)*1E6
dose_ul_Al <- 

source = c("Gladhouse", "Megget")


dose_determination<- expand.grid( source = source, dose_ul = dose_ul_product)


1/(26.981539*1000)

# read jar data
masterjars<- read.csv("DATA/MASTERJARS/MASTERJARS.csv", stringsAsFactors = FALSE)%>%
          mutate(start = dmy_hms(paste0(date," ",start, ":00"), tz = "europe/london"),
                 dose = dmy_hms(paste0(date," ",dose, ":00"), tz = "europe/london"),
                 end = dmy_hms(paste0(date," ", end, ":00"), tz = "europe/london"),
                 masterjar = paste0(day,jar))%>%
          dplyr::filter(#source != "Gladhouse",
                        jar != 21)%>%
          mutate(rapidmix = gsub("norapidmix","no rapid mix", rapidmix),
                 temp_label = temp_code,
                 temp_label = gsub("HIGH", "HIGH (19.5°C ± 2)", temp_label),
                 temp_label = gsub("LOW", "LOW (7.5°C ± 2)", temp_label),
                 rpm_label = paste(rpm," RPM floc mixing"),
                 mix_label = paste(rapidmix, " & ", rpm_label))%>%
    mutate(Source = ifelse(source == "Megget", "Source A", "Source B"))





masterjars%>%
          group_by(temp_code)%>%
          summarise(mean = mean(temperature, na.rm=T),
                    max = max(temperature, na.rm=T),
                    min = min(temperature, na.rm=T))

# read zeta data
masterjar_zeta<- list.files("DATA/MASTERJARS/", pattern ="masterjar_zeta", full.names = TRUE)%>%
          lapply(., FUN = read.table, sep = "\t", stringsAsFactors = FALSE,header = TRUE )%>%
          bind_rows()%>%
          filter(grepl("Transfer", Sample.Name) == FALSE)%>%
          separate(Sample.Name, into = c("Source", "CoagDose", "Coag", "masterjar", "rep"), sep = " ")%>%
          mutate(masterjar = gsub("masterjar", "",tolower(masterjar)))%>%
          group_by(masterjar)%>%
          summarise(ZetaPotential = median(ZP..mV.))


# join jars and zeta
mjz<-left_join( masterjars,masterjar_zeta)%>%
          filter(is.na(source)==FALSE)


# read mastersizer results
masterjars_std<- list.files("DATA/MASTERSIZER/", pattern ="STD", full.names = TRUE)%>%
          lapply(., FUN = read.csv, stringsAsFactors = FALSE,fileEncoding="utf-16")%>%
          bind_rows(.)%>%
          mutate(obstime = dmy_hms(Analysis.Date.Time, tz = "europe/london"))



# joint jar and size info 
masterjars_stddata<- sqldf::sqldf("SELECT * FROM masterjars_std LEFT JOIN mjz ON masterjars_std.obstime > mjz.start AND masterjars_std.obstime < mjz.end", drv = "SQLite")%>%
          dplyr::filter(is.na(masterjar)==F)%>%
          group_by(masterjar)%>%
          mutate(jartime = as.numeric(difftime( obstime,dose, units = "min")),
                 Dx..50. = ifelse(masterjar == "thur5" & Dx..50. > 35, NA, Dx..50.),
                 Dx..50. = ifelse( Dx..50. > 100, NA, Dx..50.))%>%
        mutate(Source = ifelse(source == "Megget", "Source A", "Source B"))





