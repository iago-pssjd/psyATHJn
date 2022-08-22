## data_arrangement.r

### data loading

if(Sys.info()["sysname"] == "Linux"){
	data_add <- paste0("/media/",
			   system("whoami", intern = TRUE),
			   "/",
			   system(paste0("ls /media/",system("whoami", intern = TRUE)), intern = TRUE),
			   "/PSSJD/athlos/data/")
	data_add <- data_add[file.exists(data_add)]
} else if(Sys.info()["sysname"] == "Windows"){
	data_add <- paste0(grep("^[A-Z]:$", sub(":(.*)", ":",shell("wmic logicaldisk get name", intern = TRUE)), value = TRUE), "/PSSJD/athlos/data")
	data_add <- data_add[file.exists(data_add)]
	data_add <- paste0(data_add, "/")
}

load(paste0(data_add, "athlos_v2.1.1.rdata"))

### libraries attaching

library(data.table)

setDT(athlos)
cols <- names(athlos)
athlos[, c(cols) := lapply(.SD, \(.x) replace(.x, list = which(.x %in% 991:999), values = NA))]
athlos[, c("study", "country") := lapply(.SD, haven::as_factor), .SDcols = c("study", "country")]
setorder(athlos, study, cohort, country, athlos_id, wave)

# For studies with old population like 10/66 or ALSA, education is imputed with last previous data
# For CHARLS, are generally 45+ (1stQ =50/51), so it is also imputed
athlos[, education := fcase(study == "10/66" & wave == 2 & !is.na(yintw), shift(education, n = 1L, type = "lag"), 
                            study == "ALSA" & wave == 2 & is.na(education), shift(education, n = 1L, type = "lag"),
                            study == "ALSA" & wave == 11 & !is.na(yintw), shift(education, n = 9L, type = "lag"),
                            study == "CHARLS" & wave == 2 & is.na(education) & !is.na(yintw), shift(education, n = 1L, type = "lag"),
			    study == "JSTAR" & wave == 2 & !is.na(yintw), shift(education, n = 1L, type = "lag"),
			    study == "JSTAR" & wave == 3 & !is.na(yintw), shift(education, n = 2L, type = "lag"),
                            rep(TRUE, .N),  education), 
       by = .(athlos_id2)]

# athlos[, .(athlos_id, athlos_id2, study, cohort, wave, country, age, sex, marital_status, education, employed, retired, wealth, healthstatus, resid_place, confidant, spouse, cont_fr, cont_rel, depression, anxiety_symp, loneliness)
#        ][, lapply(.SD, \(.x) sum(is.na(.x)))]
# 
# 
# athlos[, .(athlos_id, athlos_id2, study, cohort, wave, country, age, sex, marital_status, education, employed, retired, wealth, healthstatus, resid_place, confidant, spouse, cont_fr, cont_rel, depression, anxiety_symp, loneliness)
#        ][, lapply(.SD, \(.x) sum(!is.na(.x))), by = .(study, cohort, wave)] |> 
# View()

athlos[, .(athlos_id2, study, cohort, wave, country, yintw,
           age, sex, marital_status, education, employed, healthstatus, resid_place, 
           confidant, loneliness,
           depression, anxiety_symp, suicidal_ideation_12m, suicidal_ideation_lm)
       ][, lapply(.SD, \(.x) sum(is.na(.x)))]


athlos[study == "TILDA", .(athlos_id2, study, cohort, wave, country, yintw, 
           age, sex, marital_status, education, employed, healthstatus, resid_place, 
           confidant, loneliness, 
           depression, anxiety_symp, suicidal_ideation_12m, suicidal_ideation_lm)
       ][, lapply(.SD, \(.x) sum(!is.na(.x))), by = .(study, cohort, wave)] |> 
  t() |> 
  View()

options(max.print = 999999)
athlos[study=="SHARE", .(.N), by = .(study, cohort, wave, yintw)] |> print(nrows = 999)
athlos[order(study, cohort, country, wave, yintw), .(.N), by = .(study, cohort, wave, country, yintw)] |> View()




#NOTES
# 2010 - 2015
# loneliness && (depression or anxiety) && sex && age (&& marital_status && education && healthstatus if possible)

# 10/66 wave 2; missing values of yintw in wave 2 are id's participating only in wave 1
# ALSA wave 11
# ATTICA has no loneliness
# CHARLS wave 2 (more individuals)
# COURAGE wave 1 (remove Finland conflicting with Health2000)
# ELSA wave 6
# ENRICA has no loneliness
# HAPIEE study is outside years range 2010-2015
# HRS wave 10
# Health2000-2011 wave 2
# JSTAR last waves (yintw == 2011)
# KLOSA wave 3
# LASI
# MHAS wave 3
# SAGE study is outside years range 2010-2015
# SHARE wave 5
# TILDA wave 1
