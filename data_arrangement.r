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
athlos[, `:=` (education = fcase(study == "10/66" & wave == 2 & !is.na(yintw), shift(education, n = 1L, type = "lag"), 
                            study == "ALSA" & wave == 2 & is.na(education), shift(education, n = 1L, type = "lag"),
                            study == "CHARLS" & wave == 2 & is.na(education) & !is.na(yintw), shift(education, n = 1L, type = "lag"),
                            study == "JSTAR" & wave == 2 & !is.na(yintw), shift(education, n = 1L, type = "lag"),
                            study == "MHAS" & wave == 2 & is.na(education), shift(education, n = 1L, type = "lag"),
                            rep(TRUE, .N),  education),
               w = fcase(study == "ALSA" & wave == 11 & !is.na(yintw), shift(w, n = 10L, type = "lag"),
                         study == "ALSA" & wave == 3 & !is.na(yintw), shift(w, n = 2L, type = "lag"),
                         rep(TRUE, .N), w),
               sex = fcase(study == "ALSA" & wave == 11 & !is.na(yintw), shift(sex, n = 10L, type = "lag"),
                         rep(TRUE, .N), sex)), 
       by = .(athlos_id2)
       ][, education := fcase(study == "ALSA" & wave == 11 & !is.na(yintw), shift(education, n = 9L, type = "lag"),
                              study == "ALSA" & wave == 3 & !is.na(yintw), shift(education, n = 1L, type = "lag"),
                              study == "JSTAR" & wave == 3 & !is.na(yintw), shift(education, n = 1L, type = "lag"),
                              study == "MHAS" & wave == 3 & !is.na(yintw) & is.na(education), shift(education, n = 1L, type = "lag"),
                              rep(TRUE, .N),  education), 
         by = .(athlos_id2)
         ][, education := fcase(study == "MHAS" & wave == 3 & !is.na(yintw) & is.na(education), shift(education, n = 2L, type = "lag"),
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


athlos[study == "SHARE", .(athlos_id2, study, cohort, wave, country, yintw, w,
                                              age, sex, marital_status, education, healthstatus, 
                                              loneliness, 
                                              depression, anxiety_symp)
         ][, lapply(.SD, \(.x) sum(!is.na(.x))), by = .(study, cohort, wave)] |> 
  t() |> 
  View()
athlos[study=="MHAS" & !is.na(yintw), .(.N), keyby = .(study, cohort, wave, yintw)] |> print(nrows = 999)



athlos[study=="MHAS" & wave == 3 & !is.na(yintw) & is.na(education) & (athlos_id %in% athlos[study == "MHAS" & wave == 1 & !is.na(yintw) & !is.na(education)]$athlos_id)]$age |> quantile(probs = seq(0,1,0.05), na.rm = TRUE)

options(max.print = 999999)
athlos[study=="10/66", .(.N), by = .(study, cohort, wave, yintw)] |> print(nrows = 999)
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

psathlos <- athlos[((study == "10/66" & wave == 2) | 
                      (study == "ALSA" & wave == 11) | 
                      (study == "CHARLS" & wave == 2) | 
                      (study == "COURAGE" & wave == 1 & cohort != "Finland") |
                      (study == "ELSA" & wave == 6) |
                      (study == "HRS" & wave == 10) |
                      (study == "Health2000-2011" & wave == 2) |
                      (study == "JSTAR" & yintw == 2011) |
                      (study == "KLOSA" & wave == 3) |
                      (study == "LASI") |
                      (study == "MHAS" & wave == 3) |
                      (study == "SHARE" & wave == 5) |
                      (study == "TILDA" & wave == 1)) & !is.na(yintw), .(athlos_id2, study, cohort, wave, country, yintw, w,
                                                                         age, sex, marital_status, education, employed, healthstatus, resid_place, 
                                                                         confidant, loneliness, 
                                                                         depression, anxiety_symp, suicidal_ideation_12m, suicidal_ideation_lm)]



psathlos2 <- athlos[((study == "10/66" & wave == 1) | 
                      (study == "ALSA" & wave == 3) | 
                      (study == "CHARLS" & wave == 2) | 
                      (study == "COURAGE" & wave == 1 & cohort != "Finland") |
                      (study == "ELSA" & wave == 1) |
                      (study == "HAPIEE" & wave == 1 & cohort != "Lithuania") | # Lithuania has no loneliness data
                      (study == "HRS" & cohort %in% c("hrs", "ahead") & wave == 2) |
                      (study == "HRS" & cohort %in% c("coda", "wb") & wave == 4) |
                      (study == "HRS" & cohort == "ebb" & wave == 7) |
                      (study == "HRS" & cohort == "mbb" & wave == 10) |
                      (study == "Health2000-2011" & wave == 2) |
                      (study == "JSTAR" & wave == 1) |
                      (study == "KLOSA" & wave == 1) |
                      (study == "LASI") |
                      (study == "MHAS" & wave == 1) |
                      (study == "SAGE") |
                      (study == "SHARE" & wave == 5) |
                      (study == "TILDA" & wave == 1)) & !is.na(yintw), .(athlos_id2, study, cohort, wave, country, yintw, w,
                                                                         age, sex, marital_status, education, employed, healthstatus, resid_place, 
                                                                         confidant, loneliness, 
                                                                         depression, anxiety_symp, suicidal_ideation_12m, suicidal_ideation_lm)]

# psathlos <- na.omit(psathlos, cols = c("age", "sex", "loneliness"))
 
# library(dplyr)
# library(dtplyr)
# psathlos_dt <- lazy_dt(psathlos)
# psathlos_dt |> 
#   filter(!if_any(c(age, sex, loneliness), is.na)) |> 
#   as.data.table() |> 
#   dim()

library(haven)
write_dta(psathlos, path = paste0(data_add, "psathlos.dta"))
write_dta(psathlos2, path = paste0(data_add, "psathlos2.dta"))
