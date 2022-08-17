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


athlos[, .(athlos_id2, study, cohort, wave, country, yintw, 
           age, sex, marital_status, education, employed, healthstatus, resid_place, 
           confidant, loneliness, 
           depression, anxiety_symp, suicidal_ideation_12m, suicidal_ideation_lm)
       ][, lapply(.SD, \(.x) sum(!is.na(.x))), by = .(study, cohort, wave)] |> 
  View()

options(max.print = 999999)
athlos[, .(.N), by = .(study, cohort, wave, country, yintw)] |> print(nrows = 999)
athlos[, .(.N), by = .(study, cohort, wave, country, yintw)] |> View()

#NOTES
# 2010 - 2015
# loneliness && (depression or anxiety) && sex && age (&& marital_status && education && healthstatus if possible)

# 10/66 wave 2; missing values of yintw in wave 2 are id's participating only in wave 1

