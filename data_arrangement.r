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
athlos[, c("study", "country") := lapply(.SD, haven::as_factor), .SDcols = c("study", "country")]

athlos[, .(athlos_id, athlos_id2, study, cohort, wave, country, age, sex, marital_status, education, employed, retired, wealth, healthstatus, resid_place, confidant, spouse, cont_fr, cont_rel, depression, anxiety_symp, loneliness)
       ][, lapply(.SD, \(.x) sum(is.na(.x)))]


athlos[, .(athlos_id, athlos_id2, study, cohort, wave, country, age, sex, marital_status, education, employed, retired, wealth, healthstatus, resid_place, confidant, spouse, cont_fr, cont_rel, depression, anxiety_symp, loneliness)
       ][, lapply(.SD, \(.x) sum(!is.na(.x))), by = .(study, cohort, wave)] |> 
View()
