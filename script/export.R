suppressMessages(require(googledrive))
suppressMessages(require(zip))

wd <- getwd()
setwd("/srv/shiny-server/app-plan/")
name <- paste0("appplan_dev_",gsub(pattern = "[-]",replacement = "",as.character(as.Date(Sys.time(),"%Y%m%d"))),".zip")
file_upload <- paste0("/srv/shiny-server/app-plan/",name)
local <- "/srv/shiny-server/app-plan/"
zip(file_upload, local)
drive_auth("security/key_google.rds")
drive_upload(name,as_id("1Ojtn-lQIpnn-48pFv_nEkUjS8rx8q2B8"))
drive_deauth()
setwd(wd)

