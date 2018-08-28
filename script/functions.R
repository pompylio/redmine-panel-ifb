extract_redmine <- function(pool_connect,select_project,year_project, col_names,col_select,select_custom_values){
  require(pool)
  require(RMySQL)
  require(tidyr)
  require(stringr)
  verify_pool <- function(){
    yes <- c("S","s","Y","y","Sim","sim","SIM","Yes","yes","YES")
    message("The connection data was not informed or not found. Want to inform? (Y or N)")
    question <- scan(what = "character",nmax = 1,quiet = TRUE,encoding = "Latin-1")
    if(question %in% yes){
      message("Inform a name, host, username and password:")
      pool_connect <- scan(what = "character",nmax = 4,quiet = TRUE,encoding = "Latin-1")
      names(pool_connect) <- c("name","host","username","password")
      return(pool_connect)
    }else{
      stop()
    }
  }
  if(missing(pool_connect)){
    pool_connect <- verify_pool()
  }else{
    if(!length(pool_connect) == 4){
      pool_connect <- verify_pool()
    }else{
      if(class(pool_connect)=="list"){
        pool_connect <- unlist(pool_connect)
        names(pool_connect) <- c("name","host","username","password")
      }}}
  if(missing(select_custom_values)){
    select_custom_values <- FALSE
  }
  connect <- dbPool(drv = MySQL(),dbname = pool_connect["name"],host = pool_connect["host"],username = pool_connect["username"],password = pool_connect["password"],idleTimeout = 3600000)
  conn <- poolCheckout(connect)
  date_dbi <- dbWithTransaction(conn, {dbGetQuery(conn, "SELECT updated_on FROM issues ORDER BY updated_on DESC LIMIT 1")})
  update_dbi <- as.POSIXct(strptime(date_dbi$updated_on,"%Y-%m-%d %H:%M:%S"))
  tb <- c("custom_fields","custom_values","issues","issue_statuses","projects","trackers","users")
  for(i in 1:NROW(tb)){assign(tb[i],dbReadTable(conn = conn,name = tb[i]))}
  custom_fields[,sapply(custom_fields, class) == "character"] <- apply(X = custom_fields[,sapply(custom_fields, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  custom_values[,sapply(custom_values, class) == "character"] <- apply(X = custom_values[,sapply(custom_values, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  issue_statuses$name <- iconv(x = issue_statuses$name,from = "latin1",to = "UTF-8")
  issues[,sapply(issues, class) == "character"] <- apply(X = issues[,sapply(issues, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  projects[,sapply(projects, class) == "character"] <- apply(X = projects[,sapply(projects, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  trackers$name <- iconv(x = trackers$name,from = "latin1",to = "UTF-8")
  users[,sapply(users, class) == "character"] <- apply(X = users[,sapply(users, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
  if(select_custom_values == TRUE){
    custom_values <- merge(x = custom_values,y = custom_fields[,c("id","name")],by.x = "custom_field_id",by.y = "id",all.x = TRUE,sort = FALSE)
    custom_values <- custom_values[custom_values$custom_field_id %in% c(120,121,122,136,145,146,147),c("custom_field_id","customized_id","name","value")]
    custom_values[custom_values$custom_field_id %in% c(120,121,122),]$name <- "setor"
    custom_values[custom_values$custom_field_id %in% c(136),]$name <- "natureza_despesa"
    custom_values[custom_values$custom_field_id %in% c(145),]$name <- "limite_orcamento"
    custom_values[custom_values$custom_field_id %in% c(146),]$name <- "previsao_orcamento"
    custom_values[custom_values$custom_field_id %in% c(147),]$name <- "validacao"
    custom_values <- custom_values[,-c(1)]
    custom_values1 <- custom_values[custom_values$name == "setor",]
    custom_values1 <- spread(data = custom_values1,key = name,value = value,fill = "")
    custom_values2 <- custom_values[custom_values$name == "natureza_despesa",]
    custom_values2 <- spread(data = custom_values2,key = name,value = value,fill = "")
    custom_values3 <- custom_values[custom_values$name == "limite_orcamento",]
    custom_values3 <- spread(data = custom_values3,key = name,value = value,fill = "")
    custom_values3$limite_orcamento <- str_replace_all(string = custom_values3$limite_orcamento,c("[.]"="","[,]"="."))
    custom_values3$limite_orcamento <- as.numeric(custom_values3$limite_orcamento)
    custom_values4 <- custom_values[custom_values$name == "previsao_orcamento",]
    custom_values4 <- spread(data = custom_values4,key = name,value = value,fill = "")
    custom_values4$previsao_orcamento <- str_replace_all(string = custom_values4$previsao_orcamento,c("[.]"="","[,]"="."))
    custom_values4$previsao_orcamento <- as.numeric(custom_values4$previsao_orcamento)
    custom_values5 <- custom_values[custom_values$name == "validacao",]
    custom_values5 <- spread(data = custom_values5,key = name,value = value,fill = "")
    custom_values <- merge(x = custom_values1,y = custom_values2,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values3,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values4,by.x = "customized_id",by.y = "customized_id",all = TRUE)
    custom_values <- merge(x = custom_values,y = custom_values5,by.x = "customized_id",by.y = "customized_id",all = TRUE)
  }
  if(!missing(select_project)){
    projects$year <- str_extract(string = projects$name,pattern = "[0-9]{4,4}")
    projects$year <- as.integer(ifelse(nchar(projects$year) == 4,projects$year,""))
    if(length(select_project)==1){
      proj <- projects[grepl(pattern = select_project,projects$name) & projects$year %in% c(year_project),]$id
      issues <- issues[issues$project_id %in% proj,]
    }else if(length(select_project)>1){
      proj <- c()
      for(i in 1:length(select_project)){
        p <- projects[grepl(pattern = select_project[i],projects$name) & projects$year %in% c(year_project),]$id
        proj <- append(proj,p)
      }
      issues <- issues[issues$project_id %in% proj,]
    }
  }
  issues <- merge(x = issues,y = trackers[,c("id","name")],by.x = "tracker_id",by.y = "id",all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = projects[,c("id","name","status")],by.x = "project_id",by.y = "id",suffixes = c("","_project"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = projects[,c("id","name")],by.x = "tracker_id",by.y = "id",suffix = c("","_project_parent"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = issue_statuses[,c("id","name")],by.x = "status_id",by.y = "id",suffix = c("","_status"),all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "assigned_to_id",by.y = "id",all.x = TRUE,sort = FALSE)
  issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "author_id",by.y = "id",suffix = c("","_author"),all.x = TRUE,sort = FALSE)
  if(select_custom_values == TRUE){
    issues <- merge(x = issues,y = custom_values,by.x = "id",by.y = "customized_id",all.x = TRUE,sort = FALSE)
  }
  issues$name <- str_replace_all(issues$name,c("[ç]"="c","[ã]"="a","[õ]"="o","[á]"="a","[é]"="e","[í]"="i","[ó]"="o","[ú]"="u","[:punct:]"=""))
  issues$correct_status <- ifelse(issues$status_id == 5,"Fechada",ifelse(issues$done_ratio == 100,"Finalizada",ifelse(issues$done_ratio == 0,"Nao_Iniciada","Iniciada")))
  issues$deadline_status <- ifelse(is.na(issues$due_date),"Sem_Data",ifelse(issues$correct_status == "Finalizada","Concluida",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) <= class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date(),"No_prazo",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) >  class(as.numeric(substr(issues$start_date,1,4))),"Em_atraso",ifelse(issues$correct_status == "Iniciada"   & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date < Sys.Date(),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date() & issues$start_date >= Sys.Date(),"No_prazo",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) >  class(as.numeric(substr(issues$start_date,1,4))),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date < Sys.Date(),"Em_atraso",ifelse(issues$correct_status == "Nao_Iniciada" & class(as.numeric(substr(Sys.Date(),1,4))) <=  class(as.numeric(substr(issues$start_date,1,4))) & issues$due_date >= Sys.Date() & issues$start_date <= Sys.Date(),"Em_atraso","Sem_Definicao")))))))))
  issues$year <- str_extract(string = issues$name_project,pattern = "[0-9]{4,4}")
  issues$department <- str_extract(string = issues$name_project,pattern = "[^(]*[)$]")
  issues$department <- substr(x = issues$department,start = 1,nchar(issues$department)-1)
  if(select_custom_values == TRUE){
    issues$department_initials <- ifelse(is.na(issues$setor)|issues$setor == "","Nao_Informado",as.character(substr(x = issues$setor,start = as.numeric(gregexpr(pattern = "[(]",issues$setor))+1,stop = as.numeric(gregexpr(pattern = "[)]",issues$setor))-1)))
  }
  issues$due_date <- as.Date(issues$due_date)
  issues$created_on <- as.POSIXct(strptime(issues$created_on,"%Y-%m-%d %H:%M:%S"))
  issues$updated_on <- as.POSIXct(strptime(issues$updated_on,"%Y-%m-%d %H:%M:%S"))
  issues$start_date <- as.Date(issues$start_date)
  issues$assign <- ifelse(is.na(issues$firstname) & is.na(issues$lastname),"Sem Atribuicao",paste(issues$firstname,issues$lastname))
  issues$author <- paste(issues$firstname_author,issues$lastname_author)
  issues$parent_id0 <- issues$parent_id
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id0",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","1"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id1",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","2"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id2",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","3"))
  issues <- merge(x = issues,y = issues[,c("id","name","parent_id","subject")],by.x = "parent_id3",by.y = "id",all.x = TRUE,sort = FALSE,suffixes = c("","4"))
  # if(missing(col_select)){
  #   if(select_custom_values == TRUE){
  #     col_select <- factor(x =      c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","department_initials","natureza_despesa","limite_orcamento","previsao_orcamento","validacao","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"),
  #                          levels = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","department_initials","natureza_despesa","limite_orcamento","previsao_orcamento","validacao","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"))
  #   }else{
  #     col_select <- factor(x =      c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"),
  #                          levels = c("id","name_project","year","name","subject","description","created_on","updated_on","start_date","due_date","done_ratio","name_status","correct_status","deadline_status","assign","author","setor","department","parent_id","parent_id0","parent_id1","name1","subject1","parent_id2","name2","subject2","parent_id3","name3","subject3","parent_id4","name4","subject4"))
  #     }
  #   }
  # issues <- issues[,col_select]
  if(!missing(col_names)){
    colnames(issues) <- c(col_names)
  }
  issues$date_extract <- Sys.time()
  issues$date_update <- update_dbi
  poolReturn(conn)
  poolClose(connect)
  return(issues)
  }

aca_sit_ger <- function(database,department,year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db0 <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = ifelse(substr(department,1,1)=="C","Campus","Reitoria"),Unidade = department,Tipo = correct_status) %>%
      summarise(Valor= n())
    db1 <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = "IFB",Unidade = "IFB",Tipo = correct_status) %>%
      summarise(Valor= n())
    db <- bind_rows(db0,db1)
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
      group_by(Grupo = ifelse(substr(department,1,1)=="C","Campus","Reitoria"),Unidade = department,Tipo = correct_status) %>%
      summarise(Valor= n())
    }
  return(db)
  }

aca_sit_res <- function(database,department,type,year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    if(type == "Envolvido"){
      db0 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
      db1 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
      db <- bind_rows(db0,db1)
    }else if(type == "Setor"){
      db0 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
      db1 <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
      db <- bind_rows(db0,db1)
    }
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    if(type == "Envolvido"){
      db <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Envolvido",Unidade = department,Responsavel = assign,Tipo = correct_status) %>%
        summarise(Valor= n())
    }else if(type == "Setor"){
      db <- database %>%
        filter(name == "Acao" & name1 == "Indicador",department %in% d, year %in% y) %>%
        group_by(Grupo = "Setor",Unidade = department,Responsavel = department_initials,Tipo = correct_status) %>%
        summarise(Valor= n())
    }
  }
  return(db)
}

ind_sit_ger <- function(database,department,col_select,year){
  if(missing(database)){
    if(exists("sgi",where = 1)){
      database <- sgi
    }else{
      message("missing 'database' input")
    }
  }
  if(missing(department)){
    d <- unique(database$department)
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name1 == "Indicador",name == "Indicador",year %in% y) %>%
      group_by(Indicador = subject, Cod_Indicador = substr(subject,1,5), Percentual = done_ratio) %>%
      summarise()
  }else{
    d <- department
    if(missing(year)){
      y <- unique(database$year)
    }else{
      y <- year
    }
    db <- database %>%
      filter(name1 == "Indicador",name == "Indicador",department %in% d,year %in% y) %>%
      group_by(Indicador = subject, Cod_Indicador = substr(subject,1,5), Percentual = done_ratio) %>%
      summarise()
  }
  if(!missing(col_select)){
    db <- db[,c(col_select)]
  }
  return(db)
}

