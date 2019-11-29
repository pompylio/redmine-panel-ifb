# LIBRARY ---------------------------------------------------------------------------------------------------------
suppressMessages(require(tidyr))
suppressMessages(require(dplyr))
suppressMessages(require(DT))
suppressMessages(require(rCharts))
suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(shinyjs))
suppressMessages(require(stringr))
suppressMessages(require(DBI))
suppressMessages(require(RMySQL))
suppressMessages(require(pool))
suppressMessages(require(reshape2))
# SHINY DATABASE --------------------------------------------------------------------------------------------------
source("security/key_redmine.R",encoding = "UTF-8")
bsc   <- readRDS("data/bsc.rds")
bsc_meta <- bsc[, c("Cod_Indicador", "Meta_2014", "Meta_2015", "Meta_2016", "Meta_2017", "Meta_2018")]
bsc_meta <- melt(data = bsc_meta, 
                 id.vars = "Cod_Indicador", 
                 measure.vars = c("Meta_2014", "Meta_2015", "Meta_2016", "Meta_2017", "Meta_2018"), 
                 variable.name = "ano", 
                 value.name = "meta", na.rm = FALSE)
bsc_meta$ano <- substr(bsc_meta$ano, 6, 9)
bsc_meta$meta <- as.numeric(gsub("[^\\d]+", "", bsc_meta$meta, perl=TRUE))
bsc_meta[is.na(bsc_meta$meta), ]$meta <- 0
colnames(bsc_meta) <- c("indicador", "ano","meta")
siafi <- readRDS("data/siafi.rds")
version <- readRDS("data/version.rds")
dt_ind <- c("i111", "i112", "i121", "i122", "i123", "i124", "i131", "i132", "i133", "i211", 
              "i213", "i214", "i311", "i312", "i313", "i314", "i321", "i331", "i332", "i333", 
              "i341", "i342", "i343", "i351", "i361", "i362", "i363", "i371", "i372", "i373", 
              "i381", "i382", "i383", "i411", "i412", "i413", "i414", "i415", "i421", "i422", 
              "i441", "i442", "i443", "ind_pdi")
for(i in 1:length(dt_ind)){
  assign(dt_ind[i] ,readRDS(file = paste0("data/",dt_ind[i],".rds")))
}
project_app <- list(planejamento = c("640","652","653","654","655","656","657","658","659","660","662","663","664","665","666","667","668","669","670",
                                     "671","697","702","703","704","705","706","707","708","709","710","711","712","713","714","715","716","717","718",
                                     "719","720","755","756","757","758","759","760","761","762","763","764","765","766","767","768","769","770",
                                     "771","772","773","774",
                                     as.character(seq(from = 779,to = 798))))
connect <- dbPool(drv = RMySQL::MySQL(), dbname = kpass$dbname, host = kpass$host,username = kpass$username,password = kpass$password,idleTimeout = 3600000)
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
custom_values <- merge(x = custom_values,y = custom_fields[,c("id","name")],by.x = "custom_field_id",by.y = "id",all.x = TRUE,sort = FALSE)
custom_values <- custom_values[custom_values$custom_field_id %in% c(120,121,122,147),c("custom_field_id","customized_id","name","value")]
custom_values[custom_values$custom_field_id %in% c(120,121,122),]$name <- "Setor"
custom_values[custom_values$custom_field_id %in% c(147),]$name <- "Validacao"
custom_values <- custom_values[,-c(1)]
custom_values1 <- custom_values[custom_values$name == "Setor",]
custom_values1 <- spread(data = custom_values1,key = name,value = value,fill = "")
custom_values5 <- custom_values[custom_values$name == "Validacao",]
custom_values5 <- spread(data = custom_values5,key = name,value = value,fill = "")
custom_values <- merge(x = custom_values1,y = custom_values5,by.x = "customized_id",by.y = "customized_id",all = TRUE)
issues <- issues[issues$project_id %in% c(project_app$planejamento,project_app$orcamento),]
issues <- merge(x = issues,y = trackers[,c("id","name")],by.x = "tracker_id",by.y = "id",all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = projects[,c("id","name","status")],by.x = "project_id",by.y = "id",suffixes = c("","_project"),all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = projects[,c("id","name")],by.x = "tracker_id",by.y = "id",suffix = c("","_project_parent"),all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = issue_statuses[,c("id","name")],by.x = "status_id",by.y = "id",suffix = c("","_status"),all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "assigned_to_id",by.y = "id",all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = users[,c("id","firstname","lastname")],by.x = "author_id",by.y = "id",suffix = c("","_author"),all.x = TRUE,sort = FALSE)
issues <- merge(x = issues,y = custom_values,by.x = "id",by.y = "customized_id",all.x = TRUE,sort = FALSE)
issues$name <- str_replace_all(issues$name,c("[ç]"="c","[ã]"="a","[õ]"="o","[á]"="a","[é]"="e","[í]"="i","[ó]"="o","[ú]"="u"))
issues$Situacao_Corrigida <- ifelse(issues$status_id == 5,"Fechada",ifelse(issues$done_ratio == 100,"Finalizada",ifelse(issues$done_ratio == 0,"Nao_Iniciada","Iniciada")))
issues$Situacao_Prazo <- ifelse(is.na(issues$due_date),"Sem_Data",
                         ifelse(issues$Situacao_Corrigida == "Finalizada","Concluida",
                         ifelse(issues$Situacao_Corrigida == "Iniciada" &     as.numeric(substr(Sys.Date(),1,4)) <= as.numeric(substr(issues$start_date,1,4)) & issues$due_date >= Sys.Date(),"No_prazo",
                         ifelse(issues$Situacao_Corrigida == "Iniciada" &     as.numeric(substr(Sys.Date(),1,4)) >  as.numeric(substr(issues$start_date,1,4)),"Em_atraso",
                         ifelse(issues$Situacao_Corrigida == "Iniciada" &     as.numeric(substr(Sys.Date(),1,4)) <= as.numeric(substr(issues$start_date,1,4)) & issues$due_date < Sys.Date(),"Em_atraso",
                         ifelse(issues$Situacao_Corrigida == "Nao_Iniciada" & as.numeric(substr(Sys.Date(),1,4)) <= as.numeric(substr(issues$start_date,1,4)) & issues$due_date >= Sys.Date() & issues$start_date >= Sys.Date(),"No_prazo",
                         ifelse(issues$Situacao_Corrigida == "Nao_Iniciada" & as.numeric(substr(Sys.Date(),1,4)) >  as.numeric(substr(issues$start_date,1,4)),"Em_atraso",
                         ifelse(issues$Situacao_Corrigida == "Nao_Iniciada" & as.numeric(substr(Sys.Date(),1,4)) <= as.numeric(substr(issues$start_date,1,4)) & issues$due_date < Sys.Date(),"Em_atraso",
                         ifelse(issues$Situacao_Corrigida == "Nao_Iniciada" & as.numeric(substr(Sys.Date(),1,4)) <= as.numeric(substr(issues$start_date,1,4)) & issues$due_date >= Sys.Date() & issues$start_date <= Sys.Date(),"Em_atraso","Sem_Definicao")))))))))
issues$Ano <- str_extract(string = issues$name_project, pattern = "\\d+\\.\\d|\\d+")
issues$Unidade <- ifelse(issues$project_id %in% unique(issues[grep(pattern = c("[(]"),x = issues$name_project),]$project_id),as.character(substr(issues$name_project,unlist(gregexpr(pattern = "[(]",text = issues$name_project))+1,unlist(gregexpr(pattern = "[)]",text = issues$name_project))-1)),"")
issues$Setor_Sigla <- ifelse(is.na(issues$Setor)|issues$Setor == "","Nao_Informado",as.character(substr(x = issues$Setor,start = as.numeric(gregexpr(pattern = "[(]",issues$Setor))+1,stop = as.numeric(gregexpr(pattern = "[)]",issues$Setor))-1)))
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
issues <- issues[,c("id","subject","name","Ano","Unidade","project_id","name_project","name_status","Situacao_Corrigida","Situacao_Prazo","assign","updated_on","start_date","due_date","done_ratio","parent_id","parent_id1","subject1","name1","parent_id2","subject2","name2","parent_id3","subject3","name3","parent_id4","subject4","name4","Setor","Setor_Sigla","Validacao")]
names(issues) <- c("Id","Titulo","Tipo","Ano","Unidade","Id_Projeto","Projeto","Situacao","Situacao_Corrigida","Situacao_Prazo","Atribuido_para","Atualizado_em","Inicio","Data_prevista","Perc_Terminado","Cod_Tarefa_Pai","Cod_Tarefa_Pai1","Titulo1","Tipo1","Cod_Tarefa_Pai2","Titulo2","Tipo2","Cod_Tarefa_Pai3","Titulo3","Tipo3","Cod_Tarefa_Pai4","Titulo4","Tipo4","Setor","Setor_Sigla","Validacao")
sgi <- issues
poolReturn(conn)
onStop(function() {poolClose(connect)})
vals <- reactiveValues(count=0)
# FUNCTIONS -------------------------------------------------------------------------------------------------------
taskitembox <- function(title,subtitle,value,description,color,icon){
  stylecolor <- ifelse(color == "blue","info-box bg-aqua",ifelse(color == "green","info-box bg-green",ifelse(color == "yellow","info-box bg-yellow",ifelse(color == "red","info-box bg-red"))))
  stylevalue <- paste0("width: ",value,"%")
  tags$div(class = "shiny-html-output col-sm-3 shiny-bound-output",
           tags$div(class = stylecolor,
                    tags$span(class = "info-box-icon",tags$i(class = icon)),
                    tags$div(class = "info-box-content",
                             tags$span(class = "info-box-text",title),
                             tags$span(class = "info-box-number",subtitle),
                             tags$div(class = "progress",
                                      tags$div(class = "progress-bar",style = stylevalue)),
                             tags$span(class = "progress-description",description))))
}
calloutbox <- function(status,title,paragraph){
  stylestatus <- ifelse(status=="danger","callout callout-danger",
                        ifelse(status=="info","callout callout-info",
                               ifelse(status=="warning","callout callout-warning",
                                      ifelse(status=="success","callout callout-success"))))
  tags$div(class = stylestatus,
           tags$h4(title),
           tags$p(paragraph))
}
barprogress <- function(title,type,font_color,height,value,position_value){
  tags$div(class = "row",
           style = "margin-right: 0px; margin-left: 0px",
           tags$h5(title),
           tags$div(class = "progress",style = paste0("height: ",if(missing(height)){"18px"}else{height}),
                    tags$div(class = if(missing(type)){"progress-bar"}else{paste0("progress-bar progress-bar-",type)},
                             role = "progressbar",
                             tags$area(valuenow = if(missing(value)){"0"}else{value},valuemin = 0,valuemax = 100),
                             style = paste0("width: ",if(missing(value)){"0"}else{value},"%;",
                                            "text-align: ",if(missing(position_value)){"center"}else{position_value},";",
                                            "color: ",if(value %in% c("0","1")){"#000"}else if(missing(font_color)){"#fff"}else{font_color},";",
                                            "height: ",if(missing(height)){"18px"}else{height}),
                             HTML(paste0(if(missing(value)){"0"}else{value},"%")))))
}
plot_rChart_ifb <- function(id, fonte, origem, lim_eixo_y){
  if (missing(lim_eixo_y)) lim <- FALSE
  else                     lim <- TRUE
  db <- fonte %>% filter(sigla_unidade == "IFB") %>% group_by(indicador, ano, meta, resultado) %>% summarise() 
  if (nrow(db) < 5){
    db <- left_join(x = bsc_meta[bsc_meta$indicador == unique(db$indicador), ], y = db, by = c("indicador", "ano"), suffix = c("", "_resultado"))
    if (any(is.na(db$resultado))) db[is.na(db$resultado), ]$resultado <- 0
  }
  plot <- renderChart2({
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    h1$credits(enabled = TRUE, text = paste0("Fonte: ", origem))
    h1$series(name = "Meta", data = db$meta)
    h1$series(name = "Resultado", data = db$resultado)
    h1$xAxis(categories = db$ano)
    if (lim) h1$yAxis(title="", stackLabels = list(enabled = TRUE), min = lim_eixo_y[["min"]], max = lim_eixo_y[["max"]])
    else     h1$yAxis(title="", stackLabels = list(enabled = TRUE))
    h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>')
    h1$plotOptions(column = list(dataLabels = list(enabled = F), allowPointSelect = T, borderRadius = 4))
    h1$set(width = get0(paste0("session$clientData$",id)))
    return(h1)
  })
  return(plot)
}
plot_rChart_campi <- function(id, fonte, origem){
  db <- fonte %>% filter(!sigla_unidade == "IFB")
  dt <- tibble(
    indicador = unique(db$indicador),
    ano = rep(c("2014", "2015", "2016", "2017", "2018"), each = 10),
    meta = rep(bsc_meta[bsc_meta$indicador == unique(db$indicador), ]$meta, each = 10),
    sigla_unidade = rep(c("CBRA", "CCEI", "CEST", "CGAM", "CPLA", "CRFI", "CSAM", "CSSB", "CTAG", "CTGC"), 5))
  db <- left_join(x = dt, y = db[, c("indicador", "ano", "sigla_unidade", "resultado")], by = c("indicador", "ano", "sigla_unidade"))
  if(any(is.na(db$resultado))) db[is.na(db$resultado), ]$resultado <- 0
  db <- db[, c("indicador", "ano", "sigla_unidade", "meta", "resultado")]
  meta <- bsc_meta[bsc_meta$indicador == unique(db$indicador), ]$meta
  plot <- renderChart2({
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    h1$credits(enabled = TRUE, text = paste0("Fonte: ", origem), href = "http://dashboard.drpo.ifb.local/")
    h1$xAxis(categories = unique(db$ano))
    h1$yAxis(title="", stackLabels = list(enabled = TRUE))
    h1$series(name = "META", data = meta, type = "areaspline", borderColor = "#f0f2f4", color = "#f0f2f4", pointWidth = 20)
    h1$series(name = "CBRA", data = db[db$sigla_unidade == "CBRA", ]$resultado)#, color = "#ffffff")
    h1$series(name = "CCEI", data = db[db$sigla_unidade == "CCEI", ]$resultado)#, color = "#181818")
    h1$series(name = "CEST", data = db[db$sigla_unidade == "CEST", ]$resultado)#, color = "#303030")
    h1$series(name = "CGAM", data = db[db$sigla_unidade == "CGAM", ]$resultado)#, color = "#484848")
    h1$series(name = "CPLA", data = db[db$sigla_unidade == "CPLA", ]$resultado)#, color = "#606060")
    h1$series(name = "CRFI", data = db[db$sigla_unidade == "CRFI", ]$resultado)#, color = "#707070")
    h1$series(name = "CSAM", data = db[db$sigla_unidade == "CSAM", ]$resultado)#, color = "#888888")
    h1$series(name = "CSSB", data = db[db$sigla_unidade == "CSSB", ]$resultado)#, color = "#A0A0A0")
    h1$series(name = "CTAG", data = db[db$sigla_unidade == "CTAG", ]$resultado)#, color = "#B0B0B0")
    h1$series(name = "CTGC", data = db[db$sigla_unidade == "CTGC", ]$resultado)#, color = "#C0C0C0")
    h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>')
    h1$plotOptions(column = list(dataLabels = list(enabled = F), allowPointSelect = T, borderRadius = 4))
    h1$set(width = get0(paste0("session$clientData$",id)))
    return(h1)
  })
  return(plot)
}
colorchart <- list(SGISitTarefa = list(Finalizada = "#0d1a26",Iniciada = "#336699",NaoIniciada = "#8cb3d9",Fechada = "#d4d9de"),SGISitPrazo  = list(Concluida = "#06130d",NoPrazo = "#206040",EmAtraso = "#40bf80",SemData = "#b3e6cc"),SGISitValor = list(Geral = "#336699",Borda = "#4080bf"),SGIExeOrc  = list(Acao = "#0d1a26",Categoria = "#336699",Despesa = "#8cb3d9"))
# SHINY UI --------------------------------------------------------------------------------------------------------
ui <-
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "DASHBOARD | DRPO",
      titleWidth = 240),
    dashboardSidebar(
      width = 240,
      collapsed = FALSE,
      sidebarMenu(
        menuItem(
          text = strong("PLANEJAMENTO"),
          tabName = 'menu1',
          startExpanded = TRUE,
          icon = icon("map-signs"),
          menuSubItem(
            text = strong("ESTRATÉGIA"),
            icon = icon("caret-right"),
            tabName = "menu1sub0",
            selected = TRUE),
          menuSubItem(
            text = strong("INDICADORES"),
            icon = icon("caret-right"),
            tabName = "menu1sub1"),
          menuItem(
            text = strong("PLANOS DE AÇÃO"),
            icon = icon("caret-right"),
            tabName = "menu1sub2",
            startExpanded = FALSE,
            menuSubItem(
              text = strong("Geral"),
              icon = icon("angle-right"),
              tabName = "menu1sub2sub1"),
            menuSubItem(
              text = strong("Unidade"),
              icon = icon("angle-right"),
              tabName = "menu1sub2sub2")
          ),
          menuItem(
            text = strong("RESULTADO PDI"),
            icon = icon("caret-right"),
            tabName = 'menu1sub3',
            startExpanded = FALSE,
            menuItem(
              text = strong("Resultados"),
              icon = icon("angle-right"),
              tabName = "menu1sub3sub1",
              badgeLabel = "P1", 
              badgeColor = "red"), 
            menuItem(
              text = strong("Sociedade"),
              icon = icon("angle-right"),
              tabName = "menu1sub3sub2",
              badgeLabel = "P2", 
              badgeColor = "yellow"),
            menuItem(
              text = strong("Processos Internos"),
              icon = icon("angle-right"),
              tabName = "menu1sub3sub3",
              badgeLabel = "P3", 
              badgeColor = "green"),
            menuItem(
              text = strong("Pessoas e Tecnologia"),
              icon = icon("angle-right"),
              tabName = "menu1sub3sub4",
              badgeLabel = "P4", 
              badgeColor = "blue")
            )
          )
        )
      ),
    dashboardBody(
      tags$head(
        tags$head(HTML(
          "<!-- Global site tag (gtag.js) - Google Analytics -->
          <script async src='https://www.googletagmanager.com/gtag/js?id=UA-103778910-2'></script>
          <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

          gtag('config', 'UA-103778910-2');
          </script>"
        ))
      ),
      tags$head(tags$style(HTML(".box-body {padding: 20px;}"))),
      tabItems(
        tabItem(
          tabName = 'menu1sub0',
          fluidRow(
            column(
              width = 12,
              column(
                width = 12,
                selectInput(
                  inputId = "year0",
                  label = "Ano:",
                  choices = unique(sgi$Ano),
                  selected = "2019.2")),
              column(
                width = 12,
                tags$p(tags$h4(strong("PERSPECTIVAS"))),br())),
            column(
              width = 12,
              column(width = 3, htmlOutput("box.per.res")),
              column(width = 3, htmlOutput("box.per.pro")),
              column(width = 3, htmlOutput("box.per.pes")),
              column(width = 3, htmlOutput("box.per.soc"))),
            column(
              width = 12,
              column(width = 12,br(),tags$p(tags$h4(strong("OBJETIVOS ESTRATÉGICOS"))),br())),
            column(
              width = 12,
              uiOutput("box.obj.res"), uiOutput("box.obj.pro")),
            column(
              width = 12,
              uiOutput("box.obj.pes"), uiOutput("box.obj.soc"))
          )
        ),
        tabItem(
          tabName = "menu1sub1",
          fluidRow(
            column(
              width = 12,
              column(
                width = 12,
                selectInput(
                  inputId = "year1",
                  label = "Ano:",
                  choices = unique(sgi$Ano),
                  selected = "2019.2")),
              column(
                width = 6,
                uiOutput("menuperspectiva")),
                # selectInput(
                #   inputId = "input0menu1sub1",
                #   label = "Perspectiva do BSC",
                #   choices = "",
                #   multiple = TRUE,
                #   width = "100%")),
              column(
                width = 4,
                selectInput(
                  inputId = "input1menu1sub1",
                  label = "Tipo de Resultado",
                  choices = c("Quantidade", "Percentual"),
                  selected = "Quantidade"))),
            column(width = 12,br()),
            column(
              width = 12,
              box(
                width = 12,
                title = "Indicadores por Perspectiva",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("ind.com.ger", "highcharts"))),
            column(
              width = 12,
              br(),
              column(
                width = 12,
                uiOutput("menuindicador")
                # selectInput(
                #   inputId = 'input3menu1sub1',
                #   label = "Indicador",
                #   choices = c(sort(unique(sgi$Titulo[sgi$Tipo == "Indicador" & sgi$Situacao %in% c("Nova","Em andamento","Resolvida")]))),
                #   selected = "",
                #   width = "100%"))
              )),
            column(width = 12,br()),
            column(
              width = 12,
              box(
                width = 6,
                title = "Indicador por Unidade e Execução",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("ind.exe.per", "highcharts")),
              box(
                width = 6,
                title = "Indicador por Unidade e Situação",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("ind.sit.uni", "highcharts")))
          )
        ),
        tabItem(
          tabName = 'menu1sub2sub1',
          fluidRow(
            column(
              width = 12,
              selectInput(
                inputId = "year2",
                label = "Ano:",
                choices = unique(sgi$Ano),
                selected = "2019.2")),
            box(
              width = 3,
              title = "Por Situação",
              solidHeader = TRUE,
              collapsible = TRUE,
              showOutput("aca.sit.ger", "highcharts")),
            box(
              width = 9,
              title = "Por Situação e Unidade",
              solidHeader = TRUE,
              collapsible = TRUE,
              showOutput("aca.sit.com", "highcharts")),
            box(
              width = 3,
              title = "Por Prazo",
              solidHeader = TRUE,
              collapsible = TRUE,
              showOutput("aca.pra.ger", "highcharts")),
            box(
              width = 9,
              title = "Por Prazo e Unidade",
              solidHeader = TRUE,
              collapsible = TRUE,
              showOutput("aca.pra.com", "highcharts"))
          )
        ),
        tabItem(
          tabName = "menu1sub2sub2",
          fluidRow(
            column(
              width = 12,
              column(
                width = 4,
                selectInput(
                  inputId = "year3",
                  label = "Ano:",
                  choices = unique(sgi$Ano),
                  selected = "2019.2")),
              column(
                width = 4,
                selectInput(
                  inputId = 'input0menu1sub2sub2',
                  label = "Unidade",
                  choices = c("CBRA","CCEI","CEST","CGAM","CPLA","CREM","CRFI","CSAM","CSSB","CTAG","DICOM","DRPO","DTIC",
                              "GAB-RIFB","PRAD","PREN","PREX","PRGP","PRPI"),
                  selected = "CBRA"))),
            column(
              width = 12,
              br()),
            column(
              width = 12,
              box(
                width = 6,
                title = "Por Situação",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.sit.uni", "highcharts")),
              box(
                width = 6,
                title = "Por Prazo",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.pra.uni", "highcharts"))),
            column(
              width = 12,
              box(
                width = 6,
                title = "Por Situação e Setor",
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.sit.set", "highcharts")),
              box(
                title = "Por Prazo e Setor",
                width = 6,
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.pra.set", "highcharts"))),
            column(
              width = 12,
              box(
                title = "Por Situação e Envolvido",
                width = 6,
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.sit.env", "highcharts")),
              box(
                title = "Por Prazo e Envolvido",
                width = 6,
                solidHeader = TRUE,
                collapsible = TRUE,
                showOutput("aca.pra.env", "highcharts"))),
            column(
              width = 12,
              box(
                title = "Plano de Ação",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("tab.aca.env")))
          )
        ),
         tabItem(
           tabName = "menu2sub2",
           fluidRow(
             column(
               width = 12,
               column(
                 width = 2,
                 selectInput(
                   width = "100%",
                   inputId = 'input0menu2sub2',
                   label = "Ano",
                   choices = unique(sgi$Ano),
                   selected = "2019.2")),
               column(
                 width = 2,
                 selectInput(
                   width = "100%",
                   inputId = 'input1menu2sub2',
                   label = "Unidade",
                   choices = c("IFB","CBRA","CCEI","CEST","CGAM","CPLA","CREM","CRFI","CSAM","CSSB",
                               "CTAG","REITORIA"),
                   selected = "IFB"))
               ),
             column(
               width = 12,
               box(
                 title = "Execução Orçamentária por Ação Orçamentária",
                 width = 12,
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 showOutput("orc.exe.aca", "highcharts"))
             ),
             column(
               width = 12,
               box(
                 title = "Execução Orçamentária por Categoria de Gasto",
                 width = 12,
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 selectInput(
                   width = "100%",
                   inputId = 'input2menu2sub2',
                   label = "Ação Orçamentária",
                   choices = c("20RG EXPANSÃO E REESTRUTURAÇÃO" = "20RG","20RL FUNCIONAMENTO DA INSTITUIÇÃO" = "20RL",
                               "2994 ASSISTÊNCIA AOS ESTUDANTES" = "2994","4572 CAPACITAÇÃO DE SERVIDORES" = "4572"),
                   selected = "20RL"),
                 showOutput("orc.exe.cat", "highcharts")
                 )
             )
             )
           ),
         tabItem(
           tabName = "menu2sub3",
           fluidRow(
             column(
               width = 12,
               column(
                 width = 3,
                 selectInput(
                   width = "100%",
                   inputId = 'input2menu2sub3',
                   label = "Unidade",
                   choices = c("IFB","CBRA","CCEI","CEST","CGAM","CPLA","CREM","CRFI","CSAM","CSSB",
                               "CTAG","REITORIA"),
                   selected = "IFB"))
               ),
             column(
               width = 12,
               box(
                 width = 6,
                 title = "Dotação por Exercício",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 showOutput("orc.exe.exe","highcharts")),
               box(
                 width = 6,
                 title = "Pago por Exercício",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 showOutput("orc.exe.pag","highcharts")))
             )
           ),
        tabItem(
          tabName = "menu1sub3sub1",
          fluidRow(
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[1], solidHeader = TRUE, collapsible = TRUE, showOutput("i111", "highcharts")),
              box(width = 6, title = bsc$Indicador[2], solidHeader = TRUE, collapsible = TRUE, showOutput("i112", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[3], solidHeader = TRUE, collapsible = TRUE, showOutput("i121", "highcharts")),
              box(width = 6, title = bsc$Indicador[4], solidHeader = TRUE, collapsible = TRUE, showOutput("i122", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[5], solidHeader = TRUE, collapsible = TRUE, showOutput("i123", "highcharts")),
              box(width = 6, title = bsc$Indicador[6], solidHeader = TRUE, collapsible = TRUE, showOutput("i124", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[7], solidHeader = TRUE, collapsible = TRUE, showOutput("i131", "highcharts")),
              box(width = 6, title = bsc$Indicador[8], solidHeader = TRUE, collapsible = TRUE, showOutput("i132", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[9], solidHeader = TRUE, collapsible = TRUE, showOutput("i133", "highcharts"))))
          ),
        tabItem(
          tabName = "menu1sub3sub2",
          fluidRow(
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[10], solidHeader = TRUE, collapsible = TRUE, showOutput("i211", "highcharts")),
              #box(width = 6, title = bsc$Indicador[11], solidHeader = TRUE, collapsible = TRUE, showOutput("i212", "highcharts")),
              box(width = 6, title = bsc$Indicador[12], solidHeader = TRUE, collapsible = TRUE, showOutput("i213", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[13], solidHeader = TRUE, collapsible = TRUE, showOutput("i214", "highcharts"))))
              #box(width = 6, title = bsc$Indicador[14], solidHeader = TRUE, collapsible = TRUE, showOutput("i215", "highcharts")),
          ),
        tabItem(
          tabName = "menu1sub3sub3",
          fluidRow(
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[15], solidHeader = TRUE, collapsible = TRUE, showOutput("i311", "highcharts")),
              box(width = 6, title = bsc$Indicador[16], solidHeader = TRUE, collapsible = TRUE, showOutput("i312", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[17], solidHeader = TRUE, collapsible = TRUE, showOutput("i313", "highcharts")),
              box(width = 6, title = bsc$Indicador[18], solidHeader = TRUE, collapsible = TRUE, showOutput("i314", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[19], solidHeader = TRUE, collapsible = TRUE, showOutput("i321", "highcharts")),
              box(width = 6, title = bsc$Indicador[20], solidHeader = TRUE, collapsible = TRUE, showOutput("i331", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[21], solidHeader = TRUE, collapsible = TRUE, showOutput("i332", "highcharts")),
              box(width = 6, title = bsc$Indicador[22], solidHeader = TRUE, collapsible = TRUE, showOutput("i333", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[23], solidHeader = TRUE, collapsible = TRUE, showOutput("i341", "highcharts")),
              box(width = 6, title = bsc$Indicador[24], solidHeader = TRUE, collapsible = TRUE, showOutput("i342", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[25], solidHeader = TRUE, collapsible = TRUE, showOutput("i343", "highcharts")),
              box(width = 6, title = bsc$Indicador[26], solidHeader = TRUE, collapsible = TRUE, showOutput("i351", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[27], solidHeader = TRUE, collapsible = TRUE, showOutput("i361", "highcharts")),
              box(width = 6, title = bsc$Indicador[28], solidHeader = TRUE, collapsible = TRUE, showOutput("i362", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[29], solidHeader = TRUE, collapsible = TRUE, showOutput("i363", "highcharts")),
              box(width = 6, title = bsc$Indicador[30], solidHeader = TRUE, collapsible = TRUE, showOutput("i371", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[31], solidHeader = TRUE, collapsible = TRUE, showOutput("i372", "highcharts")),
              box(width = 6, title = bsc$Indicador[32], solidHeader = TRUE, collapsible = TRUE, showOutput("i373", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[33], solidHeader = TRUE, collapsible = TRUE, 
                  p("Conforme Relatório de Gestão 2017, 'esse é um dos indicadores do Termo de Acordos e Metas e Compromissos (TAM). 
                     Para não gerar conflito de resultado com o previsto no TAM e com o manual para cálculo dos indicadores de 
                     gestão da RFEPCT e para não apresentar resultado sem sentido, utilizando o cálculo previsto pelo manual,
                     o índice de eficácia do PDI não foi mensurado neste exercício'. Para consultar o resultado, acesse: 
                     http://ifbemnumeros.ifb.edu.br/")),
              box(width = 6, title = bsc$Indicador[34], solidHeader = TRUE, collapsible = TRUE, showOutput("i382", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[35], solidHeader = TRUE, collapsible = TRUE, showOutput("i383", "highcharts"))))),
        tabItem(
          tabName = "menu1sub3sub4",
          fluidRow(
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[36], solidHeader = TRUE, collapsible = TRUE, showOutput("i411", "highcharts")),
              box(width = 6, title = bsc$Indicador[37], solidHeader = TRUE, collapsible = TRUE, showOutput("i412", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[38], solidHeader = TRUE, collapsible = TRUE, showOutput("i413", "highcharts")),
              box(width = 6, title = bsc$Indicador[39], solidHeader = TRUE, collapsible = TRUE, showOutput("i414", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[40], solidHeader = TRUE, collapsible = TRUE, showOutput("i415", "highcharts")),
              box(width = 6, title = bsc$Indicador[41], solidHeader = TRUE, collapsible = TRUE, showOutput("i421", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[42], solidHeader = TRUE, collapsible = TRUE, showOutput("i422", "highcharts")),
              box(width = 6, title = bsc$Indicador[43], solidHeader = TRUE, collapsible = TRUE, showOutput("i431", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[44], solidHeader = TRUE, collapsible = TRUE, showOutput("i432", "highcharts")),
              box(width = 6, title = bsc$Indicador[45], solidHeader = TRUE, collapsible = TRUE, showOutput("i433", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[46], solidHeader = TRUE, collapsible = TRUE, showOutput("i441", "highcharts")),
              box(width = 6, title = bsc$Indicador[47], solidHeader = TRUE, collapsible = TRUE, showOutput("i442", "highcharts"))),
            column(
              width = 12,
              box(width = 6, title = bsc$Indicador[48], solidHeader = TRUE, collapsible = TRUE, showOutput("i443", "highcharts")))
            )
          )
        ),
      tags$a(href="https://github.com/pompylio/redmine-panel-ifb", target="_new", icon("github"), style="font-size:25px;color:black"),
      tags$p("cgpl@ifb.edu.br | ", update_dbi, " | ", tags$a(href = "http://sgi.prdi.ifb.edu.br/login", target = "_blank", "SGI"), " e ",
             tags$a(href = "http://sistec.mec.gov.br/login/login", target = "_blank","SISTEC"))
      )
    )

# SHINY SERVER ----------------------------------------------------------------------------------------------------
server <-
  function(input, output, session) {
    dbboxind <- reactive({
      bd <- sgi %>%
        filter(Tipo == "Perspectiva",Ano == as.character(input$year0),Id_Projeto %in% project_app$planejamento) %>%
        group_by("Perspectiva" = str_trim(substr(Titulo,3,nchar(Titulo))),Tipo = "Percentual","Valor" = Perc_Terminado) %>%
        summarise()
      bd1 <- sgi %>%
        filter(Tipo == "Acao",Tipo1 == "Indicador",Ano == as.character(input$year0),Id_Projeto %in% project_app$planejamento) %>%
        group_by("Perspectiva" = str_trim(substr(Titulo4,3,nchar(Titulo4))),Tipo = "Quantidade") %>%
        summarise(Valor = n())
      bd <- bind_rows(bd, bd1)
      bd <- spread(data = bd,key = Tipo,value = Valor,fill ="")
      bd
      })
    output$box.per.res <- renderInfoBox({
      db <- dbboxind()[dbboxind()$Perspectiva == "Resultados", ]
      taskitembox(title = db$Perspectiva,
                  subtitle = paste0(db$Percentual,"%"),
                  value = db$Percentual,
                  description= paste("% de execução das", db$Quantidade,"ações"),
                  color = "red",
                  icon = "fa fa-line-chart")
    })
    output$box.per.pro <- renderInfoBox({
      db <- dbboxind()[dbboxind()$Perspectiva == "Processos Internos", ]
      taskitembox(title = db$Perspectiva,
                  subtitle = paste0(db$Percentual,"%"),
                  value = db$Percentual,
                  description= paste("% de execução das", db$Quantidade,"ações"),
                  color = "green",
                  icon = "fa fa-tasks")
    })
    output$box.per.pes <- renderInfoBox({
      db <- dbboxind()[dbboxind()$Perspectiva == "Pessoas e Tecnologia", ]
      taskitembox(title = db$Perspectiva,
                  subtitle = paste0(db$Percentual,"%"),
                  value = db$Percentual,
                  description= paste("% de execução das", db$Quantidade,"ações"),
                  color = "blue",
                  icon = "fa fa-wrench")
    })
    observe({
      if(any(dbboxind()$Perspectiva == "Sociedade")){
        output$box.per.soc <- renderInfoBox({
          db <- dbboxind()[dbboxind()$Perspectiva == "Sociedade", ]
          taskitembox(title = db$Perspectiva,
                      subtitle = paste0(db$Percentual,"%"),
                      value = db$Percentual,
                      description= paste("% de execução das", db$Quantidade,"ações"),
                      color = "yellow",
                      icon = "fa fa-users")
        })
      } else {
        output$box.per.soc <- renderText("")
      }
    })
    
    dbboxobj <- reactive({
      bd <- sgi %>%
        filter(Tipo == "Objetivo",Ano == as.character(input$year0),Id_Projeto %in% project_app$planejamento) %>%
        group_by("Objetivo" = Titulo, "Percentual" = Perc_Terminado) %>%
        summarise()
    })
    output$box.obj.res <- renderUI({
      type_color <- "danger"
      if(input$year0 %in% c("2017", "2018", "2019")){
        box <- box(
          width = 6,
          title = strong("RESULTADOS"),
          solidHeader = FALSE,
          status = type_color,
          collapsible = TRUE,
          collapsed = FALSE,
          barprogress(title = dbboxobj()$Objetivo[1],type = type_color,
                      value = dbboxobj()$Percentual[1]),
          barprogress(title = dbboxobj()$Objetivo[2],type = type_color,
                      value = dbboxobj()$Percentual[2]),
          barprogress(title = dbboxobj()$Objetivo[3],type = type_color,
                      value = dbboxobj()$Percentual[3]))
      } else {
        box <- box(
          width = 6,
          title = strong("RESULTADOS"),
          solidHeader = FALSE,
          status = type_color,
          collapsible = TRUE,
          collapsed = FALSE,
          barprogress(title = dbboxobj()$Objetivo[1],type = type_color,
                      value = dbboxobj()$Percentual[1]),
          barprogress(title = dbboxobj()$Objetivo[2],type = type_color,
                      value = dbboxobj()$Percentual[2]))
      }
      box
    })
    output$box.obj.pro <- renderUI({
      type_color <- "success"
      if(input$year0 %in% c("2017", "2018", "2019")){
      box <- box(
        width = 6,
        title = strong("PROCESSOS INTERNOS"),
        solidHeader = FALSE,
        status = type_color,
        collapsible = TRUE,
        collapsed = FALSE,
        barprogress(title = dbboxobj()$Objetivo[5],type = type_color,
                    value = dbboxobj()$Percentual[5]),
        barprogress(title = dbboxobj()$Objetivo[6],type = type_color,
                    value = dbboxobj()$Percentual[6]),
        barprogress(title = dbboxobj()$Objetivo[7],type = type_color,
                    value = dbboxobj()$Percentual[7]),
        barprogress(title = dbboxobj()$Objetivo[8],type = type_color,
                    value = dbboxobj()$Percentual[8]),
        barprogress(title = dbboxobj()$Objetivo[9],type = type_color,
                    value = dbboxobj()$Percentual[9]),
        barprogress(title = dbboxobj()$Objetivo[10],type = type_color,
                    value = dbboxobj()$Percentual[10]),
        barprogress(title = dbboxobj()$Objetivo[11],type = type_color,
                    value = dbboxobj()$Percentual[11]),
        barprogress(title = dbboxobj()$Objetivo[12],type = type_color,
                    value = dbboxobj()$Percentual[12]))
      } else {
        box <- box(
          width = 6,
          title = strong("PROCESSOS INTERNOS"),
          solidHeader = FALSE,
          status = type_color,
          collapsible = TRUE,
          collapsed = FALSE,
          barprogress(title = dbboxobj()$Objetivo[3],type = type_color,
                      value = dbboxobj()$Percentual[3]),
          barprogress(title = dbboxobj()$Objetivo[4],type = type_color,
                      value = dbboxobj()$Percentual[4]),
          barprogress(title = dbboxobj()$Objetivo[5],type = type_color,
                      value = dbboxobj()$Percentual[5]))
      }
    })
    output$box.obj.pes <- renderUI({
      type_color <- "primary"
      if(input$year0 %in% c("2017", "2018", "2019")){
      box <- box(width = 6,title = strong("PESSOAS E TECNOLOGIA"),solidHeader = FALSE,status = type_color,collapsible = TRUE,collapsed = FALSE,
          barprogress(title = dbboxobj()$Objetivo[13],type = type_color,
                      value = dbboxobj()$Percentual[13]),
          barprogress(title = dbboxobj()$Objetivo[14],type = type_color,
                      value = dbboxobj()$Percentual[14]),
          barprogress(title = dbboxobj()$Objetivo[15],type = type_color,
                      value = dbboxobj()$Percentual[15]),
          barprogress(title = dbboxobj()$Objetivo[16],type = type_color,
                      value = dbboxobj()$Percentual[16]))
      } else {
        box <- box(width = 6,title = strong("PESSOAS E TECNOLOGIA"),solidHeader = FALSE,status = type_color,collapsible = TRUE,collapsed = FALSE,
                   barprogress(title = dbboxobj()$Objetivo[6],type = type_color,
                               value = dbboxobj()$Percentual[6]),
                   barprogress(title = dbboxobj()$Objetivo[7],type = type_color,
                               value = dbboxobj()$Percentual[7]),
                   barprogress(title = dbboxobj()$Objetivo[8],type = type_color,
                               value = dbboxobj()$Percentual[8]))
      }
      box
    })
    
    observe({
      type_color <- "warning"
      if(input$year0 %in% c("2017", "2018", "2019")){
        output$box.obj.soc <- renderUI({box <- box(width = 6,title = strong("SOCIEDADE"),solidHeader = FALSE,status = type_color,collapsible = TRUE,collapsed = FALSE,
                   barprogress(title = dbboxobj()$Objetivo[4],type = type_color,
                               value = dbboxobj()$Percentual[4]))})
      } else {
        output$box.obj.soc <- renderText("")
      }
    })
    # PLANEJAMENTO - INDICADORES
    output$menuperspectiva <- renderUI({
      choices <- sort(unique(sgi[sgi$Ano == input$year1 & sgi$Tipo == "Perspectiva",]$Titulo))
      selectInput(inputId = "input0menu1sub1", label = "Perspectiva", choices = choices, selected = choices, multiple = TRUE, width = "100%")
    })
    output$menuindicador <- renderUI({
      choices <- sort(unique(sgi[sgi$Ano == input$year1 & sgi$Tipo == "Indicador" & sgi$Situacao %in% c("Nova","Em andamento","Resolvida"),]$Titulo))
      selectInput(inputId = "input3menu1sub1", label = "Indicador", choices = choices, selected = choices[1], width = "100%")
    })
    dbindger <- reactive({
      if(input$input1menu1sub1 == "Quantidade"){
        bd <- sgi %>%
          filter(Ano == input$year1,Tipo1 == "Indicador",Tipo == "Acao",Titulo4 %in% input$input0menu1sub1) %>%
          group_by(Titulo4,Titulo3,Titulo2,Cod_Indicador = paste0(substr(Titulo1,1,1),substr(Titulo1,3,3),substr(Titulo1,5,5)),Tipo = "Qtd de Ações") %>%
          summarise(Quantidade = n())
        bd$Cor <- ifelse(substr(bd$Cod_Indicador,1,1) == "1","#00a65a",ifelse(substr(bd$Cod_Indicador,1,1) == 2,"#00c0ef",ifelse(substr(bd$Cod_Indicador,1,1) == 3,"#f39c12",ifelse(substr(bd$Cod_Indicador,1,1) == 4,"#dd4b39",""))))
        list(Perspectiva = bd$Titulo4,Objetivo = bd$Titulo3,Indicador = bd$Titulo2,Cod_Indicador = bd$Cod_Indicador,Resultado = bd$Quantidade,Tipo = bd$Tipo,Cor = bd$Cor)
      }else if(input$input1menu1sub1 == "Percentual"){
        bd <- sgi %>%
          filter(Ano == input$year1,Tipo1 == "Objetivo",Tipo == "Indicador",Titulo2 %in% input$input0menu1sub1) %>%
          group_by(Titulo3,Titulo2,Titulo,Cod_Indicador = paste0(substr(Titulo,1,1),substr(Titulo,3,3),substr(Titulo,5,5)),Perc_Terminado,Tipo = "% Executado") %>%
          summarise()
        bd$Cor <- ifelse(substr(bd$Cod_Indicador,1,1) == "1","#00a65a",ifelse(substr(bd$Cod_Indicador,1,1) == 2,"#00c0ef",ifelse(substr(bd$Cod_Indicador,1,1) == 3,"#f39c12",ifelse(substr(bd$Cod_Indicador,1,1) == 4,"#dd4b39",""))))
        list(Perspectiva = bd$Titulo3,Objetivo = bd$Titulo2,Indicador = bd$Titulo,Cod_Indicador = bd$Cod_Indicador,Resultado = bd$Perc_Terminado,Tipo = bd$Tipo,Cor = bd$Cor)
      }
    })
    output$ind.com.ger <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$credits(enabled=FALSE)
      h1$data(x = dbindger()$Indicador,y = dbindger()$Resultado,name = unique(dbindger()$Tipo),color = colorchart$SGISitValor$Geral)
      h1$xAxis(categories = c(list(as.character(dbindger()$Cod_Indicador))[[1]],as.character(dbindger()$Cod_Indicador)))
      if(input$input1menu1sub1 == "Quantidade")
      {h1$yAxis(title="",stackLabels = list(enabled = TRUE))} else{h1$yAxis(title="",stackLabels = list(enabled = TRUE),max = 100)}
      if(input$input1menu1sub1 == "Quantidade")
      {h1$tooltip(pointFormat = '{point.y}<br/>')}else{h1$tooltip(pointFormat = '{point.y}%<br/>')}
      h1$plotOptions(column = list(dataLabels = list(enabled = F),allowPointSelect = T,borderRadius = 4))
      h1$set(width = session$clientData$ind.com.ger)
      return(h1)
    })
    dbinduni <- reactive({
      bd0 <- sgi %>%
        filter(Tipo == "Acao",Tipo1 == "Indicador",Ano == input$year1,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade,Cod_Indicador = substr(Titulo1,1,5)) %>%
        summarise(Quantidade = n())
      bd1 <- sgi %>%
        filter(Tipo == "Acao",Tipo1 == "Indicador",Ano == input$year1,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade,Cod_Indicador = substr(Titulo1,1,5),Situacao_Corrigida) %>%
        summarise(Quantidade = n())
      bd1 <- spread(data = bd1,key = Situacao_Corrigida,value = Quantidade, fill = 0)
      bd <- sgi %>%
        filter(Tipo == "Indicador",Tipo1 == "Indicador",Ano == input$year1,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade,Cod_Indicador = substr(Titulo,1,5), Indicador = Titulo,Percentual = Perc_Terminado) %>%
        summarise() %>%
        inner_join(y = bd0,by = c("Unidade","Cod_Indicador")) %>%
        inner_join(y = bd1,by = c("Unidade","Cod_Indicador"))
    })
    output$ind.exe.per <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$data(x = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Percentual,y = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Unidade,name = "% Executado",color = colorchart$SGISitValor$Geral)
      h1$xAxis(categories = c(list(as.character(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Unidade))[[1]],as.character(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Unidade)))
      h1$yAxis(min = 0,max = 100,title="")
      h1$plotOptions(bar = list(dataLabels = list(enabled = F),allowPointSelect = T,borderRadius = 4,borderColor= colorchart$SGISitValor$Borda))
      h1$set(width = session$clientData$ind.exe.per)
      return(h1)
    })
    output$ind.sit.uni <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(bar = list(stacking = "normal",dataLabels = list(enabled = F),allowPointSelect = T, borderRadius = 4))
      if(suppressWarnings(is.null(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Fechada))){NULL}else{
        h1$series(name = "Fechada",data = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Fechada,color = colorchart$SGISitTarefa$Fechada)}
      if(suppressWarnings(is.null(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Nao_Iniciada))){NULL}else{
        h1$series(name = "Não Iniciada",data = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Nao_Iniciada,color = colorchart$SGISitTarefa$NaoIniciada)}
      if(suppressWarnings(is.null(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Iniciada))){NULL}else{
        h1$series(name = "Iniciada",data = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Iniciada,color = colorchart$SGISitTarefa$Iniciada)}
      if(suppressWarnings(is.null(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Iniciada))){NULL}else{
        h1$series(name = "Finalizada",data = dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Finalizada,color = colorchart$SGISitTarefa$Finalizada)}
      h1$xAxis(categories = c(list(as.character(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Unidade))[[1]],as.character(dbinduni()[dbinduni()$Cod_Indicador == substr(input$input3menu1sub1,1,5),]$Unidade)))
      h1$yAxis(title="")
      h1$set(width = session$clientData$ind.sit.uni)
      return(h1)
    })
    # PLANEJAMENTO - PLANO DE AÇÃO - GERAL
    dbacacom <- reactive({
      bd0 <- sgi %>%
        filter(Tipo == "Acao" & Tipo1 == "Indicador",Ano == input$year2,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade, Tipo = Situacao_Corrigida) %>%
        summarise(Valor= n())
      bd1 <- sgi %>%
        filter(Tipo == "Acao" & Tipo1 == "Indicador", Ano == input$year2,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade, Tipo = Situacao_Prazo) %>%
        summarise(Valor = n())
      bd2 <- sgi %>%
        filter(Tipo == "Acao" & Tipo1 == "Indicador",Ano == input$year2,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade, Tipo = "Quantidade") %>%
        summarise(Valor = n())
      bd3 <- bind_rows(bd0,bd1,bd2) %>% group_by(Tipo) %>% summarise(Valor = sum(Valor,na.rm=TRUE))
      bd3$Unidade <- "IFB"
      bd3 <- bd3[, c(3, 1, 2)]
      bd <- bind_rows(bd0,bd1,bd2,bd3)
      if(any(is.na(bd$Tipo))){
        bd <- bd[!is.na(bd$Tipo),]
      }
      bd
    })
    output$aca.sit.ger <- renderChart2({
      bd <- dbacacom()[dbacacom()$Unidade == "IFB",]
      if(!any(bd$Tipo == "Nao_Iniciada")){
        db <- tibble(Unidade = "IFB", Tipo = "Nao_Iniciada", Valor = 0L)
        bd <- bind_rows(bd, db)
      }
      if(!any(bd$Tipo == "Iniciada")){
        db <- tibble(Unidade = "IFB", Tipo = "Iniciada", Valor = 0L)
        bd <- bind_rows(bd, db)
      }
      if(!any(bd$Tipo == "Finalizada")){
        db <- tibble(Unidade = "IFB", Tipo = "Finalizada", Valor = 0L)
        bd <- bind_rows(bd, db)
      }
      h1 <- Highcharts$new()
      h1$chart(type = "pie")
      h1$credits(enabled=FALSE)
      h1$series(data = list(
        list(name = "Não Iniciada", y = bd[bd$Tipo == "Nao_Iniciada",]$Valor,color = colorchart$SGISitTarefa$NaoIniciada),
        list(name = "Iniciada",y = bd[bd$Tipo == "Iniciada",]$Valor,color = colorchart$SGISitTarefa$Iniciada),
        list(name = "Finalizada",y = bd[bd$Tipo == "Finalizada",]$Valor,color = colorchart$SGISitTarefa$Finalizada),
        list(name = "Fechada",y = bd[bd$Tipo == "Fechada",]$Valor,color = colorchart$SGISitTarefa$Fechada)))
      h1$tooltip(pointFormat = '{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(pie = list(dataLabels = list(enabled = FALSE), allowPointSelect = TRUE, showInLegend = TRUE))
      h1$set(width = session$clientData$aca.sit.ger)
      return(h1)
    })
    output$aca.sit.com <- renderChart2({
      db <- dbacacom() %>% filter(!Unidade %in% "IFB")
      db <- spread(data = db,key = Tipo,value = Valor,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$credits(enabled=FALSE)
      h1$xAxis(categories = db$Unidade)
      h1$yAxis(title="",stackLabels = list(enabled = TRUE))
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(column = list(stacking = "normal",dataLabels = list(enabled = F),allowPointSelect = T,borderRadius = 3))
      if(suppressWarnings(is.null(db$Fechada))){NULL}else{
        h1$series(name = "Fechada",data = db$Fechada,color = colorchart$SGISitTarefa$Fechada)}
      if(suppressWarnings(is.null(db$Nao_Iniciada))){NULL}else{
        h1$series(name = "Não Iniciada",data = db$Nao_Iniciada,color = colorchart$SGISitTarefa$NaoIniciada)}
      if(suppressWarnings(is.null(db$Iniciada))){NULL}else{
        h1$series(name = "Iniciada",data = db$Iniciada,color = colorchart$SGISitTarefa$Iniciada)}
      if(suppressWarnings(is.null(db$Finalizada))){NULL}else{
        h1$series(name = "Finalizada",data = db$Finalizada,color = colorchart$SGISitTarefa$Finalizada)}
      h1$set(width = session$clientData$aca.sit.com)
      return(h1)
    })
    output$aca.pra.ger <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "pie")
      h1$credits(enabled=FALSE)
      h1$series(data = list(
        list(name = "Sem Data",y = dbacacom()[dbacacom()$Unidade == "IFB" & dbacacom()$Tipo == "Sem_Data",]$Valor,color = colorchart$SGISitPrazo$SemData),
        list(name = "Em atraso",y = dbacacom()[dbacacom()$Unidade == "IFB" & dbacacom()$Tipo == "Em_atraso",]$Valor,color = colorchart$SGISitPrazo$EmAtraso),
        list(name = "No prazo",y = dbacacom()[dbacacom()$Unidade == "IFB" & dbacacom()$Tipo == "No_prazo",]$Valor,color = colorchart$SGISitPrazo$NoPrazo),
        list(name = "Concluida",y = dbacacom()[dbacacom()$Unidade == "IFB" & dbacacom()$Tipo == "Concluida",]$Valor,color = colorchart$SGISitPrazo$Concluida)))
      h1$tooltip(pointFormat = '{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(pie = list(dataLabels = list(enabled = FALSE),allowPointSelect = TRUE,showInLegend = TRUE))
      h1$set(width = session$clientData$aca.pra.ger)
      return(h1)
    })
    output$aca.pra.com <- renderChart2({
      db <- dbacacom() %>% filter(!Unidade %in% "IFB")
      db <- spread(data = db,key = Tipo,value = Valor,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$credits(enabled=FALSE)
      h1$xAxis(categories = db$Unidade)
      h1$yAxis(title="",stackLabels = list(enabled = TRUE))
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(column = list(stacking = "normal",dataLabels = list(enabled = FALSE),allowPointSelect = T,borderRadius = 3))
      if(suppressWarnings(is.null(db$Sem_Data))){NULL}else{
        h1$series(name = "Sem data",data = db$Sem_Data,color = colorchart$SGISitPrazo$SemData)}
      if(suppressWarnings(is.null(db$Sem_Data))){NULL}else{
        h1$series(name = "Em atraso",data = db$Em_atraso,color = colorchart$SGISitPrazo$EmAtraso)}
      if(suppressWarnings(is.null(db$No_prazo))){NULL}else{
        h1$series(name = "No prazo",data = db$No_prazo,color = colorchart$SGISitPrazo$NoPrazo)}
      if(suppressWarnings(is.null(db$No_prazo))){NULL}else{
        h1$series(name = "Concluida",data = db$Concluida,color = colorchart$SGISitPrazo$Concluida)}
      h1$set(width = session$clientData$aca.pra.com)
      return(h1)
    })
    # PLANEJAMENTO - PLANO DE AÇÃO - UNIDADE
    dbacasituni <- reactive({
      db <- sgi %>%
        filter(Ano == input$year3,Tipo == "Acao", Tipo1 == "Indicador",Id_Projeto %in% project_app$planejamento,Unidade == input$input0menu1sub2sub2) %>%
        group_by(Situacao_Corrigida) %>%
        summarise(Valor = n())
    })
    output$aca.sit.uni <- renderChart2({
      db <- spread(data = dbacasituni(),key = Situacao_Corrigida,value = Valor,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "pie")
      h1$credits(enabled=FALSE)
      h1$series(data = list(
        list(name = "Não Iniciada",y = if(is.null(db$Nao_Iniciada)){0}else{db$Nao_Iniciada},color = colorchart$SGISitTarefa$NaoIniciada),
        list(name = "Iniciada",y = if(is.null(db$Iniciada)){0}else{db$Iniciada},color = colorchart$SGISitTarefa$Iniciada),
        list(name = "Finalizada",y = if(is.null(db$Finalizada)){0}else{db$Finalizada},color = colorchart$SGISitTarefa$Finalizada),
        list(name = "Fechada",y = if(is.null(db$Fechada)){0}else{db$Fechada},color = colorchart$SGISitTarefa$Fechada)))
      h1$tooltip(pointFormat = '{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(pie = list(dataLabels = list(enabled = FALSE),allowPointSelect = TRUE,showInLegend = TRUE))
      h1$set(width = session$clientData$aca.sit.uni)
      return(h1)
    })
    dbacaprauni <- reactive({
      db <- sgi %>%
        filter(Ano == input$year3,Tipo == "Acao", Tipo1 == "Indicador",Id_Projeto %in% project_app$planejamento,Unidade == input$input0menu1sub2sub2) %>%
        group_by(Situacao_Prazo) %>%
        summarise(Valor = n())
    })
    output$aca.pra.uni <- renderChart2({
      db <- spread(data = dbacaprauni(),key = Situacao_Prazo,value = Valor,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "pie")
      h1$credits(enabled=FALSE)
      h1$series(data = list(
        list(name = "Sem Data",y = if(suppressWarnings(is.null(db$Sem_Data))){0}else{db$Sem_Data},color = colorchart$SGISitPrazo$SemData),
        list(name = "Em atraso",y = if(suppressWarnings(is.null(db$Em_atraso))){0}else{db$Em_atraso},color = colorchart$SGISitPrazo$EmAtraso),
        list(name = "No prazo",y = if(suppressWarnings(is.null(db$No_prazo))){0}else{db$No_prazo},color = colorchart$SGISitPrazo$NoPrazo),
        list(name = "Concluida",y = if(suppressWarnings(is.null(db$Concluida))){0}else{db$Concluida},color = colorchart$SGISitPrazo$Concluida)))
      h1$tooltip(pointFormat = '{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(pie = list(dataLabels = list(enabled = FALSE),allowPointSelect = TRUE,showInLegend = TRUE))
      h1$set(width = session$clientData$aca.pra.uni)
      return(h1)
    })
    dbacares <- reactive({
      bd0 <- sgi %>%
        filter(Tipo == "Acao", Tipo1 == "Indicador",Ano == input$year3,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Grupo = "Setor",Unidade = "IFB",Responsavel = Setor_Sigla,Situacao = Situacao_Corrigida) %>%
        summarise(Quantidade = n())
      bd0$Situacao <- str_replace_all(bd0$Situacao,c("[ã]"="a","[ ]"="_"))
      bd1 <- sgi %>%
        filter(Tipo == "Acao", Tipo1 == "Indicador",Ano == input$year3,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Grupo = "Envolvido",Unidade = "IFB",Responsavel = Atribuido_para,Situacao = Situacao_Corrigida) %>%
        summarise(Quantidade = n())
      bd1$Situacao <- str_replace_all(bd1$Situacao,c("[ã]"="a","[ ]"="_"))
      bd2 <- sgi %>%
        filter(Tipo == "Acao", Tipo1 == "Indicador",Ano == input$year3,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Grupo = "Setor",Unidade,Responsavel = Setor_Sigla,Situacao = Situacao_Corrigida) %>%
        summarise(Quantidade = n())
      bd2$Situacao <- str_replace_all(bd2$Situacao,c("[ã]"="a","[ ]"="_"))
      bd3 <- sgi %>%
        filter(Tipo == "Acao", Tipo1 == "Indicador",Ano == input$year3,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Grupo = "Envolvido",Unidade,Responsavel = Atribuido_para,Situacao = Situacao_Corrigida) %>%
        summarise(Quantidade = n())
      bd3$Situacao <- str_replace_all(bd3$Situacao,c("[ã]"="a","[ ]"="_"))
      bd <- bind_rows(bd0,bd1,bd2,bd3)
      bd <- spread(bd,key = Situacao,value = Quantidade,fill = 0)
    })
    output$aca.sit.set <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(bar = list(stacking = "normal",dataLabels = list(enabled = FALSE),allowPointSelect = T,borderRadius = 3))
      h1$xAxis(categories = c(list(as.character(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Responsavel))[[1]],as.character(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Responsavel)))
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Fechada))){NULL}else{
        h1$series(name = "Fechada",data = dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Fechada,color = colorchart$SGISitTarefa$Fechada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Nao_Iniciada))){NULL}else{
        h1$series(name = "Não Iniciada",data = dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Nao_Iniciada,color = colorchart$SGISitTarefa$NaoIniciada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Iniciada))){NULL}else{
        h1$series(name = "Iniciada",data = dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Iniciada,color = colorchart$SGISitTarefa$Iniciada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Finalizada))){NULL}else{
        h1$series(name = "Finalizada",data = dbacares()[dbacares()$Grupo == "Setor" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Finalizada,color = colorchart$SGISitTarefa$Finalizada)}
      h1$yAxis(title="")
      h1$set(width = session$clientData$aca.sit.set)
      return(h1)
    })
    dbacapraset <- reactive({
      db <- sgi %>%
        filter(Ano == input$year3,Tipo %in% "Acao", Tipo1 == "Indicador",Unidade == input$input0menu1sub2sub2) %>%
        group_by(Unidade,Setor_Sigla,Situacao_Prazo) %>%
        summarise(Quantidade = n())
    })
    output$aca.pra.set <- renderChart2({
      db <- spread(data = dbacapraset(),key = Situacao_Prazo,value = Quantidade,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(bar = list(stacking = "normal",dataLabels = list(enabled = FALSE),allowPointSelect = T,borderRadius = 3))
      if(suppressWarnings(is.null(db$Sem_Data))){NULL}else{
        h1$series(name = "Sem data",data = db$Sem_Data,color = colorchart$SGISitPrazo$SemData)}
      if(suppressWarnings(is.null(db$Em_atraso))){NULL}else{
        h1$series(name = "Em atraso",data = db$Em_atraso,color = colorchart$SGISitPrazo$EmAtraso)}
      if(suppressWarnings(is.null(db$No_prazo))){NULL}else{
        h1$series(name = "No prazo",data = db$No_prazo,color = colorchart$SGISitPrazo$NoPrazo)}
      if(suppressWarnings(is.null(db$Finalizada))){NULL}else{
        h1$series(name = "Concluida",data = db$Concluida,color = colorchart$SGISitPrazo$Concluida)}
      h1$xAxis(categories = c(list(as.character(db$Setor_Sigla))[[1]],as.character(db$Setor_Sigla)))
      h1$set(width = session$clientData$aca.pra.set)
      return(h1)
    })
    output$aca.sit.env <- renderChart2({
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(bar = list(stacking = "normal",dataLabels = list(enabled = FALSE),allowPointSelect = T,borderRadius = 3))
      h1$xAxis(categories = c(list(as.character(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Responsavel))[[1]],as.character(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Responsavel)))
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Fechada))){NULL}else{
        h1$series(name = "Fechada",data = dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Fechada,color = colorchart$SGISitTarefa$Fechada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Nao_Iniciada))){NULL}else{
        h1$series(name = "Não Iniciada",data = dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Nao_Iniciada,color = colorchart$SGISitTarefa$NaoIniciada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Iniciada))){NULL}else{
        h1$series(name = "Iniciada",data = dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Iniciada,color = colorchart$SGISitTarefa$Iniciada)}
      if(suppressWarnings(is.null(dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Finalizada))){NULL}else{
        h1$series(name = "Finalizada",data = dbacares()[dbacares()$Grupo == "Envolvido" & dbacares()$Unidade == input$input0menu1sub2sub2,]$Finalizada,color = colorchart$SGISitTarefa$Finalizada)}
      h1$yAxis(title="")
      h1$set(width = session$clientData$aca.sit.env)
      return(h1)
    })
    dbacapraenv <- reactive({
      db <- sgi %>%
        filter(Ano == input$year3,Tipo %in% "Acao", Tipo1 == "Indicador",Unidade == input$input0menu1sub2sub2) %>%
        group_by(Unidade,Atribuido_para,Situacao_Prazo) %>%
        summarise(Quantidade = n())
    })
    output$aca.pra.env <- renderChart2({
      db <- spread(data = dbacapraenv(),key = Situacao_Prazo,value = Quantidade,fill = 0)
      h1 <- Highcharts$new()
      h1$chart(type = "bar")
      h1$credits(enabled=FALSE)
      h1$tooltip(pointFormat = '{series.name}<br/>{point.y}<br/>{point.percentage:.1f}%<br/>')
      h1$plotOptions(bar = list(stacking = "normal",dataLabels = list(enabled = FALSE),allowPointSelect = T,borderRadius = 3))
      if(suppressWarnings(is.null(db$Sem_Data))){NULL}else{
        h1$series(name = "Sem data",data = db$Sem_Data,color = colorchart$SGISitPrazo$SemData)}
      if(suppressWarnings(is.null(db$Em_atraso))){NULL}else{
        h1$series(name = "Em atraso",data = db$Em_atraso,color = colorchart$SGISitPrazo$EmAtraso)}
      if(suppressWarnings(is.null(db$No_prazo))){NULL}else{
        h1$series(name = "No prazo",data = db$No_prazo,color = colorchart$SGISitPrazo$NoPrazo)}
      if(suppressWarnings(is.null(db$Finalizada))){NULL}else{
        h1$series(name = "Concluida",data = db$Concluida,color = colorchart$SGISitPrazo$Concluida)}
      h1$xAxis(categories = c(list(as.character(db$Atribuido_para))[[1]],as.character(db$Atribuido_para)))
      h1$set(width = session$clientData$aca.pra.env)
      return(h1)
    })
    dbtabenv <- reactive({
      bd <- sgi %>% filter(Tipo1 == "Indicador", Tipo == "Acao",Ano == input$year3,Unidade == input$input0menu1sub2sub2,Id_Projeto %in% project_app$planejamento) %>%
        group_by(Unidade,Indicador = substr(Titulo1,1,5),Id,Titulo,Envolvido = Atribuido_para,Inicio,Fim = Data_prevista,Percentual = Perc_Terminado) %>%
        summarise() %>%
        arrange(Indicador,Id)
      bd$Inicio <- format(bd$Inicio,"%d/%m/%Y")
      bd$Fim <- format(bd$Fim,"%d/%m/%Y")
      bd$Id <- paste0('<a href="http://sgi.prdi.ifb.edu.br/issues/', bd$Id,'">',bd$Id,'</a>')
      bd <- bd[, c("Id","Indicador","Titulo","Envolvido","Inicio","Fim","Percentual")]
    })
    output$tab.aca.env <- renderDataTable({
      datatable(dbtabenv(),
                rownames = FALSE, escape = FALSE, colnames = c("#", "Indicador", "Ação", "Envolvido", "Início", "Fim", "%"),
                options = list(language = list(url ='http://cdn.datatables.net/plug-ins/1.10.7/i18n/Portuguese-Brasil.json'))) %>% 
        formatStyle("Percentual",
                    background = styleColorBar(dbtabenv()$Percentual, 'lightblue'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    })
    # INDICADORES PDI
    output$i111 <- plot_rChart_ifb(id = "i111", fonte = i111, origem = "SISTEC", lim_eixo_y = c(min = 0, max = 10))
    output$i112 <- plot_rChart_ifb(id = "i112", fonte = i112, origem = "SISTEC", lim_eixo_y = c(min = 0, max = 10))
    output$i121 <- plot_rChart_campi(id = "i121", fonte = i121, origem = "SISTEC")
    output$i122 <- plot_rChart_campi(id = "i122", fonte = i122, origem = "SISTEC")
    output$i123 <- plot_rChart_campi(id = "i123", fonte = i123, origem = "SISTEC")
    output$i124 <- plot_rChart_campi(id = "i124", fonte = i124, origem = "SISTEC")
    output$i131 <- plot_rChart_ifb(id = "i131", fonte = i131, origem = "SGI e Google Drive")
    output$i132 <- plot_rChart_ifb(id = "i132", fonte = i132, origem = "SGI e Google Drive")
    output$i133 <- plot_rChart_ifb(id = "i133", fonte = i133, origem = "SGI e Google Drive")
    output$i211 <- plot_rChart_campi(id = "i211", fonte = i211, origem = "SGI e Google Drive")
    output$i213 <- plot_rChart_ifb(id = "i213", fonte = i213, origem = "SGI e Google Drive")
    output$i214 <- plot_rChart_ifb(id = "i214", fonte = i214, origem = "SGI e Google Drive")
    output$i311 <- plot_rChart_ifb(id = "i311", fonte = i311, origem = "SGI e Google Drive")
    output$i312 <- plot_rChart_ifb(id = "i312", fonte = i312, origem = "SGI e Google Drive")
    output$i313 <- plot_rChart_ifb(id = "i313", fonte = i313, origem = "SGI e Google Drive")
    output$i314 <- plot_rChart_ifb(id = "i314", fonte = i314, origem = "SGI e Google Drive")
    output$i321 <- plot_rChart_ifb(id = "i321", fonte = i321, origem = "SGI e Google Drive")
    output$i331 <- plot_rChart_ifb(id = "i331", fonte = i331, origem = "SGI e Google Drive")
    output$i332 <- plot_rChart_ifb(id = "i332", fonte = i332, origem = "SGI e Google Drive")
    output$i333 <- plot_rChart_ifb(id = "i333", fonte = i333, origem = "SGI e Google Drive")
    output$i341 <- plot_rChart_ifb(id = "i341", fonte = i341, origem = "SGI e Google Drive")
    output$i342 <- plot_rChart_ifb(id = "i342", fonte = i342, origem = "SGI e Google Drive")
    output$i343 <- plot_rChart_ifb(id = "i343", fonte = i343, origem = "SGI e Google Drive")
    output$i351 <- plot_rChart_ifb(id = "i351", fonte = i351, origem = "SGI e Google Drive")
    output$i361 <- plot_rChart_campi(id = "i361", fonte = i361, origem = "SGI e Google Drive")
    output$i362 <- plot_rChart_ifb(id = "i362", fonte = i362, origem = "SGI e Google Drive")
    output$i363 <- plot_rChart_ifb(id = "i363", fonte = i363, origem = "SGI e Google Drive")
    output$i371 <- plot_rChart_ifb(id = "i371", fonte = i371, origem = "SISTEC")
    output$i372 <- plot_rChart_ifb(id = "i372", fonte = i372, origem = "SISTEC")
    output$i373 <- plot_rChart_ifb(id = "i373", fonte = i373, origem = "SISTEC")
    output$i381 <- plot_rChart_ifb(id = "i381", fonte = i381, origem = "SISTEC")
    output$i382 <- plot_rChart_ifb(id = "i382", fonte = i382, origem = "SISTEC")
    output$i383 <- plot_rChart_ifb(id = "i383", fonte = i383, origem = "SISTEC")
    output$i411 <- plot_rChart_ifb(id = "i411", fonte = i411, origem = "SGI e Google Drive")
    output$i412 <- plot_rChart_ifb(id = "i412", fonte = i412, origem = "SGI e Google Drive")
    output$i413 <- plot_rChart_ifb(id = "i413", fonte = i413, origem = "SGI e Google Drive")
    output$i414 <- plot_rChart_ifb(id = "i414", fonte = i414, origem = "SGI e Google Drive")
    output$i415 <- plot_rChart_ifb(id = "i415", fonte = i415, origem = "SGI e Google Drive")
    output$i421 <- plot_rChart_ifb(id = "i421", fonte = i421, origem = "SGI e Google Drive")
    output$i422 <- plot_rChart_ifb(id = "i422", fonte = i422, origem = "SGI e Google Drive")
    output$i441 <- plot_rChart_ifb(id = "i441", fonte = i441, origem = "SGI e Google Drive")
    output$i442 <- plot_rChart_ifb(id = "i442", fonte = i442, origem = "SGI e Google Drive")
    output$i443 <- plot_rChart_ifb(id = "i443", fonte = i443, origem = "SGI e Google Drive")
  }

shinyApp(ui, server)
