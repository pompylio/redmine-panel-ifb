sink("script/log_extract_rg.txt") 
# library -----------------------------------------------------------------
library(dplyr)
library(googlesheets)
library(lubridate)
# data --------------------------------------------------------------------
sistec_2013 <- readRDS("data/sistec_2013.rds")
sistec_2014 <- readRDS("data/sistec_2014.rds")
sistec_2015 <- readRDS("data/sistec_2015.rds")
sistec_2016 <- readRDS("data/sistec_2016.rds")
sistec_2017 <- readRDS("data/sistec_2017.rds")
sistec_2018 <- readRDS("data/sistec_2018.rds")

gs_auth(token = "security/googlesheets_token.rds", verbose = FALSE)

import_sheets <- function(key_url,spreadsheet,col_isna){
  db <- extract_key_from_url(key_url)
  db <- gs_key(db)
  db <- suppressWarnings(suppressMessages(gs_read(ss = db, ws = spreadsheet)))
  names(db) <- db[1,]
  db <- db[-1,]
  db <- db[!(is.na(db[,col_isna])),]
  return(db)
}
unidade <- data.frame(unidade_ensino = unique(sistec_2017$unidade_ensino),stringsAsFactors = FALSE)
unidade$sigla <- ifelse(grepl(pattern = "SOB",unidade$unidade_ensino),"CSOB",
                 ifelse(grepl(pattern = "BRA",unidade$unidade_ensino),"CBRA",
                 ifelse(grepl(pattern = "CEI",unidade$unidade_ensino),"CCEI",
                 ifelse(grepl(pattern = "EST",unidade$unidade_ensino),"CEST",
                 ifelse(grepl(pattern = "GAM",unidade$unidade_ensino),"CGAM",
                 ifelse(grepl(pattern = "PLA",unidade$unidade_ensino),"CPLA",
                 ifelse(grepl(pattern = "RIA",unidade$unidade_ensino),"CRFI",
                 ifelse(grepl(pattern = "SEB",unidade$unidade_ensino),"CSSB",
                 ifelse(grepl(pattern = "CEN",unidade$unidade_ensino),"CREM",
                 ifelse(grepl(pattern = "SAM",unidade$unidade_ensino),"CSAM",
                 ifelse(grepl(pattern = "TAG",unidade$unidade_ensino),"CTAG","")))))))))))
unidade[(nrow(unidade)+1),] <- c("INSTITUTO FEDERAL DE BRASILIA","IFB")
keys <- gs_ls(regex = "Indicadores IFB \\[")
keys$link <- paste0("https://docs.google.com/spreadsheets/d/",keys$sheet_key,"/edit")
keys$department <- substr(keys$sheet_title,
                          as.numeric(gregexpr(pattern = "\\[",text = keys$sheet_title)) + 1,
                          as.numeric(gregexpr(pattern = "\\]",text = keys$sheet_title)) - 1)
keys_cam <- keys[keys$department %in% unidade$sigla,]
keys_rei <- keys[!keys$department %in% unidade$sigla,]

# 1.1.1 OK Número de campus ofertando ensino médio integrado por ano -------------------------------------------------
oferta <- function(fonte, ano, indicador){
  if (indicador == "1.1.1") db <- fonte %>% filter(grepl("\\<TÉCNICO\\>", tipo_curso) & grepl("\\<Integrado\\>", tipo_oferta))
  else if (indicador == "1.1.2") db <- fonte %>% filter(grepl("\\<PROEJA\\>",no_curso)|grepl("\\<PROEJA\\>",tipo_oferta)|grepl("\\<PROEJA\\>",nome_ciclo))
  db <- db %>% 
    filter(matriculados == 1) %>% 
    group_by(sigla_unidade) %>%
    summarise(resultado = 1)
  db0 <- data.frame(sigla_unidade="IFB", resultado = sum(db$resultado), stringsAsFactors = FALSE)
  db <- bind_rows(db, db0)
  db0 <- unidade[!unidade$sigla %in% c(db$sigla_unidade,"CSOB","IFB"),]$sigla
  if(nrow(db) < nrow(unidade)){
    db0 <- data.frame(sigla_unidade = db0,resultado = 0,stringsAsFactors = FALSE)
    db <- bind_rows(db,db0)}
  db$indicador <- indicador
  db$ano <- ano
  db <- db[order(db$sigla_unidade),c("indicador","ano","sigla_unidade","resultado")]
  return(db)
}
db2014 <- oferta(fonte = sistec_2014, ano = "2014", indicador = "1.1.1")
db2015 <- oferta(fonte = sistec_2015, ano = "2015", indicador = "1.1.1")
db2016 <- oferta(fonte = sistec_2016, ano = "2016", indicador = "1.1.1")
db2017 <- oferta(fonte = sistec_2017, ano = "2017", indicador = "1.1.1")
db2018 <- oferta(fonte = sistec_2018, ano = "2018", indicador = "1.1.1")
i111 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i111)

# 1.1.2 OK Número de campus ofertando PROEJA por ano -----------------------------------------------------------------

db2014 <- oferta(fonte = sistec_2014, ano = "2014", indicador = "1.1.2")
db2015 <- oferta(fonte = sistec_2015, ano = "2015", indicador = "1.1.2")
db2016 <- oferta(fonte = sistec_2016, ano = "2016", indicador = "1.1.2")
db2017 <- oferta(fonte = sistec_2017, ano = "2017", indicador = "1.1.2")
db2018 <- oferta(fonte = sistec_2018, ano = "2018", indicador = "1.1.2")
i112 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i112)

# 1.2.1 OK Número de eventos em articulação ao mundo do trabalho por Campus ------------------------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i], import_sheets(key_url = keys_cam$link[i], spreadsheet = 1, col_isna = 1))}
db <- bind_rows(CTAG, CSSB, CSAM, CRFI, CREM, CPLA, CGAM, CEST, CCEI, CBRA)
db0 <- db %>% group_by(ANO, UNIDADE) %>% summarise(resultado = n())
db1 <- db %>% group_by(ANO, UNIDADE = "IFB") %>% summarise(resultado = n())
db <- bind_rows(db0, db1)
colnames(db) <- c("ano","sigla_unidade","resultado")
db$indicador <- "1.2.1"
i121 <- db[,c("indicador", "ano", "sigla_unidade", "resultado")]
print(i121)

# 1.2.2 OK Percentual de alunos matriculados licenciatura por campus -------------------------------------------------
perc_matriculados <- function(fonte, ano, tipo, indicador){
  if (indicador == "1.2.2") db <- fonte %>% filter(grepl("\\<LICENCIATURA\\>",tipo_curso))
  if (indicador == "1.2.3") db <- fonte %>% filter(grepl("\\<PROEJA\\>",no_curso)|grepl("\\<PROEJA\\>",tipo_oferta)|grepl("\\<PROEJA\\>",nome_ciclo))
  if (indicador == "1.2.4") db <- fonte %>% filter(grepl("\\<TÉCNICO\\>",tipo_curso))
  db0 <- db %>% 
    group_by(sigla_unidade) %>%
    summarise(mat_tipo = sum(matriculados, na.rm = TRUE))
  db1 <- db %>% 
    group_by(sigla_unidade = "IFB") %>%
    summarise(mat_tipo = sum(matriculados, na.rm = TRUE))
  db <- bind_rows(db0,db1)
  dbt <- fonte %>% 
    group_by(sigla_unidade) %>% 
    summarise(mat_total = sum(matriculados, na.rm = TRUE))
  dbt0 <- fonte %>% 
    group_by(sigla_unidade = "IFB") %>% 
    summarise(mat_total = sum(matriculados, na.rm = TRUE))
  dbt <- bind_rows(dbt, dbt0)
  db <- left_join(dbt, db, by = "sigla_unidade")
  if(any(is.na(db$mat_tipo)))
    db[is.na(db$mat_tipo),]$mat_tipo <- 0
  if(any(is.na(db$mat_total)))
    db[is.na(db$mat_total),]$mat_total <- 0
  db0 <- unidade[!unidade$sigla %in% c(db$sigla_unidade,"CSOB","IFB"),]$sigla
  if(nrow(db) < nrow(unidade)){
    db0 <- data.frame(sigla_unidade = db0, resultado = 0, stringsAsFactors = FALSE)
    db <- bind_rows(db,db0)}
  db$resultado <- as.numeric(format((db$mat_tipo/db$mat_total)*100,digits = 2))
  db$indicador <- indicador
  db$ano <- ano
  db <- db[order(db$sigla_unidade), c("indicador", "ano", "sigla_unidade","resultado")]
  return(db)
}
db2014 <- perc_matriculados(fonte = sistec_2014, ano = "2014", indicador = "1.2.2")
db2015 <- perc_matriculados(fonte = sistec_2015, ano = "2015", indicador = "1.2.2")
db2016 <- perc_matriculados(fonte = sistec_2016, ano = "2016", indicador = "1.2.2")
db2017 <- perc_matriculados(fonte = sistec_2017, ano = "2017", indicador = "1.2.2")
db2018 <- perc_matriculados(fonte = sistec_2018, ano = "2018", indicador = "1.2.2")
i122 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i122)

# 1.2.3 OK Percentual de alunos matriculados PROEJA por campus -------------------------------------------------------

db2014 <- perc_matriculados(fonte = sistec_2014, ano = "2014", indicador = "1.2.3")
db2015 <- perc_matriculados(fonte = sistec_2015, ano = "2015", indicador = "1.2.3")
db2016 <- perc_matriculados(fonte = sistec_2016, ano = "2016", indicador = "1.2.3")
db2017 <- perc_matriculados(fonte = sistec_2017, ano = "2017", indicador = "1.2.3")
db2018 <- perc_matriculados(fonte = sistec_2018, ano = "2018", indicador = "1.2.3")
i123 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i123)

# 1.2.4 OK Percentual de alunos matriculados técnico de nível médio por campus ---------------------------------------

db2014 <- perc_matriculados(fonte = sistec_2014, ano = "2014", indicador = "1.2.4")
db2015 <- perc_matriculados(fonte = sistec_2015, ano = "2015", indicador = "1.2.4")
db2016 <- perc_matriculados(fonte = sistec_2016, ano = "2016", indicador = "1.2.4")
db2017 <- perc_matriculados(fonte = sistec_2017, ano = "2017", indicador = "1.2.4")
db2018 <- perc_matriculados(fonte = sistec_2018, ano = "2018", indicador = "1.2.4")
i124 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i124)

# 1.3.1 OK Índice de projetos e programas articulados com ensino, pesquisa e extensão --------------------------------

articulacao_epe <- function(indicador){
  planilha <- ifelse(indicador == "1.3.1", 1, ifelse(indicador == "1.3.2", 2, ifelse(indicador == "1.3.3", 3, "")))
  db <- import_sheets(key_url = keys_rei[keys_rei$department == "PREN,PREX,PRPI",]$link,spreadsheet = planilha,col_isna = 1)
  colnames(db) <- c("descricao","area","sigla_unidade","ano")
  db$area <- gsub(pattern = "Articulado \\[Ensino Pesquisa e Extensão\\]",replacement = "ENPEEX",x = db$area)
  db <- db[!(is.na(db$descricao)),]
  db0 <- db %>%
    group_by(ano, sigla_unidade) %>%
    summarise(total = n())
  db1 <- db  %>%
    filter(area == "ENPEEX") %>%
    group_by(ano, sigla_unidade) %>%
    summarise(total_epe = n())
  db <- left_join(db0, db1, by = c("ano", "sigla_unidade"))
  if(any(is.na(db$total))) db[is.na(db$total),]$total <- 0
  if(any(is.na(db$total_epe))) db[is.na(db$total_epe),]$total_epe <- 0
  db0 <- db %>% 
    group_by(ano, sigla_unidade = "IFB") %>% 
    summarise_at(.vars = c("total","total_epe"), .funs = sum)
  db <- bind_rows(db, db0)
  db$resultado <- as.numeric(format((db$total_epe/db$total)*100,digits = 1))
  db$indicador <- indicador
  if (indicador == "1.3.1") db <- db[,c("indicador", "ano", "sigla_unidade", "resultado")]
  if (indicador == "1.3.2" || indicador == "1.3.3") {
    db <- db[,c("indicador", "ano", "sigla_unidade", "total_epe")]
    colnames(db) <- c("indicador", "ano", "sigla_unidade", "resultado")
  }
  return(db)
}
i131 <- articulacao_epe(indicador = "1.3.1")
print(i131)

# 1.3.2 OK Número de editais conjuntos ensino, pesquisa e extensão ---------------------------------------------------

i132 <- articulacao_epe(indicador = "1.3.2")
print(i132)

# 1.3.3 OK Número de seminários, feiras, fóruns e congressos articulados com ensino, pesquisa e extensão -------------

i133 <- articulacao_epe(indicador = "1.3.3")
print(i133)

# 2.1.1 OK Índice de participação da comunidade escolar nas políticas educacionais do Campus -------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i], import_sheets(key_url = keys_cam$link[i], spreadsheet = 5, col_isna = 1))
}
db <- bind_rows(CTAG, CSSB, CSAM, CRFI, CREM, CPLA, CGAM, CEST, CCEI, CBRA)
colnames(db) <- c("descricao","sigla_unidade","ano","avaliacao","observacao")
db <- db %>% 
  group_by(ano, sigla_unidade, avaliacao) %>% 
  summarise(total = n())
db[is.na(db$avaliacao),]$avaliacao <- "Nao"
db$avaliacao <- gsub(pattern = "[ã]",replacement = "a",x = db$avaliacao)
db <- tidyr::spread(db,avaliacao,total,fill = 0)
if(is.null(db$Nao)) db$Nao <- 0
if(is.null(db$Sim)) db$Sim <- 0
db0 <- db %>% 
  group_by(ano) %>% 
  summarise_at(.vars = c("Nao", "Sim"), .funs = sum) %>% 
  mutate(sigla_unidade = "IFB")
db <- bind_rows(db,db0)
db$resultado <- as.numeric(format((db$Sim/(db$Sim+db$Nao))*100, digits = 2))
db$indicador <- "2.1.1"
i211 <- db[,c("indicador", "ano", "sigla_unidade", "resultado")]
print(i1211)

# 2.1.2 ---- Índice de satisfação dos usuários e profissionais da educação ---------------------------------------------

# 2.1.3 OK Percentual de docentes com formação pedagógica ------------------------------------------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i], import_sheets(key_url = keys_cam$link[i], spreadsheet = 6, col_isna = 3))
}
db <- bind_rows(CTAG, CSSB, CSAM, CRFI, CREM, CPLA, CGAM, CEST, CCEI, CBRA)
colnames(db) <- c("ano","sigla_unidade","total_formacao","total","percentual")
db <- db[,c(1:4)]
db$total_formacao <- as.numeric(as.character(db$total_formacao))
db$total <- as.numeric(as.character(db$total))
db0 <- db %>%
  group_by(ano, sigla_unidade) %>%
  summarise_at(.vars = c("total_formacao", "total"), .funs = sum)
db1 <- db %>%
  group_by(ano, sigla_unidade = "IFB") %>%
  summarise_at(.vars = c("total_formacao", "total"), .funs = sum)
db <- bind_rows(db,db0)
db$resultado <- (db$total_formacao/db$total)*100
db$indicador <- "2.1.3"
i213 <- db[,c("indicador", "ano", "sigla_unidade", "resultado")]
print(i213)

# 2.1.4 OK Percentual de doutores em função dos docentes em efetivo exercício ----------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link,spreadsheet = 1,col_isna = 1)
colnames(db) <- c("ano","sigla_unidade","total_formacao","total","percentual")
db <- db[,c(1:4)]
db$total_formacao <- as.numeric(db$total_formacao)
db$total <- as.numeric(db$total)
db0 <- db %>%
  group_by(ano, sigla_unidade = "IFB") %>%
  summarise_at(.vars = c("total_formacao", "total"), .funs = sum)
db <- bind_rows(db,db0)
db$resultado <- (db$total_formacao/db$total)*100
db$indicador <- "2.1.4"
i214 <- db[,c("indicador", "ano", "sigla_unidade", "resultado")]
print(i214)

# 2.1.5 ---- Percentual de egressos que atuam no mercado de trabalho formal dentro de sua área de formação -------------

# 3.1.1 OK Número de parcerias de estágios vigentes ------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PREX",]$link,spreadsheet = 3,col_isna = 1)
colnames(db) <- c("descricao","inicio","fim","situacao")
db <- db[,c(1:3)]
db$inicio <- substr(db$inicio, nchar(db$inicio)-3, nchar(db$inicio))
db$fim <- substr(db$fim, nchar(db$fim)-3, nchar(db$fim))
db$fim <- gsub(pattern = "[a-z]|[A-Z]", replacement = "", x = db$fim)
db[db$fim == "", ]$fim <- 2030
db$inicio <- as.numeric(db$inicio)
db$fim <- as.numeric(db$fim)
db2014 <- db %>% filter(inicio < 2015, fim >= 2014) %>% group_by(ano = "2014") %>% summarise(resultado = n())
db2015 <- db %>% filter(inicio < 2016, fim >= 2015) %>% group_by(ano = "2015") %>% summarise(resultado = n())
db2016 <- db %>% filter(inicio < 2017, fim >= 2016) %>% group_by(ano = "2016") %>% summarise(resultado = n())
db2017 <- db %>% filter(inicio < 2018, fim >= 2017) %>% group_by(ano = "2017") %>% summarise(resultado = n())
db2018 <- db %>% filter(inicio < 2019, fim >= 2018) %>% group_by(ano = "2018") %>% summarise(resultado = n())
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$indicador <- "3.1.1"
db$sigla_unidade <- "IFB"
i311 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i311)

# 3.1.2 OK Número de parcerias internacionais ------------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "GAB-RIFB",]$link,spreadsheet = 1,col_isna = 1)
colnames(db) <- c("descricao","inicio","fim","situacao")
db <- db[,c(1:3)]
db$inicio <- substr(db$inicio, nchar(db$inicio)-3, nchar(db$inicio))
db$fim <- substr(db$fim, nchar(db$fim)-3, nchar(db$fim))
db$fim <- gsub(pattern = "[a-z]|[A-Z]", replacement = "", x = db$fim)
db[db$fim == "", ]$fim <- 2030
db$inicio <- as.numeric(db$inicio)
db$fim <- as.numeric(db$fim)
db2014 <- db %>% filter(inicio < 2015, fim >= 2014) %>% group_by(ano = "2014") %>% summarise(resultado = n())
db2015 <- db %>% filter(inicio < 2016, fim >= 2015) %>% group_by(ano = "2015") %>% summarise(resultado = n())
db2016 <- db %>% filter(inicio < 2017, fim >= 2016) %>% group_by(ano = "2016") %>% summarise(resultado = n())
db2017 <- db %>% filter(inicio < 2018, fim >= 2017) %>% group_by(ano = "2017") %>% summarise(resultado = n())
db2018 <- db %>% filter(inicio < 2019, fim >= 2018) %>% group_by(ano = "2018") %>% summarise(resultado = n())
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$indicador <- "3.1.2"
db$sigla_unidade <- "IFB"
i312 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i312)

# 3.1.3 OK Número de parcerias nacionais vigentes --------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PREX",]$link,spreadsheet = 4,col_isna = 1)
colnames(db) <- c("descricao","inicio","fim","situacao")
db <- db[,c(1:3)]
db$inicio <- substr(db$inicio, nchar(db$inicio)-3, nchar(db$inicio))
db$fim <- substr(db$fim, nchar(db$fim)-3, nchar(db$fim))
db$fim <- gsub(pattern = "[a-z]|[A-Z]", replacement = "", x = db$fim)
db[db$fim == "", ]$fim <- 2030
db$inicio <- as.numeric(db$inicio)
db$fim <- as.numeric(db$fim)
db2014 <- db %>% filter(inicio < 2015, fim >= 2014) %>% group_by(ano = "2014") %>% summarise(resultado = n())
db2015 <- db %>% filter(inicio < 2016, fim >= 2015) %>% group_by(ano = "2015") %>% summarise(resultado = n())
db2016 <- db %>% filter(inicio < 2017, fim >= 2016) %>% group_by(ano = "2016") %>% summarise(resultado = n())
db2017 <- db %>% filter(inicio < 2018, fim >= 2017) %>% group_by(ano = "2017") %>% summarise(resultado = n())
db2018 <- db %>% filter(inicio < 2019, fim >= 2018) %>% group_by(ano = "2018") %>% summarise(resultado = n())
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$indicador <- "3.1.3"
db$sigla_unidade <- "IFB"
i313 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i313)

# 3.1.4 OK Número de parcerias regionais vigentes --------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PREX",]$link,spreadsheet = 5,col_isna = 1)
colnames(db) <- c("descricao","inicio","fim","situacao")
db <- db[,c(1:3)]
db$inicio <- substr(db$inicio, nchar(db$inicio)-3, nchar(db$inicio))
db$fim <- substr(db$fim, nchar(db$fim)-3, nchar(db$fim))
db$fim <- gsub(pattern = "[a-z]|[A-Z]", replacement = "", x = db$fim)
db[db$fim == "", ]$fim <- 2030
db$inicio <- as.numeric(db$inicio)
db$fim <- as.numeric(db$fim)
db2014 <- db %>% filter(inicio < 2015, fim >= 2014) %>% group_by(ano = "2014") %>% summarise(resultado = n())
db2015 <- db %>% filter(inicio < 2016, fim >= 2015) %>% group_by(ano = "2015") %>% summarise(resultado = n())
db2016 <- db %>% filter(inicio < 2017, fim >= 2016) %>% group_by(ano = "2016") %>% summarise(resultado = n())
db2017 <- db %>% filter(inicio < 2018, fim >= 2017) %>% group_by(ano = "2017") %>% summarise(resultado = n())
db2018 <- db %>% filter(inicio < 2019, fim >= 2018) %>% group_by(ano = "2018") %>% summarise(resultado = n())
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$indicador <- "3.1.4"
db$sigla_unidade <- "IFB"
i314 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i314)

# 3.2.1 OK Número de eventos interinstitucionais promovidos por ano --------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PREN,PREX,PRPI",]$link,spreadsheet = 7,col_isna = 1)
colnames(db) <- c("descricao","tipo","ano","local")
db <- db %>% group_by(ano, sigla_unidade = "IFB") %>% summarise(resultado = n())
db$indicador <- "3.2.1"
i321 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i321)

# 3.3.1 OK Fórum institucional para discussão de políticas internas --------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "GAB-RIFB",]$link,spreadsheet = 2,col_isna = 1)
colnames(db) <- c("descricao","politica","ano")
db <- db %>% group_by(ano, sigla_unidade = "IFB") %>% summarise(resultado = n())
db$indicador <- "3.3.1"
i331 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i331)

# 3.3.2 OK Número conferências de avaliação do PDI -------------------------------------------------------------------

i332 <- tibble(
  indicador = c("3.3.2", "3.3.2", "3.3.2", "3.3.2", "3.3.2"),
  ano = c("2014", "2015", "2016", "2017", "2018"),
  sigla_unidade = c("IFB", "IFB", "IFB", "IFB", "IFB"),
  resultado = c(0, 0, 1, 0, 0)
)
print(i332)

# 3.3.3 OK Número de eventos relacionados à Gestão Democrática -------------------------------------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i],import_sheets(key_url = keys_cam$link[i],spreadsheet = 10,col_isna = 1))
}
db <- bind_rows(CTAG, CSSB, CSAM, CRFI, CREM, CPLA, CGAM, CEST, CCEI, CBRA)
colnames(db) <- c("descricao", "sigla_unidade", "ano", "observacao")
db0 <- db %>% group_by(ano, sigla_unidade) %>% summarise(resultado = n())
db1 <- db %>% group_by(ano, sigla_unidade = "IFB") %>% summarise(resultado = n())
db <- bind_rows(db0, db1)
db$indicador <- "3.3.3"
i333 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i333)

# 3.4.1 OK Eventos externos de divulgação ----------------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "DICOM",]$link, spreadsheet = 1, col_isna = 1)
colnames(db) <- c("descricao","inicio","fim","outro")
db <- db[,c(1:3)]
db$inicio <- substr(db$inicio, nchar(db$inicio)-3, nchar(db$inicio))
db$fim <- substr(db$fim, nchar(db$fim)-3, nchar(db$fim))
db$fim <- gsub(pattern = "[a-z]|[A-Z]", replacement = "", x = db$fim)
db[db$fim == "", ]$fim <- 2030
db$inicio <- as.numeric(db$inicio)
db$fim <- as.numeric(db$fim)
db2014 <- db %>% filter(inicio < 2015, fim >= 2014) %>% group_by(ano = "2014") %>% summarise(resultado = n())
db2015 <- db %>% filter(inicio < 2016, fim >= 2015) %>% group_by(ano = "2015") %>% summarise(resultado = n())
db2016 <- db %>% filter(inicio < 2017, fim >= 2016) %>% group_by(ano = "2016") %>% summarise(resultado = n())
db2017 <- db %>% filter(inicio < 2018, fim >= 2017) %>% group_by(ano = "2017") %>% summarise(resultado = n())
db2018 <- db %>% filter(inicio < 2019, fim >= 2018) %>% group_by(ano = "2018") %>% summarise(resultado = n())
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$indicador <- "3.4.1"
db$sigla_unidade <- "IFB"
i341 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i341)

# 3.4.2 OK Número de conselhos gestores implantados com minimamente quatro reuniões ordinárias anuais ----------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i],import_sheets(key_url = keys_cam$link[i],spreadsheet = 8,col_isna = 1))
}
db <- bind_rows(CTAG,CSSB,CSAM,CRFI,CREM,CPLA,CGAM,CEST,CCEI,CBRA)
colnames(db) <- c("sigla_unidade","ano_implantacao","2014","2015","2016","2017","2018")
db <- db[,c(1,3:7)]
db <- reshape2::melt(db, 
                     id.vars = c("sigla_unidade"), 
                     measure.vars = c("2014", "2015", "2016", "2017", "2018"), 
                     variable.name = "ano", 
                     value.name = "resultado", na.rm = FALSE, stringAsfactor = FALSE)
db$resultado <- as.numeric(db$resultado)
db <- db %>% group_by(sigla_unidade, ano) %>% summarise_at(.vars = "resultado", .funs = sum) %>% filter(resultado > 3)
db <- db %>% group_by(ano) %>% summarise(resultado = n())
db$sigla_unidade <- "IFB"
db$indicador <- "3.4.2"
i342 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i342)

# 3.4.3 OK Percentual de aumento do nº candidatos inscritos nos processos seletivos em relação ao ano anterior -------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "DICOM",]$link, spreadsheet = 2, col_isna = 1)
colnames(db) <- c("descricao","2013","2014","2015","2016","2017","2018")
db <- db[db$descricao == "GERAL", ]
db[, c(2:6)] <- apply(X = db[, c(2:6)], MARGIN = 2, FUN = function(x){gsub(pattern = ",", replacement = ".", x = x)})
db[, c(2:6)] <- apply(X = db[, c(2:6)], MARGIN = 2, FUN = as.numeric)
db$r2014 <- ((db$`2014` * 100) / db$`2013`) - 100
db$r2015 <- ((db$`2015` * 100) / db$`2014`) - 100
db$r2016 <- ((db$`2016` * 100) / db$`2015`) - 100
db$r2017 <- ((db$`2017` * 100) / db$`2016`) - 100
db$r2018 <- ((db$`2018` * 100) / db$`2017`) - 100
db <- reshape2::melt(db, 
                     id.vars = c("descricao"), 
                     measure.vars = c("r2014", "r2015", "r2016", "r2017"), 
                     variable.name = "ano", 
                     value.name = "resultado", na.rm = FALSE, factorsAsStrings = TRUE)
db$ano <- substr(x = db$ano, 2, 5)
db$sigla_unidade <- "IFB"
db$indicador <- "3.4.3"
i343 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i343)

# 3.5.1 OK Número de campus envolvidos com a elaboração das diretrizes de avaliação ----------------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i],import_sheets(key_url = keys_cam$link[i],spreadsheet = 9,col_isna = 1))
}
db <- bind_rows(CTAG,CSSB,CSAM,CRFI,CREM,CPLA,CGAM,CEST,CCEI,CBRA)
colnames(db) <- c("sigla_unidade","ano","descricao")
db <- db %>% filter(descricao == "SIM") %>% group_by(ano, sigla_unidade) %>% summarise(resultado = n())
if(nrow(db) == 0){
  i351 <- data.frame(indicador = c("3.5.1", "3.5.1", "3.5.1", "3.5.1", "3.5.1"),
                   ano = c("2014", "2015", "2016", "2017", "2018"),
                   sigla_unidade = "IFB",
                   resultado = 0,
                   stringsAsFactors = FALSE)
}else{
  db$indicador <- "3.5.1"
  i351 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
}
print(i351)

# 3.6.1 OK Eventos de avaliação institucional por campus -------------------------------------------------------------

for(i in 1:NROW(keys_cam)){
  assign(keys_cam$department[i],import_sheets(key_url = keys_cam$link[i],spreadsheet = 10,col_isna = 1))
}
db <- bind_rows(CTAG,CSSB,CSAM,CRFI,CREM,CPLA,CGAM,CEST,CCEI,CBRA)
colnames(db) <- c("eventos","sigla_unidade","ano","observacao")
db0 <- db %>% group_by(ano, sigla_unidade) %>% summarise(resultado = n())
db1 <- db %>% group_by(ano, sigla_unidade = "IFB") %>% summarise(resultado = n())
db <- bind_rows(db,db0)
db$indicador <- "3.6.1"
i361 <- db[,c("indicador", "ano","sigla_unidade","resultado")]
print(i361)

# 3.6.2 OK Percentual de colegiado que realizam auto avaliação anual -------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "GAB-RIFB",]$link,spreadsheet = 3,col_isna = 1)
colnames(db) <- c("ano", "colegiado", "realizacao")
db0 <- db %>% 
  group_by(ano) %>% 
  summarise(total = n())
db1 <- db %>% 
  filter(realizacao == "SIM") %>% 
  group_by(ano) %>% 
  summarise(total_realizado = n()) 
db <- left_join(db0, db1, by = "ano")
db$resultado <- (db$total_realizado * 100) / db$total
if(nrow(db) == 0){
  i362 <- data.frame(indicador = c("3.6.2", "3.6.2", "3.6.2", "3.6.2", "3.6.2"),
                     ano = c("2014", "2015", "2016", "2017", "2018"),
                     sigla_unidade = "IFB",
                     resultado = 0,
                     stringsAsFactors = FALSE)
}else{
  db$indicador <- "3.6.2"
  db$sigla_unidade <- "IFB"
  i362 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
}
print(i362)

# 3.6.3 OK Percentual de implantação e execução do sistema de avaliação global das práticas de gestão ----------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "GAB-RIFB",]$link,spreadsheet = 4,col_isna = 1)
colnames(db) <- c("ano", "resultado")
db$resultado <- gsub(pattern = "%", replacement = ".", x = db$resultado)
db$resultado <- gsub(pattern = ",", replacement = ".", x = db$resultado)
db$resultado <- as.numeric(db$resultado)
db$sigla_unidade <- "IFB"
db$indicador <- "3.6.3"
i363 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i363)

# 3.7.1 OK Percentual de alunos FIC para Técnicos --------------------------------------------------------------------

alunos_tipo_curso <- function(fonte, tp_curso){
  if (tp_curso == "FIC") tp_curso <- c("FORMACAO INICIAL", "FORMACAO CONTINUADA")
  else if (tp_curso == "TEC") tp_curso <- "TÉCNICO"
  else if (tp_curso == "SUP") tp_curso <- c("LICENCIATURA", "TECNOLOGIA", "BACHARELADO")
  else if (tp_curso == "POS") tp_curso <- c("ESPECIALIZACAO (LATO SENSU)")
  db <- fonte %>% 
    filter(tipo_curso %in% tp_curso & situacao_matricula %in% c("EM_CURSO","CONCLUÍDA") & matriculados == 1) %>% 
    group_by(sigla_unidade, tipo_curso, nome_ciclo, nome_aluno, numero_cpf) %>% 
    summarise_at(.vars = "matriculados", .funs = sum)
  db1 <- fonte %>% 
    filter(tipo_curso %in% tp_curso & situacao_matricula %in% c("EM_CURSO","CONCLUÍDA") & matriculados == 1) %>% 
    group_by(sigla_unidade = "IFB", tipo_curso, nome_ciclo, nome_aluno, numero_cpf) %>% 
    summarise_at(.vars = "matriculados", .funs = sum)
  db <- bind_rows(db, db1)
  return(db)
}
verticalizacao <- function(data1, data2, indicador, ano){
  db0 <- data1 %>% 
    group_by(sigla_unidade) %>% 
    summarise(total_tipo = n())
  db1 <- 
    semi_join(data1, data2, by = "numero_cpf") %>% 
    group_by(sigla_unidade) %>% 
    summarise(verticalizados = n())
  db <- inner_join(db1, db0, by = "sigla_unidade")
  db$resultado <- (db$verticalizados/db$total_tipo)*100
  if(nrow(db) == 0){
    db <- tibble(indicador = indicador, ano = ano, sigla_unidade = "IFB", resultado = 0, stringsAsFactors = FALSE)
  } else {
    db$indicador <- indicador
    db$ano <- ano
  }
  db <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
  return(db)
}
tipo <- c("FIC", "TEC", "SUP", "POS")
fonte <- c("sistec_2013", "sistec_2014", "sistec_2015", "sistec_2016", "sistec_2017", "sistec_2018")
data_rm <- as.vector("")
for(t in 1:length(tipo)){
  for(i in 1:length(fonte)){
    df <- paste0(tipo[t], substr(fonte[i], nchar(fonte[i])-3, nchar(fonte[i])))
    data_rm <- append(data_rm, df)
    assign(df, alunos_tipo_curso(fonte = get(fonte[i], envir = 1), tp_curso = tipo[t]))
  }
}
FICTEC_2014 <- verticalizacao(FIC2013, TEC2014, "3.7.1", "2014")
FICTEC_2015 <- verticalizacao(FIC2014, TEC2015, "3.7.1", "2015")
FICTEC_2016 <- verticalizacao(FIC2015, TEC2016, "3.7.1", "2016")
FICTEC_2017 <- verticalizacao(FIC2016, TEC2017, "3.7.1", "2017")
FICTEC_2018 <- verticalizacao(FIC2017, TEC2018, "3.7.1", "2018")
i371 <- bind_rows(FICTEC_2014, FICTEC_2015, FICTEC_2016, FICTEC_2017, FICTEC_2018)
rm(FICTEC_2014, FICTEC_2015, FICTEC_2016, FICTEC_2017, FICTEC_2018)
print(i371)

# 3.7.2 OK Percentual de alunos Nível Superior para Pós Graduação ----------------------------------------------------

SUPPOS_2014 <- verticalizacao(SUP2013, POS2014, "3.7.2", "2014")
SUPPOS_2015 <- verticalizacao(SUP2014, POS2015, "3.7.2", "2015")
SUPPOS_2016 <- verticalizacao(SUP2015, POS2016, "3.7.2", "2016")
SUPPOS_2017 <- verticalizacao(SUP2016, POS2017, "3.7.2", "2017")
SUPPOS_2018 <- verticalizacao(SUP2017, POS2018, "3.7.2", "2018")
i372 <- bind_rows(SUPPOS_2014, SUPPOS_2015, SUPPOS_2016, SUPPOS_2017, SUPPOS_2018)
rm(SUPPOS_2014, SUPPOS_2015, SUPPOS_2016, SUPPOS_2017, SUPPOS_2018)
print(i372)

# 3.7.3 OK Percentual de alunos Técnicos para Nível Superior ---------------------------------------------------------

TECSUP_2014 <- verticalizacao(TEC2013, SUP2014, "3.7.3", "2014")
TECSUP_2015 <- verticalizacao(TEC2014, SUP2015, "3.7.3", "2015")
TECSUP_2016 <- verticalizacao(TEC2015, SUP2016, "3.7.3", "2016")
TECSUP_2017 <- verticalizacao(TEC2016, SUP2017, "3.7.3", "2017")
TECSUP_2018 <- verticalizacao(TEC2017, SUP2018, "3.7.3", "2018")
i373 <- bind_rows(TECSUP_2014, TECSUP_2015, TECSUP_2016, TECSUP_2017, TECSUP_2018)
rm(TECSUP_2014, TECSUP_2015, TECSUP_2016, TECSUP_2017, TECSUP_2018)
print(i373)

rm(FIC2013, FIC2014, FIC2015, FIC2016, FIC2017, FIC2018, TEC2013, TEC2014, 
   TEC2015, TEC2016, TEC2017, TEC2018, SUP2013, SUP2014, SUP2015, SUP2016, 
   SUP2017, SUP2018, POS2013, POS2014, POS2015, POS2016, POS2017, POS2018)
# 3.8.1 OK Índice de eficácia - concluinte em função do nº de vagas ofertadas por turma ------------------------------

eficacia <- function(fonte, indicador, ano_referencia) {
  con <- fonte %>% 
    filter(year(dt_inicio_corrigida) <= (ano_referencia + 1), year(dt_final_corrigida) >= ano_referencia) %>% 
    group_by(sigla_unidade, nome_ciclo) %>% 
    summarise_at(.vars = "concluidos", .funs = sum)
  con0 <- tibble(sigla_unidade = "IFB", nome_ciclo = "IFB", concluidos = sum(con$concluidos, na.rm = TRUE))
  con <- bind_rows(con, con0)
  vag <- fonte %>% 
    filter(year(dt_inicio_corrigida) <= (ano_referencia + 1), year(dt_final_corrigida) >= ano_referencia) %>% 
    group_by(sigla_unidade, nome_ciclo) %>% 
    summarise_at(.vars = "vagas_ofertadas", .funs = max)
  vag0 <- tibble(sigla_unidade = "IFB", nome_ciclo = "IFB", vagas_ofertadas = sum(vag$vagas_ofertadas, na.rm = TRUE))
  vag <- bind_rows(vag, vag0)
  db <- inner_join(con, vag, by = c("sigla_unidade", "nome_ciclo"))
  db$resultado <- (db$concluidos/db$vagas_ofertadas)*100
  db$indicador <- indicador
  db$ano <- as.character(ano_referencia)
  db <- db[, c("indicador", "ano", "nome_ciclo", "sigla_unidade", "resultado")]
  return(db)
}

db2014 <- eficacia(sistec_2014, "3.8.1", 2014)
db2015 <- eficacia(sistec_2015, "3.8.1", 2015)
db2016 <- eficacia(sistec_2016, "3.8.1", 2016)
db2017 <- eficacia(sistec_2017, "3.8.1", 2017)
db2018 <- eficacia(sistec_2018, "3.8.1", 2018)
i381 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i381)

# 3.8.2 OK Percentual de alunos evadidos -----------------------------------------------------------------------------

evadidos <- function(fonte, indicador, ano){
  eva <- fonte %>% 
    group_by(sigla_unidade) %>% 
    summarise_at(.vars = "evadidos", .funs = sum)
  eva0 <- tibble(sigla_unidade = "IFB", evadidos = sum(eva$evadidos, na.rm = TRUE))
  eva <- bind_rows(eva, eva0)
  
  tot <- fonte %>% 
    group_by(sigla_unidade) %>% 
    summarise_at(.vars = "matriculados", .funs = sum)
  tot0 <- tibble(sigla_unidade = "IFB", matriculados = sum(tot$matriculados, na.rm = TRUE))
  tot <- bind_rows(tot, tot0)
  db <- inner_join(eva, tot, by = "sigla_unidade")
  db$resultado <- (db$evadidos/db$matriculados)*100
  db$indicador <- indicador
  db$ano <- as.character(ano)
  db <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
  return(db)
}
db2014 <- evadidos(sistec_2014, "3.8.2", 2014)
db2015 <- evadidos(sistec_2015, "3.8.2", 2015)
db2016 <- evadidos(sistec_2016, "3.8.2", 2016)
db2017 <- evadidos(sistec_2017, "3.8.2", 2017)
db2018 <- evadidos(sistec_2018, "3.8.2", 2018)
i382 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i382)

# 3.8.3 OK Percentual de alunos retidos ------------------------------------------------------------------------------

retidos <- function(fonte, indicador, ano){
  eva <- fonte %>% 
    group_by(sigla_unidade) %>% 
    summarise_at(.vars = "retidos", .funs = sum)
  eva0 <- tibble(sigla_unidade = "IFB", retidos = sum(eva$retidos, na.rm = TRUE))
  eva <- bind_rows(eva, eva0)
  
  tot <- fonte %>% 
    group_by(sigla_unidade) %>% 
    summarise_at(.vars = "matriculados", .funs = sum)
  tot0 <- tibble(sigla_unidade = "IFB", matriculados = sum(tot$matriculados, na.rm = TRUE))
  tot <- bind_rows(tot, tot0)
  db <- inner_join(eva, tot, by = "sigla_unidade")
  db$resultado <- (db$retidos/db$matriculados)*100
  db$indicador <- indicador
  db$ano <- as.character(ano)
  db <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
  return(db)
}
db2014 <- retidos(sistec_2014, "3.8.3", 2014)
db2015 <- retidos(sistec_2015, "3.8.3", 2015)
db2016 <- retidos(sistec_2016, "3.8.3", 2016)
db2017 <- retidos(sistec_2017, "3.8.3", 2017)
db2018 <- retidos(sistec_2018, "3.8.3", 2018)
i383 <- bind_rows(db2014, db2015, db2016, db2017, db2018)
print(i383)

# 4.1.1 OK Índice de exame periódico regularizado por ano ------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link, spreadsheet = 4, col_isna = 1)
names(db) <- c("nome", "siape", "nascimento", "idade", "outro1", "outro2", "2014", "2015", "2016", "2017", "2018")
db0 <- db %>% filter(`2014` == "Realizou") %>% summarise(realizou = n())
db1 <- db %>% filter(!`2014` == "Realizou") %>% summarise(nao_realizou = n())
db2014 <- bind_cols(db0, db1)
db2014$ano <- "2014"
db0 <- db %>% filter(`2015` == "Realizou") %>% summarise(realizou = n())
db1 <- db %>% filter(!`2015` == "Realizou") %>% summarise(nao_realizou = n())
db2015 <- bind_cols(db0, db1)
db2015$ano <- "2015"
db0 <- db %>% filter(`2016` == "Realizou") %>% summarise(realizou = n())
db1 <- db %>% filter(!`2016` == "Realizou") %>% summarise(nao_realizou = n())
db2016 <- bind_cols(db0, db1)
db2016$ano <- "2016"
db0 <- db %>% filter(`2017` == "Realizou") %>% summarise(realizou = n())
db1 <- db %>% filter(!`2017` == "Realizou") %>% summarise(nao_realizou = n())
db2017 <- bind_cols(db0, db1)
db2017$ano <- "2017"
db0 <- db %>% filter(`2018` == "Realizou") %>% summarise(realizou = n())
db1 <- db %>% filter(!`2018` == "Realizou") %>% summarise(nao_realizou = n())
db2018 <- bind_cols(db0, db1)
db2018$ano <- "2018"
db <- bind_rows(db2014, db2015, db2016, db2017, db2018)
db$sigla_unidade <- "IFB"
db$resultado <- (db$realizou / (db$realizou + db$nao_realizou)) * 100
db$indicador <- "4.1.1"
i411 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i411)

# 4.1.2 OK Índice de execução do orçamento com capacitação -----------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link, spreadsheet = 5, col_isna = 1)
colnames(db) <- c("ano", "previsto", "realizado", "resultado")
db$previsto <- gsub("[,]", "", db$previsto)
db$previsto <- as.numeric(db$previsto)
db$realizado <- gsub("[,]", "", db$realizado)
db$realizado <- as.numeric(db$realizado)
db$resultado <- (db$realizado / db$previsto) *100
db$sigla_unidade <- "IFB"
db$indicador <- "4.1.2"
i412 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i412)

# 4.1.3 OK Índice de participação de servidores em eventos de capacitação --------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link, spreadsheet = 6, col_isna = 1)
colnames(db) <- c("ano", "total", "capacitados", "resultado")
db$total <- gsub("[,]", "", db$total)
db$total <- as.numeric(db$total)
db$capacitados <- gsub("[,]", "", db$capacitados)
db$capacitados <- as.numeric(db$capacitados)
db$resultado <- (db$capacitados / db$total) *100
db$sigla_unidade <- "IFB"
db$indicador <- "4.1.3"
i413 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i413)

# 4.1.4 OK Índice de qualificação dos servidores no ano (Docente) ----------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link, spreadsheet = 7, col_isna = 1)
colnames(db) <- c("ano", "total", "qualificados", "resultado")
db$total <- gsub("[,]", "", db$total)
db$total <- as.numeric(db$total)
db$qualificados <- gsub("[,]", "", db$qualificados)
db$qualificados <- as.numeric(db$qualificados)
db$resultado <- (db$qualificados / db$total) *100
db$sigla_unidade <- "IFB"
db$indicador <- "4.1.4"
i414 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i414)

# 4.1.5 OK Índice de qualificação dos servidores no ano (TAE) --------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRGP",]$link, spreadsheet = 8, col_isna = 1)
colnames(db) <- c("ano", "total", "qualificados", "resultado")
db$total <- gsub("[,]", "", db$total)
db$total <- as.numeric(db$total)
db$qualificados <- gsub("[,]", "", db$qualificados)
db$qualificados <- as.numeric(db$qualificados)
db$resultado <- (db$qualificados / db$total) *100
db$sigla_unidade <- "IFB"
db$indicador <- "4.1.5"
i415 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i415)

# 4.2.1 OK Percentual de elaboração do plano -------------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRAD",]$link, spreadsheet = 1, col_isna = 1)
colnames(db) <- c("ano", "resultado")
db$resultado <- gsub("%", "", db$resultado)
db$resultado <- gsub(",", "", db$resultado)
db$resultado <- as.numeric(db$resultado)
db$sigla_unidade <- "IFB"
db$indicador <- "4.2.1"
i421 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i421)

# 4.2.2 OK Percentual de execução do plano ---------------------------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "PRAD",]$link, spreadsheet = 2, col_isna = 1)
colnames(db) <- c("ano", "resultado")
db$resultado <- gsub("%", "", db$resultado)
db$resultado <- gsub(",", "", db$resultado)
db$resultado <- as.numeric(db$resultado)
db$sigla_unidade <- "IFB"
db$indicador <- "4.2.2"
i422 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i422)

# 4.3.1 ---- Índice de execução do orçamento de Assistência ao Educando ------------------------------------------------

#### SEM COLETA VIA GOOGLE DRIVE

# 4.3.2 ---- Índice de execução do orçamento do NAPNE ------------------------------------------------------------------

#### SEM COLETA VIA GOOGLE DRIVE

# 4.3.3 ---- Número de eventos de planejamento orçamentário e participativo por campus ---------------------------------

#### SEM COLETA VIA GOOGLE DRIVE

# 4.4.1 OK Percentual de Campus com cabeamento estruturado implantado ------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "DTIC",]$link, spreadsheet = 1, col_isna = 1)
colnames(db) <- c("ano", "sigla_unidade", "resultado")
db0 <- db %>% filter(resultado == "Não") %>% group_by(ano) %>% summarise(nao = n())
db <- db %>% filter(resultado == "Sim") %>% group_by(ano) %>% summarise(sim = n()) %>% left_join(db0, by = "ano")
db[is.na(db$nao),]$nao <- 0
db$resultado <- (db$sim / (db$sim + db$nao))*100
db$indicador <- "4.4.1"
db$sigla_unidade <- "IFB"
i441 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i441)

# 4.4.2 OK Percentual de Campus com link de internet ativado ---------------------------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "DTIC",]$link, spreadsheet = 2, col_isna = 1)
colnames(db) <- c("ano", "sigla_unidade", "resultado")
db0 <- db %>% filter(resultado == "Não") %>% group_by(ano) %>% summarise(nao = n())
db <- db %>% filter(resultado == "Sim") %>% group_by(ano) %>% summarise(sim = n()) %>% left_join(db0, by = "ano")
db[is.na(db$nao),]$nao <- 0
db$resultado <- (db$sim / (db$sim + db$nao))*100
db$indicador <- "4.4.2"
db$sigla_unidade <- "IFB"
i442 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i442)

# 4.4.3 OK Percentual de processos administrativos e acadêmicos informatizados ---------------------------------------

db <- import_sheets(key_url = keys_rei[keys_rei$department == "DTIC",]$link, spreadsheet = 3, col_isna = 1)
colnames(db) <- c("processos", "resultado", "ano")
db0 <- db %>% filter(resultado == "Não") %>% group_by(ano) %>% summarise(nao = n())
db <- db %>% filter(resultado == "Sim") %>% group_by(ano) %>% summarise(sim = n()) %>% left_join(db0, by = "ano")
db[is.na(db$nao),]$nao <- 0
db$resultado <- (db$sim / (db$sim + db$nao))*100
db$indicador <- "4.4.3"
db$sigla_unidade <- "IFB"
i443 <- db[, c("indicador", "ano", "sigla_unidade", "resultado")]
print(i443)

# consolidação ------------------------------------------------------------
bsc <- readRDS("data/bsc.rds")
ind <- unique(bsc$Cod_Indicador)
ind <- gsub("[.]","",ind)
ind <- paste0("i",ind)
fl <- list()
for(i in 1:length(ind)){
  print(c(ind[i], exists(ind[i])))
}
rm(i, import_sheets)
data_session <- ls(pattern = "^i")
save(list = data_session, file = "data/ind_pdi.rda")

sink()
  
  