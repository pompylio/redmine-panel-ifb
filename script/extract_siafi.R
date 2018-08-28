library(googledrive)
library(openxlsx)
#library(stringi)

setwd("~/R/project/app-plan/data/")
files <- drive_ls(path = "GmailToDrive",q = "name contains 'orcamento'",recursive = TRUE)
files <- files[order(files$name),]
drive_download(file = files$name[length(files$name)])

siafi <-  read.xlsx(files$name[length(files$name)],sheet = 1,startRow = 3,rowNames = FALSE,colNames = FALSE)
tit_orc <- read.xlsx(files$name[length(files$name)],sheet = 1,startRow = 1,rows = 1,rowNames = FALSE,colNames = TRUE,check.names = TRUE)
tit_orc <- tit_orc[,c(16:34)]
tit_orc <- gsub(pattern = "[.]",replacement = "_",names(tit_orc))
names(siafi) <- c("DATA","CO_UO","NO_UO","CO_UGE","NO_UGE","CO_UGR","NO_UGR","CO_CATEGORIA_ECONOMICA","NO_CATEGORIA_ECONOMICA","CO_GRUPO_DESPESA","NO_GRUPO_DESPESA","CO_MODALIDADE_APLICACAO","NO_MODALIDADE_APLICACAO","CO_ELEMENTO_DESPESA","NO_ELEMENTO_DESPESA","CO_SUBITEM","NO_SUBITEM","CO_ACAO_GOVERNO","NO_ACAO_GOVERNO","PTRES","CO_NATUREZA_DESPESA","NO_NATUREZA_DESPESA","CO_NATUREZA_DESPESA_DETALHADA","NO_NATUREZA_DESPESA_DETALHADA","CO_PLANO_INTERNO","NO_PLANO_INTERNO",tit_orc)
rm(tit_orc)
siafi$CO_SUBITEM <- ifelse(siafi$CO_SUBITEM %in% c("0","1","2","3","4","5","6","7","8","9"),paste0("0",siafi$CO_SUBITEM),siafi$CO_SUBITEM)
siafi$ANO <- substr(siafi$DATA,1,4)
siafi$DATA <- factor(siafi$DATA)
siafi$CO_PLANO_INTERNO <- gsub(pattern = "-8",replacement = "8",siafi$CO_PLANO_INTERNO)
siafi$CO_UGR <- ifelse(siafi$CO_UGR == "-8",siafi$CO_UGE,siafi$CO_UGR)
siafi$NO_UGR <- ifelse(siafi$NO_UGR == "SEM INFORMACAO",siafi$NO_UGE,siafi$NO_UGR)
siafi$UO_SIGLA  <- ifelse(siafi$CO_UO == "26428","IFB",ifelse(siafi$CO_UGR %in% c("152139","152140","152141","152142","152143","152144","152145","152146","152147","155145","155150","155151","158501","158143") |siafi$CO_UGE %in% c("152139","152140","152141","152142","152143","152144","152145","152146","152147","155145","155150","155151","158501","158143"),"IFB",siafi$NO_UO))
siafi$UGE_SIGLA <- ifelse(siafi$CO_UGE == "152139", "CGAM",ifelse(siafi$CO_UGE == "152140", "CTAG",ifelse(siafi$CO_UGE == "152141", "CSAM",ifelse(siafi$CO_UGE == "152142", "CBRA",ifelse(siafi$CO_UGE == "152143", "CTGC",ifelse(siafi$CO_UGE == "152144", "CSSB",ifelse(siafi$CO_UGE == "152145", "CCEI",ifelse(siafi$CO_UGE == "152146", "CEST",ifelse(siafi$CO_UGE == "152147", "CRFI",ifelse(siafi$CO_UGE == "155145", "CBRZ",ifelse(siafi$CO_UGE == "155150", "CCAN",ifelse(siafi$CO_UGE == "155151", "CSOB",ifelse(siafi$CO_UGE == "158501", "CPLA",ifelse(siafi$CO_UGE == "158143", "REITORIA",ifelse(siafi$CO_UGE %in% c("010090","030203"),"EXTERNO","OUTROS")))))))))))))))
siafi$UGR_SIGLA <-  ifelse(siafi$CO_UGR == "152139", "CGAM",ifelse(siafi$CO_UGR == "152140", "CTAG",ifelse(siafi$CO_UGR == "152141", "CSAM",ifelse(siafi$CO_UGR == "152142", "CBRA",ifelse(siafi$CO_UGR == "152143", "CTGC",ifelse(siafi$CO_UGR == "152144", "CSSB",ifelse(siafi$CO_UGR == "152145", "CCEI",ifelse(siafi$CO_UGR == "152146", "CEST",ifelse(siafi$CO_UGR == "152147", "CRFI",ifelse(siafi$CO_UGR == "155145", "CBRZ",ifelse(siafi$CO_UGR == "155150", "CCAN",ifelse(siafi$CO_UGR == "155151", "CSOB",ifelse(siafi$CO_UGR == "158501", "CPLA",ifelse(siafi$CO_UGR == "158143", "REITORIA","OUTROS"))))))))))))))
siafi$CLASSIFICACAO <-
  ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("14","33"),"DIARIAS E PASSAGENS",
         ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("18","20"),"AUXILIO FINANCEIRO",
                ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("30"),"MATERIAL DE CONSUMO",
                       ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("31"),"OUTROS MATERIAIS",
                              ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("32"),"OUTROS MATERIAIS",
                                     ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("35"),"OUTROS SERVICOS",
                                            ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36"),"OUTROS SERVICOS",
                                                   ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("37"),"OUTROS SERVICOS",
                                                          ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("39"),"OUTROS SERVICOS",
                                                                 ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("47"),"TRIBUTOS E OBRIGACOES",
                                                                        ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("51"),"OBRAS INSTALACOES",
                                                                               ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("52"),"MATERIAL PERMANENTE",""))))))))))))

siafi$CLASSIFICACAO <-
  ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("30") & siafi$CO_SUBITEM %in% c("17","47") |
           siafi$CO_ELEMENTO_DESPESA %in% c("35") & siafi$CO_SUBITEM %in% c("04") |
           siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("46") & siafi$CO_GRUPO_DESPESA %in% c("4") |
           siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("54","57") |
           siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("09","27","28") & siafi$CO_GRUPO_DESPESA %in% c("3") |
           siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("92","93") & siafi$CO_GRUPO_DESPESA %in% c("4") |
           siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("08","11","26","27","28","30","31","56") & siafi$CO_GRUPO_DESPESA %in% c("3") |
           siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("57","97") |
           siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("92","93","95") & siafi$CO_GRUPO_DESPESA %in% c("4") |
           siafi$CO_ELEMENTO_DESPESA %in% c("52") & siafi$CO_SUBITEM %in% c("35")& siafi$CO_GRUPO_DESPESA %in% c("4"),
         "TECNOLOGIA DA INFORMACAO",
         ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("07","26","35") |
                  siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("01","19","08") |
                  siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("79"),
                "APOIO ADMINISTRATIVO",
                ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("15") |
                         siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("16","17") & siafi$CO_GRUPO_DESPESA %in% c("3") |
                         siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("10","12","14") |
                         siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("13") & siafi$CO_GRUPO_DESPESA %in% c("4"),
                       "LOCACAO DE BENS",
                       ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("18","20","21","22") |
                                siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("19") & siafi$CO_GRUPO_DESPESA %in% c("4") |
                                siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("04","06") & siafi$CO_GRUPO_DESPESA %in% c("3") |
                                siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("18") & siafi$CO_GRUPO_DESPESA %in% c("4") |
                                siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("19") & siafi$CO_GRUPO_DESPESA %in% c("3") |
                                siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("16","17","20","21"),
                              "MANUTENCAO E CONSERVACAO",
                              ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("28") |
                                       siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("48"),
                                     "SELECAO E TREINAMENTO",
                                     ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("19") & siafi$CO_GRUPO_DESPESA %in% c("3") |
                                              siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("03")|
                                              siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("77"),
                                            "VIGILANCIA OSTENSIVA",
                                            ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("25") |
                                                     siafi$CO_ELEMENTO_DESPESA %in% c("37") & siafi$CO_SUBITEM %in% c("02") |
                                                     siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("78"),
                                                   "LIMPEZA E CONSERVACAO",
                                                   ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("27") |
                                                            siafi$CO_ELEMENTO_DESPESA %in% c("36") & siafi$CO_SUBITEM %in% c("59","63") & siafi$CO_GRUPO_DESPESA %in% c("3")|
                                                            siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("47","58","59") |
                                                            siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("49","91","92","93") & siafi$CO_GRUPO_DESPESA %in% c("3"),
                                                          "COMUNICACAO",
                                                          ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("44"),
                                                                 "AGUA E ESGOTO",
                                                                 ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("90"),
                                                                        "COMUNICACAO OFICIAL",
                                                                        ifelse(siafi$CO_ELEMENTO_DESPESA %in% c("39") & siafi$CO_SUBITEM %in% c("43") |
                                                                                 siafi$CO_ELEMENTO_DESPESA %in% c("47") & siafi$CO_SUBITEM %in% c("22"),
                                                                               "ENERGIA ELETRICA",
                                                                               siafi$CLASSIFICACAO)))))))))))
siafi[,sapply(siafi, class) == "character"] <- apply(X = siafi[,sapply(siafi, class) == "character"],MARGIN = 2,FUN = function(y){iconv(x = y,from = "latin1",to = "UTF-8")})
saveRDS(siafi,"~/R/project/app-plan/data/siafi.rds")
unlink(x = files$name[length(files$name)])
