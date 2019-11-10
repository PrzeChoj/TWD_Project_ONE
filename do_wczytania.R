# kod/wczytanie_danych.R
library("haven")
library("stringr")

# funkcja do wszystkich danych
create_data_from_sas_format <- function(dictionary, file) {
  dane.dic <- file.path(getwd(), "dane")
  
  stopifnot(is.character(dictionary), is.character(file))
  
  path <- file.path(dane.dic, dictionary, file)
  new_data <- read_sas(path)
  new_data
}






# BGR_schools.R

#####
# skrypt zawiera analize danych poswiecona szkolom w Wielkiej Brytanii
# glownym zalozeniem jest skupienie sie na podziale szkol ze wzgledu na plcie
#####

library(dplyr)
library(data.table)
library(stringi)
library(ggplot2)
library(haven)

## wczytanie danych z pliku zawierajacego znaczenie oznaczen

#kolumna STRATUM zawiera informacje o szkole - country + region + id school
#bedziemy potrzbowac oznaczen danych z tej kolumny, wiec wczytujemy je z pliku z oznaczeniami

#sciezka do tego pliku
file <- file.path("dane" ,"PUF_SAS_COMBINED_CMB_SCH_QQQ", "CY6_MS_CMB_SCH_QQQ.sas7bdat.format.sas")
#poczatek wczytywania danych - od tego nr wiersza zaczyna sie kolumna STRATUM
first_line <- grep("value \\$STRATUM", readLines(file))
#szukamy konca wczytywania - w pliku z oznaczeniami koniec kolumny oznaczany jest przez ";"
tmp <- grep(";", readLines(file))
end_line <- tmp[tmp > first_line] #to jest nr wiersza posiadajacy ";" po kolumnie statum
#wczytanie oznaczen
Labels <- read.table(file,sep = "=", skip=first_line,nrows=(end_line[1]-first_line-1), stringsAsFactors = FALSE) %>% as.data.table()
colnames(Labels) <- c("STRATUM", "Label")
#interesuje nas tylko Wielka Brytania, czyli GBR1...
GBR_schools <- Labels[grep("GBR1", Labels$STRATUM)]

#kolumne Label chcemy podzielic na oddzielne kolumny o jednej informacji
GBR_Info <- stri_split_fixed(GBR_schools$Label, ",") %>% as.data.table() %>% t()
rownames(GBR_Info) <- NULL
colnames(GBR_Info) <- c("region", "type", "subregion", "sex")
#laczymy DF GBR_Info z GBR_schools - pozwywamy sie kolumny label na rzecz oddzielonych informacji
GBR_schools <- cbind(GBR_schools[,1], GBR_Info)

head(GBR_schools, 50)

##obrobmy troche te DF
#okazuje sie ze po id pojawia sie spacja, wiec usuwamy ja juz recznie
GBR_schools$STRATUM <- GBR_schools$STRATUM %>% substr(1, 7)
#z regionu obchodzi nas jedynie sama nazwa, wiec poprawiwamy to
GBR_schools$region <- stri_extract_all(GBR_schools$region, regex="(?<=: ).*") %>% unlist()
#w typie szkol przy plci obchodzi nas jedynie pierwsze slowo
GBR_schools$sex <- stri_extract_first_words(GBR_schools$sex)

## analiza przy wykorzystaniu uzyskanych powyzej danych

#wczytujemy plik z danymi z kwestionariusza o uczniach
Student <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qqq.sas7bdat") %>% as.data.table()

#CNTSCHID zawiera id szkoly $3$
#CNTSTUID zawiera id ucznia $4$
#ST004D01T zawiera info o plci
#STRATUM $8$
#PV[1-10][przedmiot] dane o wynikach - nas interesuje jedynie PV1 z kazdego przedmiotu
tmp <- Student[, .(CNTSCHID, CNTSTUID, ST004D01T, STRATUM, PV1MATH, PV1READ, PV1SCIE)]

setkey(tmp, STRATUM)
setkey(GBR_schools, STRATUM)
GBR_results <- tmp[GBR_schools]
GBR_results <- GBR_results[which(!(GBR_results$CNTSCHID %>% is.na)), ]







# skrypty
source(file = file.path("/Users", "dtgt", "Desktop", "programiki", "R", "projekty", "1", "PISA", "kod", "skrypty.R"))





# wczytanie_danych.R

# potrzebne:
# GBR
# GBR_results

# wczytanie:
GBR.path <- file.path(getwd(), "dane", "GBR.csv")
GBR <- read.csv(GBR.path)

# pierwsza czesc
GBR_results$fullregion <- paste(GBR_results$region, GBR_results$subregion)
map <- cbind(GBR_results$fullregion %>% unique, rep(0, times=12), rep(0, times=12))
colnames(map) <- c("fullregion", "rainregion", "deszcz")
map[1,2] <- "North East England"
map[2,2] <- "Central England"
map[3,2] <- "South East England"
map[4,2] <- "Central England"
map[5,2] <- "Northern Ireland"
map[6,2] <- "Northern Ireland"
map[7,2] <- "Northern Ireland"
map[8,2] <- "Northern Ireland"
map[9,2] <- "Northern Ireland"
map[10,2] <- "North West England & Wales"
map[11,2] <- "South West England & Wales"
map[12,2] <- "South West England & Wales"

# dane z deszczu
# https://www.metoffice.gov.uk/hadobs/hadukp/data/download.html

url <- "https://www.metoffice.gov.uk/hadobs/hadukp/data/download.html"
strona <- readLines(url)
# map[,2] %>% unique # takich szukam
indexy_danych <- which(!(strona %>% stri_extract_all_regex("seasonal/") %>% unlist %>% is.na()))

pattern <- map[,2] %>% unique() %>% paste(collapse = "|")
pattern <- paste("(", pattern, ")", sep="")
indexy_danych <- indexy_danych[which(!(strona[indexy_danych-2] %>% stri_extract_all_regex(pattern) %>% unlist %>% is.na()))]

koncowki <- strona[indexy_danych] %>% stri_extract_all_regex("seasonal[^\"]*") %>% unlist
url_dane <- paste("https://www.metoffice.gov.uk/hadobs/hadukp/data/", koncowki, sep="")

deszcz <- rep(0.1, 6)
for(i in 1:6){
  strona_dane <- readLines(url_dane[i])
  deszcz[i] <- strona_dane %>% tail(12) %>% head(7) %>% substr(34, 39) %>% as.numeric() %>% sum
}

map[1,3] <- deszcz[5]
map[2,3] <- deszcz[3]
map[3,3] <- deszcz[1]
map[4,3] <- deszcz[3]
map[5,3] <- deszcz[6]
map[6,3] <- deszcz[6]
map[7,3] <- deszcz[6]
map[8,3] <- deszcz[6]
map[9,3] <- deszcz[6]
map[10,3] <- deszcz[4]
map[11,3] <- deszcz[2]
map[12,3] <- deszcz[2]

map <- map %>% tbl_df
GBR_results <- GBR_results %>% inner_join(map, by="fullregion") # nie bylo oryginalnie zrobione i dodane do GBR.csv!

#



# druga czesc
GBR$fullregion <- paste(GBR$region, GBR$subregion, sep=" ")
GBR <- GBR %>% inner_join(map, by="fullregion")





# tworzenie_danych.R
# jakie jeszcze tam sa ciekawe rzeczy nieopisane w formularzach?
tmp_duzy <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qqq.sas7bdat") # 519334x921
nr_wspolnych <- which(tmp_duzy %>% colnames %in% (GBR %>% colnames))

label_wspolnych <- character(length = length(nr_wspolnych))
names_wspolnych <- (tmp_duzy %>% colnames)[nr_wspolnych]

for( i in 1:length(nr_wspolnych) ){
  label_wspolnych[i] <- attr(tmp_duzy[[nr_wspolnych[i]]],"label")
}
label_wspolnych <- cbind(nr_wspolnych, label_wspolnych, names_wspolnych) %>% tbl_df()

# 668, 669-672, 78-80, 86-87, 91-98, 99-104, 116-118, 119-123, 246?, 311, 328-337, 379-390, 391-403, 482-488, 518-519, 604-605, 637, 638
ciekawe <- label_wspolnych[c(668, 669:672, 78:80, 86:87, 91:98, 99:104, 116:118, 119:123, 311, 328:337, 379:390, 391:403, 482:488, 518:519, 604:605, 637, 638),]

# wesmy tylko ciekawe dane
tbl_ciekawe <- GBR %>% select(ciekawe$names_wspolnych, sex, region, CNTSTUID.x, CNTSCHID)

# usunmy te kolumny, co maja tylko NA
czy.na <- logical(length(colnames(tbl_ciekawe)))
for(i in 1:(length(colnames(tbl_ciekawe))-4)){
  czy.na[i] <- tbl_ciekawe[,i] %>% is.na %>% all
  attr(tbl_ciekawe[[i]], "opis") <- ciekawe[[i, 2]]
}

tbl_ciekawe <- tbl_ciekawe %>% select(which(!czy.na))



# co ciekawego jest w szkolnych?
School <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_SCH_QQQ", "cy6_ms_cmb_sch_qqq.sas7bdat") %>% as.data.table()
label_szkolny <- character(length = length(colnames(School)))
names_szkolny <- School %>% colnames
for( i in 1:length(colnames(School)) ){
  label_szkolny[i] <- attr(School[[i]],"label")
}
# 3, 13:14, 124, 138:139
index <- 1:273
names(index) <- "index"
label_szkolny <- cbind(index, label_szkolny, names_szkolny) %>% tbl_df() %>% filter(index %in% c(3, 13:14, 124, 138:139))

tbl_szkolny <- School %>% tbl_df %>% select(label_szkolny[[3]])
# School$SC013Q01TA # 1-publiczna, 2-prywatna


# polaczny tbl_ciekawe z tbl_szkolny
tbl_szkolny$CNTSCHID <- tbl_szkolny$CNTSCHID %>% as.integer() # to musi byc integer
tbl_ciekawe <- tbl_ciekawe %>% inner_join(tbl_szkolny, by="CNTSCHID")
#




# POL
POL <- tmp_duzy %>% filter(CNT == "POL") %>% select(ciekawe$names_wspolnych, CNTSTUID, CNTSCHID)
# polaczny tbl_ciekawe z tbl_szkolny
POL$CNTSCHID <- POL$CNTSCHID %>% as.double() # to musi byc double
tbl_ciekawe_POL <- POL %>% inner_join(tbl_szkolny, by="CNTSCHID")






# jeszcze wieksza ramka 05.11.19 + poprawka 7.11.19 + dodatek 10.11.19
col <- tbl_ciekawe %>% colnames
col_ext <- c(col, "ST103Q01NA", "ST127Q01TA", "ST004D01T", "ST123Q01NA", "ESCS", "WEALTH", "EC028Q01NA", "EC028Q02NA", "EC028Q03NA", "ADDSUM") # rozszezony o 5 kolumn i 3 o chodzeniu na korki
# zamienie kolejnosc joina: najpierw GBR_results z tbl_ciekawe. Potem z tmp
tmp <- tmp_duzy %>% select(CNTSTUID, ST103Q01NA, ST127Q01TA, ST004D01T, ST123Q01NA, ESCS, WEALTH, EC028Q01NA, EC028Q02NA, EC028Q03NA)

tbl_ciekawe$CNTSCHID <- as.double(tbl_ciekawe$CNTSCHID)
tmp_ciekawe <- tbl_ciekawe %>% full_join(GBR_results, by="CNTSCHID") %>% filter(!duplicated(CNTSTUID))
# czy tmp_ciekawe jest ok? Tak jest ok
# polaczmy wiec z tmp
tbl_ciekawe_GBR_ext <- tmp_ciekawe %>% inner_join(tmp, by="CNTSTUID")

# dodatkowe info:
# "Mantained" to tylko publiczne, "mantained not selective" tylko publiczne,
# "mantained selective" to i publiczne i prywatne
# akademie to publiczne
tbl_ciekawe_GBR_ext$SC013Q01TA <- ifelse(tbl_ciekawe_GBR_ext$type=="Maintained", 1, tbl_ciekawe_GBR_ext$SC013Q01TA)
tbl_ciekawe_GBR_ext$SC013Q01TA <- ifelse(tbl_ciekawe_GBR_ext$type=="maintained non-selective", 1, tbl_ciekawe_GBR_ext$SC013Q01TA)
tbl_ciekawe_GBR_ext$SC013Q01TA <- ifelse(tbl_ciekawe_GBR_ext$type=="academy", 2, tbl_ciekawe_GBR_ext$SC013Q01TA)

tbl_ciekawe_GBR_ext$type <- ifelse(tbl_ciekawe_GBR_ext$type == "academy", "akademicka", ifelse(tbl_ciekawe_GBR_ext$type == "independent", "niezalezna", "utrzymywana"))
# czy tbl_ciekawe_GBR_ext jest ok? tak jest ok
tbl_ciekawe_POL_ext <- tbl_ciekawe_POL %>% inner_join(tmp, by=c("CNTSTUID" = "CNTSTUID"))
#


# dodanie kolumny ADDSUM informujacej o dodatkowych(ADDitional)(pozalekcyjnych) zajeciach
tbl_ciekawe_GBR_ext <- tbl_ciekawe_GBR_ext %>% mutate(ADDSUM = (EC028Q01NA == 2 | EC028Q02NA == 2 | EC028Q03NA == 2))
tbl_ciekawe_POL_ext <- tbl_ciekawe_POL_ext %>% mutate(ADDSUM = (EC028Q01NA == 2 | EC028Q02NA == 2 | EC028Q03NA == 2))













# kobieta/mezczyzna pomaga mi w nauce
pomagaGBR <- numeric(2)
pomagaPOL <- numeric(2)
tmp <- tbl_ciekawe_GBR_ext %>% select(EC030Q01NA, CNTSTUID.x) %>% filter(!duplicated(CNTSTUID.x)) %>% filter(!is.na(EC030Q01NA))
pomagaGBR[1] <- ifelse(rep(TRUE, times = length(tmp$EC030Q01NA)), tmp$EC030Q01NA-1, NA) %>% mean # w 43% domow w GBR kobieta pomaga
tmp <- tbl_ciekawe_POL_ext %>% select(EC030Q01NA, CNTSTUID) %>% filter(!duplicated(CNTSTUID)) %>% filter(!is.na(EC030Q01NA))
pomagaPOL[1] <- ifelse(rep(TRUE, times = length(tmp$EC030Q01NA)), tmp$EC030Q01NA-1, NA) %>% mean # w 40% domow w POL kobieta pomaga
tmp <- tbl_ciekawe_GBR_ext %>% select(EC030Q02NA, CNTSTUID.x) %>% filter(!duplicated(CNTSTUID.x)) %>% filter(!is.na(EC030Q02NA))
pomagaGBR[2] <- ifelse(rep(TRUE, times = length(tmp$EC030Q02NA)), tmp$EC030Q02NA-1, NA) %>% mean # w 50% domow w GBR mezczyzna pomaga
tmp <- tbl_ciekawe_POL_ext %>% select(EC030Q02NA, CNTSTUID) %>% filter(!duplicated(CNTSTUID)) %>% filter(!is.na(EC030Q02NA))
pomagaPOL[2] <- ifelse(rep(TRUE, times = length(tmp$EC030Q02NA)), tmp$EC030Q02NA-1, NA) %>% mean # w 57% domow w POL mezczyzna pomaga
names(pomagaGBR) <- c("kobieta", "mezczyzna")
ktopomaga <- rbind(pomagaGBR, pomagaPOL)








