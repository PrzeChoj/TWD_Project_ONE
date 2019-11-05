# tworzenie tabelek do pozniejszej analizy


# procentowa zdawalnosc z podzialem na panstwa i etapy szkolne
which(colnames(tmp_duzy)=="ST127Q01TA") # 77
powtarzanie_klasy <- tmp_duzy[c(2, 77, 78, 79)]
powtarzanie_klasy_podstawowka <- powtarzanie_klasy[c(1, 2)]
powtarzanie_klasy_gimnazjum <- powtarzanie_klasy[c(1, 3)]
powtarzanie_klasy_liceum <- powtarzanie_klasy[c(1, 4)]

powtarzanie_klasy_podstawowka <- procent_panstw(powtarzanie_klasy_podstawowka)
colnames(powtarzanie_klasy_podstawowka) <- c("ONCE", "MORE", "NEVER")
powtarzanie_klasy_podstawowka <- powtarzanie_klasy_podstawowka %>% tbl_df() %>%
  mutate(country=rownames(powtarzanie_klasy_podstawowka)) %>% select(country, NEVER, ONCE, MORE)

powtarzanie_klasy_gimnazjum <- procent_panstw(powtarzanie_klasy_gimnazjum)
colnames(powtarzanie_klasy_gimnazjum) <- c("NEVER", "MORE", "ONCE")
powtarzanie_klasy_gimnazjum <- powtarzanie_klasy_gimnazjum %>% tbl_df() %>%
  mutate(country=rownames(powtarzanie_klasy_gimnazjum)) %>% select(country, NEVER, ONCE, MORE)

powtarzanie_klasy_liceum <- procent_panstw(powtarzanie_klasy_liceum)
colnames(powtarzanie_klasy_liceum) <- c("ONCE", "MORE", "NEVER")
powtarzanie_klasy_liceum <- powtarzanie_klasy_liceum %>% tbl_df() %>%
  mutate(country=rownames(powtarzanie_klasy_liceum)) %>% select(country, NEVER, ONCE, MORE)


powtarzanie_klasy_merged <- powtarzanie_klasy
colnames(powtarzanie_klasy_merged) <- c("Country", "P", "G", "L")

suma_oblan <- function(P, G, L){
  # P G L to wektory
  # posiadajace 1 jesli osoba nigdy nie oblala
  # 2 jesli raz i 3 jesli wiecej razy
  # funkcja laczy je w jeden
  stopifnot(length(P) == length(G))
  stopifnot(length(P) == length(L))
  
  # radzenie sobie z NA
  P <- ifelse(is.na(P), 1, P)
  G <- ifelse(is.na(G), 1, G)
  L <- ifelse(is.na(L), 1, L)
  
  wynik <- integer(length = length(P))
  
  jeden <- function(p, g, l){
    # funkcja przyjmuje 3 argumenty i zwraca prawde gdy
    # dokladnie jeden z nich ma wartosc 2,
    # a pozostale 1
    return (p==2 & g==1 & l==1) | (p==1 & g==2 & l==1) | (p==1 & g==1 & l==2)
  }
  
  ifelse(P==1 & G==1 & L==1, 1, 
         ifelse(jeden(P, G, L), 2, 3))
}

# NOTE: niektore panstwa nie maja liceum i w nich jest caly wektor NA
# zamieniam je wiec na 1, bo takiej informacjii potrzebuje
# forie to w funkcji suma_oblan
# Poza tym recznie wiec pozbywam sie panstw, ktore nie maja zadnych danych,
# to jest NOR, JPN i ALB.
powtarzanie_klasy_merged <- powtarzanie_klasy_merged %>% tbl_df %>% 
  filter(Country != "NOR" & Country != "JPN" & Country != "ALB") %>%
  mutate(sum=suma_oblan(P,G,L))
powtarzanie_klasy_merged <- powtarzanie_klasy_merged[c(1, 5)]
powtarzanie_klasy_merged <- procent_panstw(powtarzanie_klasy_merged)
powtarzanie_klasy_merged <- powtarzanie_klasy_merged %>% tbl_df() %>% select(NEVER = 3, ONCE = 1, MORE = 2)







# posiadanie biurka
tbl <- procent_panstw(tmp_duzy[c(2,which(colnames(tmp_duzy)=="ST011Q01TA"))])
panstwa <- rownames(tbl)
colnames(tbl) <- c("YES", "NO")
tbl <- tbl %>% tbl_df() %>% mutate(country=panstwa)

tmp <- tbl %>% arrange(YES)
index <- which(tmp$country=="POL")
tmp[index,]
tmp %>% tail(20) # Polska jest w pierwszej 20 z 71 na temat posiadania biurka w domu przez uczniow

plot(tmp[[1]])
points(56, tmp[56, 1], pch=16, col="RED") # Polska



# tworzenie tabelki z wielka brytania(potrzebne wykonanie skryptu BGR_schools.R)
tmp_GBR <- tmp_duzy %>% filter(CNT == "GBR")
GBR <- inner_join(tmp_GBR, GBR_results, by="CNTSCHID")




# pogoda
# nie bylo oryginalnie zrobione i dodane do GBR.csv!
GBR_results$region %>% unique()
GBR_results$subregion %>% unique()

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
map[,2] %>% unique # takich szukam
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








# jakie jeszcze tam sa ciekawe rzeczy nieopisane w formularzach?
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
School$SC013Q01TA # 1-publiczna, 2-prywatna


# polaczny tbl_ciekawe z tbl_szkolny
tbl_szkolny$CNTSCHID <- tbl_szkolny$CNTSCHID %>% as.integer() # to musi byc integer
tbl_ciekawe <- tbl_ciekawe %>% inner_join(tbl_szkolny, by="CNTSCHID")



# polaczny tbl_ciekawe z tbl_szkolny
tbl_szkolny$CNTSCHID <- tbl_szkolny$CNTSCHID %>% as.integer() # to musi byc integer
tbl_ciekawe_POL <- tbl_ciekawe_POL %>% inner_join(tbl_szkolny, by="CNTSCHID")







# jeszcze wieksza ramka 05.11.19
tmp <- tmp_duzy %>% select(CNTSTUID, ST103Q01NA, ST127Q01TA, ST004D01T, ST123Q01NA)

tbl_ciekawe$CNTSTUID.x <- tbl_ciekawe$CNTSTUID.x %>% as.double()
tbl_ciekawe_GBR_ext <- tbl_ciekawe %>% inner_join(tmp, by=c("CNTSTUID.x" = "CNTSTUID"))
tbl_ciekawe_POL_ext <- tbl_ciekawe_POL %>% inner_join(tmp, by=c("CNTSTUID" = "CNTSTUID"))

# tbl_ciekawe_GBR_ext$type to info o rodzaju utrzymywania szkoly w GBR: akademicka, nizalezna, utrzymywana:
tbl_ciekawe_GBR_ext$type <- ifelse(tbl_ciekawe_GBR_ext$type == "academy", "akademicka", ifelse(tbl_ciekawe_GBR_ext$type == "independent", "niezalezna", "utrzymywana"))




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











