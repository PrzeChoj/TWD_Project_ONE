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




































