library("dplyr")
library("ggplot2")

school %>% head(2)

all_equal(school, tmp)
school %>% dim
rownames(school)
colnames(tmp)
colnames(school)

tmp$DM474Q01R[which(!is.na(tmp$DM474Q01R))]
tmp %>% str

attr(tmp$CNTRYID, "label")



colnames(tmp_duzy)
which(tmp_duzy %>% colnames() == "ST103Q01NA") # 164
teacher_explains_scientific_ideas <- tmp_duzy[[164]]
teacher_explains_scientific_ideas <- teacher_explains_scientific_ideas[which(!(teacher_explains_scientific_ideas %>% is.na()))]
teacher_explains_scientific_ideas <- table(teacher_explains_scientific_ideas)/length(teacher_explains_scientific_ideas)
names(teacher_explains_scientific_ideas) <- c("nigdy", "czasem", "czesto", "zawsze")
teacher_explains_scientific_ideas # procentowo jak nauczyciel tlumaczy

teacher_explains_scientific_ideas_cantry <- tmp_duzy[c(2,164)]
teacher_explains_scientific_ideas_cantry <- teacher_explains_scientific_ideas_cantry[which(!is.na(teacher_explains_scientific_ideas_cantry[[2]])),]
tmp1 <- teacher_explains_scientific_ideas_cantry[teacher_explains_scientific_ideas_cantry[[2]]==1,]
tmp2 <- teacher_explains_scientific_ideas_cantry[teacher_explains_scientific_ideas_cantry[[2]]==2,]
tmp3 <- teacher_explains_scientific_ideas_cantry[teacher_explains_scientific_ideas_cantry[[2]]==3,]
tmp4 <- teacher_explains_scientific_ideas_cantry[teacher_explains_scientific_ideas_cantry[[2]]==4,]
tmp1 <- table(tmp1[[1]])
tmp2 <- table(tmp2[[1]])
tmp3 <- table(tmp3[[1]])
tmp4 <- table(tmp4[[1]])
#tmp <- matrix(tmp1, tmp2, tmp3, tmp4)
tmp <- data_frame(tmp1, tmp2, tmp3, tmp4)
tmp <- tmp %>% mutate(sum=tmp1+tmp2+tmp3+tmp4)
unclass(tmp)

dzielenie <- function(v){
  v/tmp[[5]]
}
tmp <- sapply(tmp, dzielenie)
tmp <- tmp[,1:4]
colnames(tmp) <- c("nigdy", "czasem", "czesto", "zawsze")
rownames(tmp) <- rownames(tmp1)
tmp


dt <- tmp_duzy[c(2,164)]
dt <- powtarzanie_klasy[c(1, 4)]
procent_panstw <- function(dt){
  # przyjmuje tabele danych, gdzie pierwsza kolumna to wektor panstw, a druga to wartosci
  # wartosci NA sa usuwane
  panstwa <- dt[[1]]
  dane <- dt[[2]]
  brak_danych <- is.na(dane)
  panstwa <- panstwa[!brak_danych]
  dane <- dane[!brak_danych]
  
  n <- unique(dane)
  wynik <- matrix(nrow = length(unique(panstwa)), ncol = length(n)+1)
  wynik[,length(n)+1] <- integer(length = length(wynik[,1]))          # ustawianie na 0, zeby potem dodawac
  colnames(wynik) <- character(length(n)+1)
  rownames(wynik) <- sort(unique(panstwa)) # ustanienie nazw panstw
  colnames(wynik)[(length(n)+1)] <- "suma"
  for (i in 1:length(n)){
    panstwa_i <- panstwa[which(dane==n[i])]
    
    tabela <- table(panstwa_i)
    # trzeba zrobic join wynik(rownames) i tabela(names)
    kolejnosc <- match(rownames(tabela), rownames(wynik))
    
    wynik[kolejnosc,i] <- table(panstwa_i)
    
    # ale zamiast NA chce miec 0:
    wynik[,i] <- ifelse(is.na(wynik[,i]), 0, wynik[,i])
    
    colnames(wynik)[i] <- n[i]
    
    wynik[,length(n)+1] <- wynik[,length(n)+1] + wynik[,i]
  }
  
  # dzielenie przez sume
  for (i in 1:(length(n)+1)){
    wynik[,i] <- wynik[,i]/wynik[,(length(n)+1)]
  }
  
  wynik[,1:length(n)]
}


tmp
procent_panstw(tmp_duzy[c(2,164)])



# posiadanie biurka
tmp_duzy[c(2,which(colnames(tmp_duzy)=="ST011Q01TA"))]
tbl <- procent_panstw(tmp_duzy[c(2,which(colnames(tmp_duzy)=="ST011Q01TA"))])
panstwa <- rownames(tbl)
colnames(tbl) <- c("YES", "NO")
tbl <- tbl %>% tbl_df() %>% mutate(country=panstwa)

tmp <- tbl %>% arrange(YES)
index <- which(tmp$country=="POL")
tmp[index,]
tmp %>% tail(20) # Polska jest w pierwszej 20 z 71 na temat posiadania biurka w domu przez uczniow

plot(tmp[[1]])





# Czy powtarzales klase
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










which(startsWith(colnames(tmp_duzy),"ST004")) # 29 to kolumna z plcia
gender <- tmp_duzy[[29]]






index_samochody <- which(colnames(tmp_duzy) == "ST012Q02TA") # 57
samochody <- tmp_duzy[c(2, index_samochody)]
samochody <- procent_panstw(samochody)
samochody <- samochody %>% tbl_df() %>%
              mutate("Country" = rownames(samochody)) %>%
              select("Country" = "Country", "None"="1", "One" = "2", "Two" = "3", "More" = "4")
samochody %>% arrange(None)





tmp <- tmp_schools
tmp$female <- tmp$female %>% substr(1, 3)



# do usuniecia
# pozbycie sie pozostalosci po kodzie html
dane$Time_name <- ifelse(nchar(strona[indeksy_czasow])<15,
                         strona[indeksy_czasow] %>% substr(5, 11),
                         strona[indeksy_czasow] %>% substr(8, 14))


# Time
dane$Time <- integer(14)
# godziny
hours <- dane$Time_name %>% substr(1, 1) %>% strtoi
dane$Time <- dane$Time + hours * 60 * 60
# minuty
minutes <- dane$Time_name %>% substr(3, 4) %>% strtoi
minutes <- ifelse(is.na(minutes), 8, minutes) # zamiana NA na 8, bo odbywa sie pomylka # TODO
dane$Time <- dane$Time + minutes * 60
# sekundy
seconds <- dane$Time_name %>% substr(6, 7) %>% strtoi
dane$Time <- dane$Time + seconds



# Place
dane$Place <- strona[indeksy_czasow+2] # informacje o miescie sa o 2 dalej niz o czasie
dane$Place <- dane$Place %>% substr(5, nchar(dane$Place))
dane$Place <- dane$Place %>%
  startsWith("<i>") %>%
  ifelse(substr(dane$Place, 4, nchar(dane$Place)-4), dane$Place) # pozbycie sie <i>

# naprawianie pozostalych
dodatkowe_miasta <- dane$Place %>% stri_extract_all_regex("wiki[^\"]*") %>% unlist # wybor nazwy misat regex'em
dodatkowe_miasta <- dodatkowe_miasta %>% substr(6, nchar(dodatkowe_miasta))        # pozbycie sie pozostalosci
dane$Place <- dodatkowe_miasta %>% is.na %>% ifelse(dane$Place, dodatkowe_miasta)  # wpisanie do tabeli danych


# dodanie informacjii o rekordzie swiata
dane$WR <- character(14)
dane$WR[12] <- "(World Record)"




# szukanie danych o rodzicach, niesprawdzone pliki
szukam_rodzicow <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_COG", "cy6_ms_cmb_stu_cog.sas7bdat") # niesprawdzony
szukam_rodzicow <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QTM", "cy6_ms_cmb_stu_qtm.sas7bdat") # niesprawdzony
szukam_rodzicow <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_TCH_QQQ", "cy6_ms_cmb_tch_qqq.sas7bdat") # niesprawdzony











rodzice %>% colnames %in% c("PA004Q01NA", "PA004Q01", "PA003Q01TA") %>% any
which(rodzice %>% colnames %in% c("PA004Q01NA", "PA004Q01", "PA003Q01TA"))
which(!szukam_rodzicow[[516]] %>% is.na) # są dane :D
(rodzice %>% colnames)[c(508, 516)]





# funkcja do sprawdzania jak roznia sie rodzice z dziecmi w szkolach z podzialem na plcie od innych
  # potrzebna ramka danych GBR
# przyklad: czy rodzice rozmawiaja z dzieckiem o szkole?
PA003Q01TA
ciekawe <- GBR %>% select(PA003Q01TA, sex) %>% filter(!is.na(PA003Q01TA) & !is.na(sex)) # czemu niema takich?

# sprawdzmy to:
tmp <- rodzice_GBR %>% filter(!is.na(PA003Q01TA))
tmp$CNTSCHID %in% GBR_results$CNTSCHID

# ogolniej
rodzice_GBR$CNTSCHID %in% GBR_results$CNTSCHID
GBR[["sex"]] %>% is.na %>% mean # jest 1% NA

GBR %>% select(sex, PA006Q08TA) %>% filter(!is.na(PA006Q08TA)) %>% filter(!is.na(sex))

# sprawdzanie, czy sa jakiekolwiek informacje
tmp <- GBR %>% filter(!is.na(sex))
colnames(GBR)
CNTSCHID
GBR$CNTRYID


# na pewno sie nieda, porzucam













# szukacie ciekawych rzeczy
GBR_results
tmp_GBR <- tmp_duzy %>% filter(CNT == "GBR")



tmp_GBR$CNTSCHID %in% GBR_results$CNTSCHID


GBR <- inner_join(tmp_GBR, GBR_results, by="CNTSCHID")
# dodanie map, czyli danych o pogodzie
GBR$fullregion <- paste(GBR$region, GBR$subregion, sep=" ")
GBR <- GBR %>% inner_join(map, by="fullregion")

biurko <- GBR %>% select(sex, ST011Q01TA) %>% filter(!is.na(ST011Q01TA))
biurko <- procent_panstw(biurko)
colnames(biurko) <- c("mam biurko", "niemam")
biurko # ciut wiecej, 5 punktow procentowych, biurek maja w niemieszanych szkolach


instrumenty <- GBR %>% select(sex, ST012Q09NA) %>% filter(!is.na(ST012Q09NA))
instrumenty <- procent_panstw(instrumenty)
colnames(instrumenty) <- c("Jeden", "Niemam", "Dwa", "Wiecej")
instrumenty # tez 5 punktow procentowych




procent_sex("ST123Q01NA") # 7 punktow procentowych wiecej, malo, nieciekawe
procent_sex("ST123Q02NA")
procent_sex("ST123Q03NA") # chlopaki tak samo
procent_sex("ST123Q04NA") # 5 punktow procentowych wiecej, malo, nieciekawe



GBR$ST019Q01TA
GBR$ST019AQ01T %>% unique
procent_sex("ST019AQ01T") # ciut wiecej, 5 punktow procentowych, uczniowie z GBR sa w szkolach mieszanych

# important
# suma po nie byciu z GBR
dane <- GBR$ST019BQ01T + GBR$ST019CQ01T
dane <- cbind(GBR$sex, dane)
colnames(dane) <- c("1", "dane")
dane <- dane %>% tbl_df()
rodzice <- procent_panstw(dane)
colnames(rodzice) <- c("jeden_rodzic", "zaden", "obaj") # sa nie-brytyjczykami
rodzice # to jest ciekawe
# koniec-important


# important
dane <- GBR$ST019BQ01T + GBR$ST019CQ01T
dane <- cbind(GBR$sex, dane)
colnames(dane) <- c("1", "dane")
dane <- dane %>% tbl_df()
dane2 <- dane[which(dane$dane=="2"),]
dane3 <- dane[which(dane$dane=="3"),]
dane4 <- dane[which(dane$dane=="4"),]

wynik <- matrix(nrow=3, ncol=3)
rownames(wynik) <- c("obaj", "jeden rodzic", "zaden")
colnames(wynik) <- c("mixed", "male", "female")

wynik[1,1] <- mean(dane2$`1`=="mixed")
wynik[1,2] <- mean(dane2$`1`=="male")
wynik[1,3] <- mean(dane2$`1`=="female")

wynik[2,1] <- mean(dane3$`1`=="mixed")
wynik[2,2] <- mean(dane3$`1`=="male")
wynik[2,3] <- mean(dane3$`1`=="female")

wynik[3,1] <- mean(dane4$`1`=="mixed")
wynik[3,2] <- mean(dane4$`1`=="male")
wynik[3,3] <- mean(dane4$`1`=="female")

wynik # do jakich szkol wysylaja swoich dzieci w zaleznosci od tego, ile z rodzicow jest Brytyjczykami
# koniec-important
#



tmp <- do_plota(procent_sex("ST097Q02TA"))
ggplot(data = tmp, aes(x=wiersze, y=dane*100, fill=kolumny)) +
  geom_bar(stat="identity")
# TODO jeszcze nastepne 3


tmp <- do_plota(procent_sex("ST131Q11NA"))
ggplot(data = tmp, aes(x=wiersze, y=dane*100, fill=kolumny)) +
  geom_bar(stat="identity")
#



# zabawa z pogoda
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
# suma opadow w regionie miedzy 2008, a 2014 rokiem

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


#




# eksploracja ze wzgledu na ilosc opadow
GBR$rainregion

GBR_deszcz <- GBR %>% select(deszcz, rainregion, PV10MATH, PV10READ, PV10SCIE)
GBR_deszcz$deszcz <- GBR_deszcz$deszcz %>% as.numeric()

ggplot(data = GBR_deszcz, aes(x = deszcz, y = PV10MATH, shape=rainregion)) +
  geom_point(alpha = 0.01, position = position_jitter(width = 50), aes(colour = rainregion)) +
  geom_boxplot(aes(colour = "black"), outlier.color = NA) +
  ggtitle("Ilosc deszczu a wyniki w nauce") +
  xlab("Suma opadow w latach 2008-2014") + ylab("Wynik z matematyki") +
  scale_fill_discrete(name = "Region") +
  theme(legend.position = "none")

ggplot(data = GBR_deszcz, aes(x = deszcz, y = PV10READ, shape=rainregion)) +
  geom_point(alpha = 0.01, position = position_jitter(width = 50), aes(colour = rainregion)) +
  geom_boxplot(aes(colour = "black"), outlier.color = NA) +
  ggtitle("Ilosc deszczu a wyniki w nauce") +
  xlab("Suma opadow w latach 2008-2014") + ylab("Wynik z czytania") +
  scale_fill_discrete(name = "Region") +
  theme(legend.position = "none")

ggplot(data = GBR_deszcz, aes(x = deszcz, y = PV10SCIE, shape=rainregion)) +
  geom_point(alpha = 0.01, position = position_jitter(width = 50), aes(colour = rainregion)) +
  geom_boxplot(aes(colour = "black"), outlier.color = NA) +
  ggtitle("Ilosc deszczu a wyniki w nauce") +
  xlab("Suma opadow w latach 2008-2014") + ylab("Wynik z nauk przyrodniczych") +
  scale_fill_discrete(name = "Region") +
  theme(legend.position = "none")








# wykres wynikow ze wzgledu na narodowosc rodzicow
procent_panstw



dane <- GBR$ST019BQ01T + GBR$ST019CQ01T
dane <- cbind((GBR$PV10MATH+GBR$PV10READ+GBR$PV10SCIE)/3, dane)
colnames(dane) <- c("wynikTEST", "Rodzice")
dane <- dane %>% tbl_df() %>% filter(!is.na(Rodzice))
dane$Rodzice <- ifelse(dane$Rodzice==2, "Brytyjczycy", ifelse(dane$Rodzice==3, "Jeden", "obaj zagramaniczni"))

ggplot(data = dane, aes(x=Rodzice, y=wynikTEST)) +
  geom_point(alpha = 0.01, position = position_jitter(width = 0.4), aes(colour = Rodzice)) +
  geom_boxplot(aes(colour = "black"), outlier.color = NA) +
  ggtitle("Wyniki testow w zaleznosci od narodowosci rodzicow") +
  xlab("Narodowosc rodzicow") + ylab("Sredni wynik z testu") +
  theme(legend.position = "none")






# 3. Czy chlopcy/dziewczynki ucza sie wiecej w prywatnyc, czy publicznych szkolach?
GBR$ST004D01T.x # 1-dziewczynki, 2-chlopcy




(GBR %>% filter(!(CNTSTUID.x %>% duplicated)))$ST003D02T %>% table # ile uczniow z ktorego miesiaca
(GBR %>% filter(!(CNTSTUID.x %>% duplicated)))$ESCS %>% na.omit() %>% mean # srednia zamoznosc
(GBR %>% filter(!(CNTSTUID.x %>% duplicated)))$ESCS %>% na.omit() %>% var # wariancja zamoznosci
(GBR %>% filter(!(CNTSTUID.x %>% duplicated)))$ST022Q01TA %>% table


# jakie jeszcze tam sa ciekawe rzeczy nieopisane w formularzach?
nr_wspolnych <- which(tmp_duzy %>% colnames %in% (GBR %>% colnames))

label_wspolnych <- character(length = length(nr_wspolnych))
names_wspolnych <- (tmp_duzy %>% colnames)[nr_wspolnych]

for( i in 1:length(nr_wspolnych) ){
  label_wspolnych[i] <- attr(tmp_duzy[[nr_wspolnych[i]]],"label")
}
label_wspolnych <- cbind(nr_wspolnych, label_wspolnych, names_wspolnych) %>% tbl_df()






# urzywanie procent_prywatnych na tbl_ciekawe
col <- tbl_ciekawe %>% colnames
tbl_ciekawe[[29]]

# nie rozumiem roznicy miedzy tymi dwoma
srednia_prywatnych("TMINS") # srednia spedzoneczo czasu na nauce poza szkola w minutach
srednia_prywatnych("OUTHOURS") # srednia spedzoneczo czasu na nauce poza szkola w sumie

srednia_prywatnych("ST118Q01NA") # trudno bedzie mi pisac test
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[9]) # jestem ambitny
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[10]) # chce byc najlepszy
# ciekawe
srednia_prywatnych_porownanie(col[11]) # wole pracowac w grupie
srednia_prywatnych_porownanie(col[12]) # umien sluchac
srednia_prywatnych_porownanie(col[13]) # lubie widziec sukces moich rowiesnikow
srednia_prywatnych_porownanie(col[14]) # Biore pod uwage, czym interesuja sie inni
srednia_prywatnych_porownanie(col[15]) # Grupy podejmuja lepsze decyzje niz jednostki
srednia_prywatnych_porownanie(col[16]) # Lubie rozmyslac nad roznymi mozliwosciami
srednia_prywatnych_porownanie(col[17]) # W grupie lepiej mi sie pracuje
srednia_prywatnych_porownanie(col[18]) # Lubie pracowac w parach
srednia_prywatnych_porownanie(col[19]) # W szkole czuje sie jak samotnik

# ciekawe
srednia_prywatnych_porownanie(col[20]) # Latwo zaznajamiam sie
# ciekawe
srednia_prywatnych_porownanie(col[21]) # Czuje sie jak czesc szkoly
srednia_prywatnych_porownanie(col[22]) # Dziwnie cie czuje w szkole
# ciekawe
srednia_prywatnych_porownanie(col[23]) # Inni zdaja sie mnie lubic
# ciekawe
srednia_prywatnych_porownanie(col[24]) # Czuje sie samotny
srednia_prywatnych_porownanie(col[25]) # W ostatnich 2 tygodniach zdazalo mi sie nie przyjsc do szkoly(trzeba odjac jeden)
srednia_prywatnych_porownanie(col[26]) # W ostatnich 2 tygodniach zdazalo mi sie pomijac lekcje(trzeba odjac jeden)
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[27]) # W ostatnich 2 tygodniach zdazalo mi sie spozniac do szkoly(trzeba odjac jeden)
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[28]) # W roku szkolnym spedzilem okolo () godzin na nauce
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[29]) # Jak duzo w tygodniu sie uczylem matematyki
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[30]) # Jak duzo w tygodniu sie uczylem jezyka ojczystego -> patrz 36
# BARDZO ciekawe
srednia_prywatnych_porownanie(col[31]) # Jak duzo w tygodniu sie uczylem jezyka obcego -> patrz 37

srednia_prywatnych_porownanie(col[33]) # Zle mi bez internetu
srednia_prywatnych_porownanie(col[34]) # Ile tygodniowo zajec z fizyki
srednia_prywatnych_porownanie(col[35]) # Ile tygodniowo zajec z matematyki
srednia_prywatnych_porownanie(col[36]) # Ile tygodniowo zajec z jezyka ojczystego
srednia_prywatnych_porownanie(col[37]) # Ile tygodniowo zajec z jezyka obcego
srednia_prywatnych_porownanie(col[38]) # Ile tygodniowo zajec z nauk spolecznych
srednia_prywatnych_porownanie(col[39]) # Ile tygodniowo zajec z muzyki
srednia_prywatnych_porownanie(col[40]) # Ile tygodniowo zajec z WF-u
srednia_prywatnych_porownanie(col[41]) # Ile tygodniowo zajec z rysunku
srednia_prywatnych_porownanie(col[42]) # Ile tygodniowo zajec ze sztuki
srednia_prywatnych_porownanie(col[44]) # Chodze na korki, bo chce poznawac wiedze # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[45]) # Chodze na korki, bo chce zdac egzamin # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[47]) # Chodze na korki, bo rodzice chcieli # 1-prawda, 0-falsz
# ARCY ciekawe
srednia_prywatnych_porownanie(col[48]) # Chodze na korki, bo koledzy tesz chodza # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[49]) # Chodze na korki, bo nauczyciel powiedzial, ze warto # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[50]) # Chodze na korki, bo CHCE miec lapsze oceny # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[51]) # Chodze na korki, bo MUSZE miec lapsze oceny # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[52]) # Chodze na korki, bo uwazam, ze warto sie uczyc # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[53]) # Chodze na korki, bo dobrze to wyglada w CV # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[54]) # Chodze na korki, bo musze dla przyszlej pracy # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[56]) # NIE Chodze na korki, bo nie potrzebuje # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[57]) # NIE Chodze na korki, bo nie pasowaly mi # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[58]) # NIE Chodze na korki, malo moich kolegow chodzi # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[59]) # NIE Chodze na korki, bo niemam na to czasu # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[60]) # NIE Chodze na korki, bo niestac mnie # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[61]) # NIE Chodze na korki, bo moi nauczyciele wysarczajaco mnie ucza # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[62]) # NIE Chodze na korki, bo moi rodzice nie chca # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[63]) # NIE Chodze na korki, bo niejest to warte pieniedzy # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[64]) # NIE Chodze na korki, bo moi nauczyciele mowia, ze nie warto # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[65]) # NIE Chodze na korki, bo niegdy nie myslalem na ten temat # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[66]) # NIE Chodze na korki, bo niejest to mozliwe w mojej okolicy # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[67]) # NIE Chodze na korki, bo rodzina mi pomaga # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[68]) # NIE Chodze na korki, bo koledzy mi pomagaja # 1-prawda, 0-falsz
srednia_prywatnych_porownanie(col[69]) # Kobieta pomaga mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[70]) # Mezczyzna pomaga mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[71]) # Rodzenstwo pomaga mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[72]) # Dziadkowie pomagaja mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[73]) # Dalsi krewni pomagaja mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[74]) # Nikt nie pomaga mi w nauce # 1-prawda, 2-nie prawda
srednia_prywatnych_porownanie(col[76]) # Wiek




# ciekawe
srednia_sponsorowanych_GBR(col_ext[86]) # jak czesto nauczycie tlumacza na lekcjach: 4-zawsze, 1-prawie nigdy
srednia_prywatnych_porownanie(col_ext[86]) # polsce sporo
srednia_sponsorowanych_GBR(col_ext[87]) # czy powtarzales klase?
srednia_prywatnych_porownanie(col_ext[87]) # nieciekawe
srednia_sponsorowanych_GBR(col_ext[89]) # moi rodzice interesuja sie jak mi idzie w szkole
srednia_prywatnych_porownanie(col_ext[89]) # nieciekawe
srednia_sponsorowanych_GBR(col_ext[90]) # zarobki
srednia_prywatnych_porownanie(col_ext[90]) # Super ciekawe
srednia_sponsorowanych_GBR(col_ext[91]) # zarobki2
srednia_prywatnych_porownanie(col_ext[91]) # Super ciekawe


srednia_sponsorowanych_GBR(col_ext[76])







