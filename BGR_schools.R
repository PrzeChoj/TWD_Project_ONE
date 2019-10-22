
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
file <- file.path("Dane" ,"PUF_SAS_COMBINED_CMB_SCH_QQQ", "CY6_MS_CMB_SCH_QQQ.sas7bdat.format.sas")
#poczatek wczytywania danych - od tego nr wiersza zaczyna sie kolumna STRATUM
first_line <- grep("value \\$STRATUM", readLines(file))
#szukamy konca wczytywania - w pliku z oznaczeniami koniec kolumny oznaczany jest przez ";"
tmp <- grep(";", readLines(file))
end_line <- tmp[tmp >801] #to jest nr wiersza posiadajacy ";" po kolumnie statum
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





# obrazki
Results_in_mixed_school <- GBR_results[
                                       !is.na(CNTSCHID) & sex=="mixed",
                                       .(Mean_math = mean(PV1MATH), Mean_read = mean(PV1READ), Mean_science = mean(PV1SCIE)),
                                       ST004D01T
                                       ]
Results_in_mixed_school$ST004D01T <- ifelse(Results_in_mixed_school$ST004D01T == 1, "female", "male")

tmp_schools <- Results_in_mixed_school %>% t()
cbind(rownames(tmp_schools), tmp_schools) -> tmp_schools
rownames(tmp_schools) <- NULL
colnames(tmp_schools) <- c("results", "female", "male")
tmp_schools <- tmp_schools[-1, ] %>% as.data.table()
tmp_schools$female <- tmp_schools$female %>% as.integer()
tmp_schools$male <- tmp_schools$male %>% as.integer()

#wykres do powyzszych wynikow 
#zrobic legende TODO
ggplot(Results_in_mixed_school) +
  geom_point(aes(x = ST004D01T, y=Mean_math), stat = "identity", color="pink", size=8) +
  geom_point(aes(x = ST004D01T, y=Mean_read), stat = "identity", color="navyblue", size=8) +
  geom_point(aes(x = ST004D01T, y=Mean_science), stat = "identity", color="orange", size=8) +
  labs(title = "Średnie wyniki testów wsród szkół mieszanych z podziałem na płeć uczniów.",
       y = NULL,
       x = NULL)

#nie dala, poprawic kolory i zmienic os Y TODO
przesuniecie <- function(x){
  # przesuwa wektor, aby muc latwiej zauwarzyc ruznice w danych
  x-450
}
ggplot(tmp_schools) +
  geom_bar(aes(y = przesuniecie(female), x = results, fill=c("Female", "Female", "Female")), stat= "identity", alpha = 0.6, width = 0.5) +
  geom_bar(aes(y = przesuniecie(male), x = results, fill=c("Male", "Male", "Male")), stat = "identity", alpha = 0.4, width = 0.5) +
  labs(subtitle = "Średnie wyniki wsród szkół mieszanych. Podział na płeć.",
       y = NULL,
       x = NULL) +
  theme(legend.title=element_blank()) +
  scale_y_continuous(breaks=c(0, 25, 30, 37, 45, 49), labels=c("450", "475", "480", "487", "495", "499")) +
  scale_x_discrete(labels=c("MATH", "READ", "SCIENCE"))
  
#specjalnie dla Adasia sprawdzenie czy sa jakies kobiety w meskiej szkole i odwrotnie
GBR_results[!is.na(CNTSCHID), ][, .N, .(sex, ST004D01T)][order(sex)] #4247 kobiety i 4519 mezczyzn w szkolach mieszanych
                                                                     #34 kobiety i 1125 mezczyzn w szkolach meskich
                                                                     #1109 kobiety i 12 mezczyzn w szkolach zenskich

#chcemy zbaczyc srednie wyniki w zaleznosci od szkol 
Results_region_and_sex <- GBR_results[
                                      !is.na(CNTSCHID),
                                      .(Mean_math = mean(PV1MATH), Mean_read = mean(PV1READ), Mean_science = mean(PV1SCIE)),
                                      .(region, sex)
                                      ][
                                        order(region)
                                        ]

#srednie wyniki w zaleznosci od rodzaju szkoly - plec
Results_sex <- GBR_results[
                           !is.na(CNTSCHID), .(Mean_math = mean(PV1MATH), Mean_read = mean(PV1READ), Mean_science = mean(PV1SCIE)), sex
                           ]

# dostosowanie tej gownianej tableki do dobrej formy
tmp <- matrix(nrow=9, ncol=3)
colnames(tmp) <- c("sex", "Type", "Score")
tmp <- tbl_df(tmp)
tmp$sex <- rep(Results_sex$sex, length=3)
tmp$Type <- rep(c("SCIENCE", "READ", "MATH"), each=3)
tmp$Score <- c(Results_sex$Mean_science, Results_sex$Mean_read, Results_sex$Mean_math)

#wykres wynikow
ggplot(tmp, aes(x=sex, color=Type, y=Score)) +
  geom_point(size=8) +
  labs(title = "Średnie wyniki testów wsród roznych typow szkół.",
       y = NULL,
       x = NULL)


tmp <- tbl_df(Results_region_and_sex)
tmp <- tmp %>% mutate(MEAN = (Mean_math + Mean_read + Mean_science)/3) %>% select(region, sex, MEAN)
ggplot(tmp, aes(colour = sex)) +
  geom_point(aes(x = region, y=MEAN), stat = "identity", size=8, position = position_jitter(width = 0.2)) +
  labs(title = "Średnie wyniki testów wsród szkół w zależności od regionu.",
       y = NULL,
       x = NULL) + 
  scale_colour_manual(values = c("#CC2A00", "#044BA5", "#E69F00"))

### DALEJ NIC NIE ZOSTAŁO JESZCZE ZROBIONE

tmp_math <- GBR_results[order(PV1MATH, decreasing = TRUE)][1:10,]
tmp_read <- GBR_results[order(PV1READ, decreasing = TRUE)][1:10,]
tmp_science <- GBR_results[order(PV1SCIE, decreasing = TRUE)][1:10,]

setkey(tmp_math, CNTSTUID)
setkey(tmp_read, CNTSTUID)
setkey(tmp_science, CNTSTUID)
tmp_math[tmp_read, nomatch=0][tmp_science, nomatch = 0][, 1:11]
Student[CNTSTUID==82606747, ][, .(ST123Q01NA, ST123Q02NA, ST123Q03NA, ST123Q04NA)] #ciekawe - najlepsza dziewczyna ma wsparcie rodzicow
                                                                                   #czy wsparcie rodzicow wplywa na oceny?

setkey(Student, CNTSTUID)
setkey(GBR_results, CNTSTUID)
parents_influence <- GBR_results[Student[, .(CNTSTUID, ST123Q01NA, ST123Q02NA, ST123Q03NA, ST123Q04NA)], nomatch=0]

parents_influence %>% mutate(MEAN_RATE = (PV1MATH + PV1READ + PV1SCIE)/3) -> parents_influence
parents_influence %>% mutate(MEAN_INFLU = (ST123Q01NA + ST123Q02NA + ST123Q03NA + ST123Q04NA)/4) -> parents_influence
parents_influence %>% as.data.table() -> parents_influence

ggplot(parents_influence, aes(x = MEAN_INFLU, y = MEAN_RATE)) +
  geom_point(aes(x = MEAN_INFLU, y = MEAN_RATE), position = position_jitter(width = 0.3), colour = "#008762") +
  facet_wrap(~ sex) +
  labs(title = "Wpływ zainteresowania rodziców na wyniki w nauce.",
       x = "Średni wynik ucznia",
       y = "Wpływ rodzica na wyniki dziecka") +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("VERY WEAK", "WEAK", "STRONG", "VERY STRONG"))
#WNIOSEK - zainteresowanie rodzicow nie wplywa na wyniki uczniow - ale mozna zauwazyc ze rodzice prawie zawsze wspieraja swoje dzieci

#wczytujemy plik z danymi z kwestionariusza o szkole
School <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_SCH_QQQ", "cy6_ms_cmb_sch_qqq.sas7bdat") %>% as.data.table()

# Sprawdzamy czy typ szkoly publiczna/prywatna wplywa na wyniki w nauce
setkey(School, CNTSCHID)
setkey(GBR_results, CNTSCHID)
# xDDDDDDD w Walii wszystkie szkoly meskie sa publiczne a damskie prywatne 
GBR_results[School[,  .(CNTSCHID, SC013Q01TA)], nomatch=0][!is.na(SC013Q01TA),.(MEAN = mean(SC013Q01TA)), .(sex, region)][order(region)]

GBR_results[School[,  .(CNTSCHID, SC013Q01TA)], nomatch=0] %>% mutate(MEAN_RATE = (PV1MATH + PV1READ + PV1SCIE)/3) %>% as.data.table() -> type_schools
type_schools[!is.na(SC013Q01TA), ] -> type_schools
ifelse(type_schools$SC013Q01TA==1, "public", "private") -> type_schools$SC013Q01TA 

ggplot(type_schools) +
  geom_violin(aes(x = sex, y = MEAN_RATE)) +
  facet_wrap(~region + SC013Q01TA) +
  labs(title = "Średnie wyniki uczniow w zalezności od regionu i typu szkoly")
# WNIOSEK - w szkolach prywatnych wyniki sa troche lepsze niz w publicznych - u kobiet jest to bardziej zauwazalne, poprawia sie srednio o 100-200 pkt
#do wykresu wyzej mozna zrobic tez boxplota

#czy typ szkoly wplywa na zaangazowanie rodzicow?
setkey(School, CNTSCHID)
setkey(parents_influence, CNTSCHID)
parents_influence[School[, .(CNTSCHID,SC013Q01TA)], nomatch=0][!is.na(SC013Q01TA) & !is.na(MEAN_INFLU), ] -> parents_and_type_school
ifelse(parents_and_type_school$SC013Q01TA==1, "public", "private") -> parents_and_type_school$SC013Q01TA 

#wykresik do tematu z powyzszego komentarza
ggplot(parents_and_type_school) +
  geom_violin(aes(x=MEAN_RATE, y=MEAN_INFLU, fill = SC013Q01TA), alpha= .4) +
  facet_wrap(~ sex) +
  labs(title = "Zaangazowanie rodziców a wplyw na wyniki w zaleznosci od typu szkoly",
       x = "Średni wynik",
       y = "Zaangażowanie rodzica w życie dziecka") +
  scale_y_continuous(labels = c("VERY WEAK", "WEAK", "STRONG", "VERY STRONG")) +
  scale_x_continuous(breaks = seq(200, 800, 100)) +
  scale_fill_discrete(name = "Typ szkoły")
#wniosek - w szkolach prywatnych rodzice sie troche bardziej interesuja dziecmi, ale tylko troszke - trudno zauwazyc
#dodatkowo widac tez wyniki w zaleznosci od szkoly prywatnej/publicznej, czyli ze wyniki sa troche lepsze w szkolach prywatnych meskich i zenskich


### PODSUMOWANIE - do tej pory mamy 7 slabych wykresow ktore tak naprawde nic nie mowia

#sprawdzamy ilosc szkol w miastach o roznej ilosci populacji
#ten wykres nic nie pokazuje, wiec nie byl poprawiany
setkey(School, CNTSCHID)
setkey(parents_and_type_school, CNTSCHID)
School[!is.na(SC001Q01TA), .(CNTSCHID, SC001Q01TA)][parents_and_type_school, nomatch=0] -> tmp

ggplot(tmp) +
  geom_point(aes(x = sex, y = SC001Q01TA)) +
  facet_wrap(~ SC013Q01TA) +
  labs(title = "Rozmiar miejscowosci w jakiej jest szkola a typ szkoly") +
  scale_y_continuous( breaks = seq(1,5,1), labels = c("fewer than 3 000", "3 000 to about 15 000", "15 000 to about 100 000",
                                                      "100 000 to about 1 000 000", "with over 1 000 000"))

#sprobujmy zrobic to samo ale w tabeli zliczajac ilosc szkol w roznych wioskach

# ifelse(tmp$SC001Q01TA==1, "fewer than 3 000", tmp$SC001Q01TA) -> tmp$SC001Q01TA
# ifelse(tmp$SC001Q01TA==2, "3 000 to about 15 000", tmp$SC001Q01TA) -> tmp$SC001Q01TA
# ifelse(tmp$SC001Q01TA==3, "15 000 to about 100 000", tmp$SC001Q01TA) -> tmp$SC001Q01TA
# ifelse(tmp$SC001Q01TA==4, "100 000 to about 1 000 000", tmp$SC001Q01TA) -> tmp$SC001Q01TA
# ifelse(tmp$SC001Q01TA==5, "with over 1 000 000", tmp$SC001Q01TA) -> tmp$SC001Q01TA

a <- tmp[, .(COUNT = .N), .(SC013Q01TA, SC001Q01TA)][order(COUNT, decreasing = TRUE)] #najwiecej szkol publicznych w kazdej mozliwej kategorii miasta
b <- tmp[, .(COUNT = .N), .(sex, SC001Q01TA)][order(COUNT, decreasing = TRUE)] #roznie, ale najwiecej szkol mieszanych

#wykresiki
ggplot(a, aes(x = SC001Q01TA, y = COUNT, fill = SC013Q01TA)) +
  geom_bar(stat="identity", width = .6) +
  labs(x = "Wielkosc miasta",
       y = "Liczba szkol", 
       title = "Liczba szkol w zaleznosci od wielkosci miasta") + 
  scale_fill_discrete(name = "Typ szkoły") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1,5,1), labels = c("fewer than 3 000", "3 000 to about 15 000", "15 000 to about 100 000",
                                                      "100 000 to about 1 000 000", "with over 1 000 000"))
#wniosek - najwiecej szkol i prywatnych i publicznych jest w miastach srednich 15 tys do 100 tys

ggplot(b, aes(x = SC001Q01TA, y = COUNT, fill = sex)) +
  geom_bar(stat="identity", width = .6, alpha=.7) +
  labs(x = "Wielkosc miasta",
       y = "Liczba szkol", 
       title = "Liczba szkol w zaleznosci od wielkosci miasta") + 
  scale_fill_discrete(name = "Typ szkoły") +
  scale_fill_manual(values = c("pink", "royalblue4", "maroon3"))  +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous( breaks = seq(1,5,1), labels = c("fewer than 3 000", "3 000 to about 15 000", "15 000 to about 100 000",
                                                      "100 000 to about 1 000 000", "with over 1 000 000"))
#wniosek - analogiczne jak wyzej