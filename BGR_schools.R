
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
#zmienic kropki na cos innego TODO
ggplot(Results_in_mixed_school) +
  geom_point(aes(x = ST004D01T, y=Mean_math), stat = "identity", color="red") +
  geom_point(aes(x = ST004D01T, y=Mean_read), stat = "identity", color="green") +
  geom_point(aes(x = ST004D01T, y=Mean_science), stat = "identity", color="blue") +
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

#wczytujemy plik z danymi z kwestionariusza o szkole
School <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_SCH_QQQ", "cy6_ms_cmb_sch_qqq.sas7bdat") %>% as.data.table()

setkey(School, STRATUM)
setkey(GBR_schools, STRATUM)
School[GBR_schools] -> y
