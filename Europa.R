#kolumna CNT zawiera cale nazwy panstw

#sciezka do pliku 
file <- file.path("dane" ,"PUF_SAS_COMBINED_CMB_SCH_QQQ", "CY6_MS_CMB_SCH_QQQ.sas7bdat.format.sas")
#poczatek wczytywania danych - od tego nr wiersza zaczyna sie kolumna CNT
first_line <- grep("value \\$CNT", readLines(file))
#szukamy konca wczytywania - w pliku z oznaczeniami koniec kolumny oznaczany jest przez ";"
tmp <- grep(";", readLines(file))
end_line <- tmp[tmp > first_line] #to jest nr wiersza posiadajacy ";" po kolumnie statum
#wczytanie oznaczen
Labels <- read.table(file,sep = "=", skip=first_line,nrows=(end_line[1]-first_line-1), stringsAsFactors = FALSE) %>% as.data.table()
colnames(Labels) <- c("CNT", "Label")

Labels$CNT <- Labels$CNT %>% substr(1,3)

#wczytujemy plik z danymi z kwestionariusza o uczniach
Student <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qqq.sas7bdat") %>% as.data.table()
#wczytujemy plik z danymi z kwestionariusza o szkole
School <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_SCH_QQQ", "cy6_ms_cmb_sch_qqq.sas7bdat") %>% as.data.table()

#nazwy panstw takie jak w DF Labels i tylko te panstwa ktore sa w DF Labels
europa <- c(" Albania", " Austria", " Belgium", " Bulgaria", " Croatia", " Cyprus", " Czech Republic", " Denmark",
            " Estonia", " Finland", " France" ," Georgia", " Germany", " Greece", " Hungary", " Iceland", " Ireland", 
            " Italy", " Kazakhstan", " Latvia", " Lithuania", " Luxembourg"," Malta", " Moldova", " Montenegro",
            " Netherlands", " Norway", " Poland", " Portugal", " Romania", " Russian Federation", " Slovak Republic", 
            " Slovenia", " Spain", " Sweden", " Switzerland", " Turkey", " United Kingdom") %>% as.data.table()

setkey(Labels, Label)
setkey(europa, .)
Labels[europa, nomatch = 0] -> Labels2

Europa_Results <- Student[
                          , .(CNT, PV1MATH, PV1READ, PV1SCIE)
                          ][
                            , .(MEAN = (PV1MATH+PV1READ+PV1SCIE)/3), CNT
                          ][
                            , .(MEDIAN = median(MEAN), MAX = max(MEAN), MIN= min(MEAN)), CNT
                            ]

setkey(Europa_Results, CNT)
setkey(Labels2, CNT)
Europa_Results <- Europa_Results[Labels2, nomatch=0][order(MEAN)]

cbind(Europa_Results[, .(MEDIAN, Label)], rep("median", length(Europa_Results$MEAN))) -> A
cbind(Europa_Results[, .(MAX, Label)], rep("max", length(Europa_Results$MEAN))) -> B
cbind(Europa_Results[, .(MIN, Label)], rep("min", length(Europa_Results$MEAN))) -> C
rbindlist(list(A, B, C)) -> Europa

Europa[order(MEAN, decreasing = TRUE)] -> Europa

ggplot(Europa) +
  geom_point(aes(reorder(Label, MEAN), MEAN, colour = V2), stat = "identity") +
  theme_bw()+
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 850)) +
  labs(x = "Państwa europejskie",
        y = "Średnie wyniki w państwie z testów PISA",
        title = "Porównanie wyników państw z Europy",
       color = "Wynik testu") +
  geom_vline(xintercept = c(29,0), size = 1, 
             color = "grey", linetype = "dashed", alpha=0.5) +
  annotate("point", x = 29, y = 813.8760, colour = "red", size=4) +
  annotate("point", x = 29, y = 497.4420, colour = "green", size=4) +
  annotate("point", x = 29, y = 175.8683, colour = "blue", size=4)

#panstwa uznane w 2016 za panstwa o najlepszej struktorze szkolnictwa
country <- c(" Finland", " Japan", " United Kingdom", " Denmark", " Russian Federation", " Norway", " Sweden", " Israel", " Hong Kong",
            " Netherlands", " Belgium", " Germany", " China", " Singapore", " Portugal", " Hungary", " Estonia", " France", " United States") %>% as.data.table()
setkey(Labels, Label)
setkey(country, .)
Labels[country, nomatch = 0] -> Labels3

World_Results <- Student[
  , .(CNT, PV1MATH, PV1READ, PV1SCIE)
  ][
    , .(MEAN = (PV1MATH+PV1READ+PV1SCIE)/3), CNT
    ][
      , .(MEDIAN = median(MEAN), MAX = max(MEAN), MIN= min(MEAN)), CNT
      ]

setkey(World_Results, CNT)
setkey(Labels3, CNT)
World_Results <- World_Results[Labels3, nomatch=0][order(MEDIAN)]

cbind(World_Results[, .(MEDIAN, Label)], rep("median", length(World_Results$MEDIAN))) -> A
cbind(World_Results[, .(MAX, Label)], rep("max", length(World_Results$MEDIAN))) -> B
cbind(World_Results[, .(MIN, Label)], rep("min", length(World_Results$MEDIAN))) -> C
rbindlist(list(A, B, C)) -> World

World[order(Label, decreasing = TRUE)] -> World

#x <- World$Label %>% match(c(" Finland", " Japan", " Denmark", " Russian Federation", " Norway", " United Kingdom", " Sweden", " Israel", " Hong Kong",
#                        " Netherlands", " Belgium", " Germany", " China", " Singapore", " Portugal", " Hungary", " Estonia", " France", " United States"))

ggplot(World) +
  geom_point(aes(Label, MEDIAN, colour = V2), stat = "identity") +
  theme_bw()+
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 850)) +
  labs(x = "Państwa z najlepszymi systemami szkolnictwa",
       y = "Średnie wyniki w państwie z testów PISA",
       title = "Porównanie wyników państw o najlepszych systemach szkolnictwa",
       color = "Wynik testu") +
  geom_vline(xintercept = c(17,0), size = 1, 
             color = "grey", linetype = "dashed", alpha=0.5) +
  geom_hline(yintercept = c(0,497.4420), size = 1, 
             color = "grey", linetype = "dashed", alpha=0.5) +
  annotate("point", x = 17, y = 813.8760, colour = "red", size=4) +
  annotate("point", x = 17, y = 497.4420, colour = "green", size=4) +
  annotate("point", x = 17, y = 175.8683, colour = "blue", size=4)
