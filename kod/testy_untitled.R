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











