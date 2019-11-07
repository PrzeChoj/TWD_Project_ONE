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


# przyklad:
# tmp_duzy <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qqq.sas7bdat")
# procent_panstw(tmp_duzy[c(2,164)])



procent_sex <- function(nazwa){
  # potrzebuje ramki GBR
  dane <- GBR[!is.na(nazwa), c("sex", nazwa)]
  procent_panstw(dane)
}

srednia_prywatnych <- function(nazwa){
  # potrzebuje ramki tbl_ciekawe
  dane <- tbl_ciekawe_GBR_ext %>% select("SC013Q01TA", "CNTSTUID.x", nazwa) %>% filter(!is.na(SC013Q01TA)) %>% filter(!duplicated(CNTSTUID.x))
  pub <- (dane %>% filter(SC013Q01TA == 1))[[nazwa]]
  pryw <- (dane %>% filter(SC013Q01TA == 2))[[nazwa]]
  
  odp <- c(mean(pub, na.rm = TRUE), mean(pryw, na.rm = TRUE))
  names(odp) <- c("publiczna", "prywatna")
  odp
}

srednia_prywatnych_POL <- function(nazwa){
  # potrzebuje ramki tbl_ciekawe_POL
  dane <- tbl_ciekawe_POL_ext %>% select("SC013Q01TA", "CNTSTUID", nazwa) %>% filter(!is.na(SC013Q01TA)) %>% filter(!duplicated(CNTSTUID))
  pub <- (dane %>% filter(SC013Q01TA == 1))[[nazwa]]
  pryw <- (dane %>% filter(SC013Q01TA == 2))[[nazwa]]
  
  odp <- c(mean(pub, na.rm = TRUE), mean(pryw, na.rm = TRUE))
  names(odp) <- c("publiczna", "prywatna")
  odp
}

srednia_prywatnych_porownanie <- function(nazwa){
  out <- rbind(srednia_prywatnych(nazwa), srednia_prywatnych_POL(nazwa))
  rownames(out) <- c("GBR", "POL")
  out
}


# 05.11.19
srednia_sponsorowanych_GBR <- function(nazwa){
  # potrzebuje ramki tbl_ciekawe_GBR_ext
  dane <- tbl_ciekawe_GBR_ext %>% select("type", "CNTSTUID.x", nazwa) %>% filter(!is.na(type)) %>% filter(!duplicated(CNTSTUID.x))
  akademicka <- (dane %>% filter(type == "akademicka"))[[nazwa]]
  niezalezna <- (dane %>% filter(type == "niezalezna"))[[nazwa]]
  utrzymywana <- (dane %>% filter(type == "utrzymywana"))[[nazwa]]
  
  odp <- c(mean(akademicka, na.rm = TRUE), mean(niezalezna, na.rm = TRUE), mean(utrzymywana, na.rm = TRUE))
  names(odp) <- c("akademicka", "niezalezna", "utrzymywana")
  odp
}




do_plota <- function(dt, kolumny=TRUE){
  # funkcja przyjmuje macierz wygenerowana przez procent_panstw i przetabia ja
    # na dobra do wrzucenia do ggplota
  nazwy_col <- colnames(dt)
  nazwy_row <- rownames(dt)
  wynik <- matrix(ncol = 3, nrow = length(nazwy_row) * length(nazwy_col))
  
  if(kolumny){
    wynik[,1] <- rep(nazwy_col, each = length(nazwy_row))
    wynik[,2] <- rep(nazwy_row, times = length(nazwy_col))
    colnames(wynik) <- c("kolumny", "wiersze", "dane")
    wynik <- tbl_df(wynik)
    
    # petla jest OK, bo to bardzo mala ramka zawsze
    for(i in 1:length(nazwy_col)){
      for(j in 1:length(nazwy_row)){
        wynik[(i-1)*length(nazwy_row)+j,3] <- dt[j,i] %>% substr(1, 5) # tylko 3 cyfry, bo i tak to jest procent, to male roznice
      }
    }
  }
  if(!kolumny){
    wynik[,1] <- rep(nazwy_row, each = length(nazwy_col))
    wynik[,2] <- rep(nazwy_col, times = length(nazwy_row))
    colnames(wynik) <- c("wiersze", "kolumny", "dane")
    wynik <- tbl_df(wynik)
    
    # petla jest OK, bo to bardzo mala ramka zawsze
    for(i in 1:length(nazwy_row)){
      for(j in 1:length(nazwy_col)){
        wynik[(i-1)*length(nazwy_col)+j,3] <- dt[i,j] %>% substr(1, 5) # tylko 3 cyfry, bo i tak to jest procent, to male roznice
      }
    }
  }
  
  wynik$dane <- wynik$dane %>% as.numeric()
  
  wynik
}


wykres_korkow <- function(n){
  name <- attr(tmp_duzy[[col_ext[n]]], "label")
  title <- name %>% substr(1, 10)
  subtitle <- name %>% substr(60, nchar(name))
  
  data <- do_plota(srednia_prywatnych_porownanie(col_ext[n]))
  data$wyniki <- data$dane
  data$nazwa <- paste(as.character(data$wyniki * 100), "%", sep="")
  
  ggplot(data, aes(x=wiersze, fill=kolumny)) +
    geom_bar(stat="identity", aes(y=wyniki), position=position_dodge(), colour=NA) +
    geom_text(aes(label = nazwa, y=wyniki-0.02, fontface="bold"),
              position=position_dodge(width=0.9), colour = "#000000", size=3.5) +
    #geom_text(y=0, label=name, size = 5, theta = 10)
    labs(title = title, subtitle = subtitle)
}






