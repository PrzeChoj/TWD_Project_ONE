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






