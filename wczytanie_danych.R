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
























