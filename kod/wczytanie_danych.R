# wczytanie danych PISA z folderu ./../Dane

# UWAGA! Nie wczytywac ich calych na raz, dane sa zbyt duze!(5GB)


install.packages("sas7bdat")
library("sas7bdat")

katalog_bazowy <- getwd()
dane.dic <- file.path(katalog_bazowy, "Dane") # w tym folderze sa dane


# Czemu sa w takie same pliki?
pierwszy_plik <- file.path(dane.dic, "CY6_MS_CM2_SCH_QQQ.sas7bdat")
mySASData <- read.sas7bdat(pierwszy_plik)

drugi_plik <- file.path(dane.dic, "CY6_MS_CM2_SCH_QQQ.sas7bdat.format.sas")
mySASData2 <- read.sas7bdat(pierwszy_plik)
all.equal(mySASData2, mySASData)
