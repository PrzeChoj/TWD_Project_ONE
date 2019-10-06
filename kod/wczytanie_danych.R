# wczytanie danych PISA z folderu ./../Dane

# UWAGA! Nie wczytywac ich calych na raz, dane sa zbyt duze!(5GB)

# nie zdecydowalem sie ktorego czytnika SAS'ow urzywac
install.packages("sas7bdat")
library("sas7bdat")        # u mnie dziala bardzo powoli
install.packages("haven")
library("haven")           # dziala 15 razy szybciej

katalog_bazowy <- getwd()
dane.dic <- file.path(katalog_bazowy, "Dane") # w tym folderze sa dane


# Wczytanie damych z kwestjonariusza szkolnego
school_folder <- file.path(dane.dic, "PUF_SAS_COMBINED_CMB_SCH_QQQ")
school_plik <- file.path(school_folder, "cy6_ms_cmb_sch_qqq.sas7bdat")
school <- read.sas7bdat(school_plik)
school <- read_sas(school_plik)



student_pierwszy_folder <- file.path(dane.dic, "PUF_SAS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH")
student_pierwszy_plik <- file.path(student_pierwszy_folder, "CY6_MS_CM2_STU_COG.sas7bdat")
tmp <- read_sas(student_pierwszy_plik)

duze_dane <- read.sas7bdat(plik)













