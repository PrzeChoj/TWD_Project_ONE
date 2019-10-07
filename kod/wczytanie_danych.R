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

# student2_drugi_folder <- file.path(dane.dic, "PUF_SAS_COMBINED_CMB_STU_QQQ")
# student2_pierwszy_plik <- file.path(student2_drugi_folder, "cy6_ms_cmb_stu_qq2.sas7bdat")
# tmp1 <- read_sas(student2_pierwszy_plik)
# 
# student2_drugi_plik <- file.path(student2_drugi_folder, "cy6_ms_cmb_stu_qqq.sas7bdat") # dużo kolumn, nazwy niektórych są z kweestionariuszy rodziców, o karierze itp.
# tmp2 <- read_sas(student2_drugi_plik)

# Wczytywanie po kolei wszystkich danych jest żmudne i słabo zrobione - lepiej stowrzyć jedną funkcję

create_data_from_sas_format <- function(path, dictionary, file) {
  
  stopifnot(is.character(path), is.character(dictionary), is.character(file))
  
  path <- file.path(path, dictionary, file)
  new_data <- read_sas(path)
  new_data
}

tmp3 <- create_data_from_sas_format(dane.dic, "PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qq2.sas7bdat")
#tmp3 == tmp1


