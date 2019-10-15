# wczytanie danych PISA z folderu ./../Dane

# UWAGA! Nie wczytywac ich calych na raz, dane sa zbyt duze!(5GB)

# funkcja read_sas z pakietu heaven dziala duzo szybciej niz funkcja sas7bdat z pakietu sas7bdat
install.packages("haven")
library("haven")

# funkcja do wszystkich danych
create_data_from_sas_format <- function(dictionary, file) {
  dane.dic <- file.path(getwd(), "Dane")
  
  stopifnot(is.character(dictionary), is.character(file))
  
  path <- file.path(dane.dic, dictionary, file)
  new_data <- read_sas(path)
  new_data
}

# kwastjonariusz szkolny
school <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_SCH_QQQ", "cy6_ms_cmb_sch_qqq.sas7bdat")


# nie wiem co to za plik
tmp <- create_data_from_sas_format("PUF_SAS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH", "CY6_MS_CM2_SCH_QQQ.sas7bdat")  # 691x273
tmp <- create_data_from_sas_format("PUF_SAS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH", "CY6_MS_CM2_STU_COG.sas7bdat")  # 23051x2048
tmp <- create_data_from_sas_format("PUF_SAS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH", "CY6_MS_CM2_STU_CPS.sas7bdat")  # 8861x190
tmp <- create_data_from_sas_format("PUF_SAS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH", "cy6_ms_cm2_stu_qq2.sas7bdat")  # 28266x21
tmp <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QTM", "cy6_ms_cmb_stu_qtm.sas7bdat")

tmp3 <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qq2.sas7bdat")




tmp_maly <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qq2.sas7bdat") # 519334x21
tmp_duzy <- create_data_from_sas_format("PUF_SAS_COMBINED_CMB_STU_QQQ", "cy6_ms_cmb_stu_qqq.sas7bdat") # 519334x921





























