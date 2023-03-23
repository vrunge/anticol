

#' read_antichol
#'
#' @description Reading a csv file of prescription data
#' @param file file name
#' @param sep separator
#' @param dec decimal symbol
#' @param header true or false
#' @param quote quote parameter
#' @param fill true or false (fill the non available data by NA)
#' @param encoding Latin-1 or UTF-8
#' @return a data.table object (see data.table package for further information)
read_antichol <- function(file = "../mydata/0_CH-Rouffach-Prescriptions-2008-2018_v1.txt.csv",
                         sep = "|",
                         dec = ".",
                         header = TRUE,
                         quote = "",
                         fill = FALSE,
                         encoding = "Latin-1")
{
  PRESC <- data.table::fread(
    file = file,
    sep = sep,
    dec = dec,
    header = header,
    quote = quote,
    fill = fill,
    encoding = encoding,
    check.names = T)
  return(PRESC)
}



#' standardize_variables
#'
#' @description changing variable type, process missing data
#' @param data data table object
#' @return standardized data.table object
standardize_variables <- function(data)
{
  #### ADD NA when necessary
  data$DCI.produit[data$DCI.produit == ""] <- NA
  data$observation.ligne.prescription[data$observation.ligne.prescription == ""] <- NA
  data$Rythme.date.de.debut[data$Rythme.date.de.debut == ""] <- NA
  data$Taille[data$Taille < 1] <- NA

  ### as double
  data <- data %>% dplyr::mutate(IMC = as.double(IMC))

  ### as factor
  data <- data %>% dplyr::mutate(Si.besoin = as.factor(Si.besoin))
  data <- data %>% dplyr::mutate(Heure.de.prise = as.factor(Heure.de.prise))

  ### as character
  data <- data %>% dplyr::mutate(DCI.produit = as.character(DCI.produit))
  data <- data %>% dplyr::mutate(Nom.commercial.produit = as.character(Nom.commercial.produit))
  data <- data %>% dplyr::mutate(observation.ligne.prescription = tolower(observation.ligne.prescription))

  ########## CONVERSION DES DATES
  # Conversion de Rythme.date.de.debut en date
  data <- data %>%
    dplyr::mutate(Rythme.date.de.debut =
             as.Date(Rythme.date.de.debut,
                     format = "%d/%m/%Y"))

  # Conversion de Date.heure.debut.d.adm.
  # (date avec heures minutes)
  data <- data %>%
    dplyr::mutate(Date.heure.debut.d.adm. =
             as.POSIXct(as.character(Date.heure.debut.d.adm.),
                        format = "%d/%m/%Y %H:%M"))

  # Conversion de Date.heure.fin.d.adm.
  # (date avec heures minutes)
  data <- data %>%
    dplyr::mutate(Date.heure.fin.d.adm. =
             as.POSIXct(as.character(Date.heure.fin.d.adm.),
                        format = "%d/%m/%Y %H:%M"))
  return(data)
}


#' delete_variables
#' @description delete variables under the threshold
#' @param data data table object
#' @param threshold delete variable with a percent of missing value > 1 - threshold
#' @return a data.table object
delete_variables <- function(data, threshold = 0.1)
{
  return(data)
}

