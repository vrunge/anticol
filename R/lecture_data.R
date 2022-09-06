
#' read_anticho
#' @description Reading a csv file of prescription data
#' @param file file name
#' @param sep separator
#' @param dec decimal symbol
#' @param header true or false
#' @param quote quote parameter
#' @param fill true or false (fill the non available data by NA)
#' @return a data.table object (see data.table package for further information)
read_anticho <- function(file = "CH-Rouffach-Prescriptions-2008-2018_v1.txt.csv",
                         sep = "|",
                         dec = ".",
                         header = TRUE,
                         quote = "",
                         fill = FALSE)
{
  PRESC <- data.table::fread(
    file = file,
    sep = sep,
    dec = dec,
    header = header,
    quote = quote,
    fill = fill,
    check.names=T)
  return(PRESC)
}



#' standardize_variables
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

  ### as character
  data$DCI.produit <- as.character(data$DCI.produit)
  data$Nom.commercial.produit <- as.character(data$Nom.commercial.produit)
  data$observation.ligne.prescription <- as.character(data$observation.ligne.prescription)

  ### as double
  data$IMC <- as.double(data$IMC)/100

  ### as factor
  data$Si.besoin = as.factor(data$Si.besoin)
  data$Heure.de.prise = as.factor(data$Heure.de.prise)


  data$Date.heure.debut.d.adm. <- as.POSIXct(as.character(data$Date.heure.debut.d.adm.),
                                             format = "%d/%m/%Y %H:%M")
  data$Date.heure.fin.d.adm. <- as.POSIXct(as.character(data$Date.heure.fin.d.adm.),
                                           format = "%d/%m/%Y %H:%M")
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

