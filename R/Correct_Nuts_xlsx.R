#' Find NUTS of a register file with xlsx extension using 2016 eurostat NUTS.
#'
#' Using NUTS3-Zipcode correspondence tables of eurostat, the function import the pre-specified xlsm file
#' and insert all the 2016 eurostat NUTS basing on the ZIPCODE.
#'
#' @param name_of_the_file It is the name of the register file without the format extension. In order to avoid errors,
#' it is highly recommended to set the folder that contains the file as the working directory of R.
#' @param country character (length == 2), it specifies the country code Aplha-2 of the country.
#' @param csv logical (T/F). It specifies if a csv file (clone of the location worksheet but updated),
#' has to be created in the working directory or not.
#'
#' @return The function returns a dataframe (or a csv file) that contains the location worksheet updated with
#' all the available NUTS-3 code.
#'
#' @author Massimo Caprari
#'
#' @references \href{https://it.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}, \href{http://ec.europa.eu/eurostat/tercet/flatfiles.do}{Eurostat: Postcode and Nuts}
#'
#' @examples
#' ## Not Run:
#' Belgium <- Correct_Nuts_xlsm("register_ES_final", "es")
#'
#'
#'  Correct_Nuts_xlsm("register_ES_final", "es", T)
#'
#'
#'## End (Not run)
#' @export
Correct_Nuts_xlsx <- function(name_of_the_file, country, csv=F, format="xlsm") {

  name_plus_format <- paste0(name_of_the_file, ".xlsx")

  location <- read_xlsx(name_plus_format, sheet = "location")
  location <- location [-c(1, 2) , ]

  temp <- tempfile()
  base <- "http://ec.europa.eu/eurostat/tercet/download.do?file=pc2018_"
  factor <- country
  format<-"_NUTS-2016_v1.0.zip"
  full_url <- glue(base, factor, format, sep ="")
  download.file(full_url, temp, quiet = TRUE)
  folder_name <- paste0("pc2018_", country,"_NUTS-2016_v1.0.csv")
  Zip_Nuts <- read.csv(unz(temp, folder_name), sep = ";")


  Zip_Nuts <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(Zip_Nuts)))
  match_vector <- match(location$Postcode, Zip_Nuts$CODE)
  nuts_vector <- as.vector(Zip_Nuts$NUTS3)
  new_vector <- numeric(0)

  for (i in match_vector) {
    new_vector <- c(new_vector, nuts_vector[i])
  }

  location[["NUTS 3 region"]] <- new_vector

  if(csv==T) {
    File_Name <- paste0(country, "_Zip_Nuts_Corrected.csv")
    write.csv(location, File_Name)
  }
  if(csv==F){
    return(location)
  }
}
