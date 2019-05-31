#' Search the NUTS3 code.
#'
#' Using NUTS3-Zipcode correspondence tables of eurostat, the function returns the NUTS3 code for
#' a specific zipode.
#'
#' @param zipcode alphanumeric; it specifies the zipcode of interest (symbol - is allowed)
#' @param country character (length == 2), it specifies the country code Aplha-2 of the country.
#'
#' @return The function returns a class "alphanumeric" object containing the NUTS3 code of the pre-specified zipcode.
#'
#' @author Massimo Caprari
#'
#' @references \href{https://it.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}, \href{http://ec.europa.eu/eurostat/tercet/flatfiles.do}{Eurostat: Postcode and Nuts}
#'
#' @examples
#' ## Not Run:
#'
#'Find_Nuts("10-004", "pl")
#'
#'Find_Nuts("110 00", "cz")
#'
#'# Due to dimension of the file, uk may be take time.
#'Find_Nuts("CV1 5FB", "uk")
#'
#'
#'
#'## End (Not run)
#' @export
Find_Nuts <- function(zipcode, country) {
temp <- tempfile()
base <- "http://ec.europa.eu/eurostat/tercet/download.do?file=pc2018_"
factor <- country
format<-"_NUTS-2016_v1.0.zip"
full_url <- glue(base, factor, format, sep ="")
download.file(full_url, temp, quiet = TRUE)
folder_name <- paste0("pc2018_", country,"_NUTS-2016_v1.0.csv")
Zip_Nuts <- read.csv(unz(temp, folder_name), sep = ";")
Zip_Nuts <- as.data.frame(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", as.matrix(Zip_Nuts)))
match_position <- match(zipcode, Zip_Nuts$CODE)
nuts_vector <- as.vector(Zip_Nuts$NUTS3)
my_nuts <- nuts_vector[match_position]
return(my_nuts)
}
