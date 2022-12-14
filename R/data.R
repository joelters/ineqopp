#' Encuesta de Condiciones de Vida (ECV 2019) Madrid
#'
#' A subset of the Spanish ECV 2019 survey which is part of EUSILC.
#' The subset restricts data to Madrid and is cleaned to have only
#' data on income and circumstances.
#'
#' @format ## `mad2019`
#' A data frame with 1571 rows and 25 columns:
#' \describe{
#'   \item{weight}{Survey weights}
#'   \item{sex}{Sex}
#'   \item{nadults}{Number of adults living in household when indivual was 14}
#'   \item{nkids}{Number of kids living in household when indivual was 14}
#'   \item{nadultsworking}{Number of working adults living in household when indivual was 14}
#'   \item{educM}{Mother's education}
#'   \item{educM}{Mother's education}
#'   \item{occstatF}{Occupational status of the father}
#'   \item{managF}{Did father hold a managerial position}
#'   \item{occF}{Occupation of the father}
#'   \item{occstatM}{Occupational status of the mother}
#'   \item{finsit}{Household's financial situation when individual was 14}
#'   \item{tenancy}{ownership status of house when individual was 14}
#'   \item{livingwithM}{Living with the mother when individual was 14}
#'   \item{livingwithF}{Living with the father when individual was 14}
#'   \item{munipop}{Municipality's population when individual was 14}
#'   \item{schneeds}{Did individual have basic school needs covered}
#'   \item{foodneeds}{Did individual have basic food needs covered}
#'   \item{holidays}{Holidays at least once a year when individual was 14}
#'   \item{ctrybirth}{Country of birth}
#'   \item{contbornF}{Continent of birth father}
#'   \item{contnatF}{Continent nationality father}
#'   \item{contbornM}{Continent of birth mother}
#'   \item{contnatM}{Continent nationality mother}
#'   ...
#' }
#' @source <https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176807&menu=ultiDatos&idp=1254735976608>
"mad2019"
