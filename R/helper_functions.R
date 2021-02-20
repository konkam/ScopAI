
Colours = c("B", "D", "S", "C")

ColoursDict = setNames(object = c("Spade", "Coppe", "Denari", "Bastoni"), nm = c("S", "C", "D", "B"))

#' Title
#'
#' @param letter
#'
#' @return
#' @export
#'
#' @examples
GiveFullColourName = function(letter) {
  return(ColoursDict[letter])
}
