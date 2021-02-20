GetColorsOfCards <- function(cards) {
  sapply(deck_dict[cards], "[[", 1)
}
GetValuesOfCards <- function(cards) {
  sapply(deck_dict[cards], "[[", 2)
}
GetPrimieraValuesOfCards <- function(cards) {
  primiera_dict[GetValuesOfCards(cards)]
}
SubsetOneColourInCards <- function(cards, to_subset = "D") {
  cards[GetColorsOfCards(cards) == to_subset]
}
SubsetOneValueInCards <- function(cards, to_subset = 7) {
  cards[GetValuesOfCards(cards) == to_subset]
}
CountDenariNumber <- function(cards) {
  length(SubsetOneColourInCards(cards))
}
CountCardsNumber <- function(cards) {
  length(cards)
}
CountSevenNumber <- function(cards) {
  length(SubsetOneValueInCards(cards))
}
CountPrimiera <- function(cards) {
  sum(max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "D"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "B"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "S"))),
      max(GetPrimieraValuesOfCards(SubsetOneColourInCards(cards, "C"))))
}

new_deck <- ShuffleNewDeck()
toto <- new_deck[1:3]
GetColorsOfCards(toto)
GetValuesOfCards(toto)
SubsetOneColourInCards(toto, "C")
CountDenariNumber(toto)
GetPrimieraValuesOfCards(toto)
CountPrimiera(new_deck)



