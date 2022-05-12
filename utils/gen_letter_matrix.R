scrabble_points <- list(
  `1` = c("A", "E", "I", "O", "U", "L", "N", "S", "T", "R"),
  `2` = c("D", "G"),
  `3` = c("B", "C", "M", "P"),
  `4` = c("F", "H", "V", "W", "Y"),
  `5` = c("K"),
  `8` = c("J", "X"),
  `10` = c("Q", "Z")
)

scrabble_dfs <- lapply(seq_along(scrabble_points), function(i) {
  chars <- scrabble_points[[i]]
  val <- as.numeric(names(scrabble_points)[i])
  data.frame(
    letter = chars,
    points = rep(val, length(chars))
  )
})

SCRABBLE_DF <- do.call(rbind, scrabble_dfs)
SCRABBLE_DF$prob <- (11 - SCRABBLE_DF$points) / 11


gen_letter_matrix <- function(nrows = 5, ncols = 5) {
  s <- sample(
    x = SCRABBLE_DF$letter,
    prob = SCRABBLE_DF$prob,
    size = nrows * ncols,
    replace = TRUE
  )
  matrix(s, nrows, ncols)
}
