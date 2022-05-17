source("utils/gen_letter_matrix.R")
source("utils/find_solutions.R")

MIN_WORD_LEN <- 3

# Preset demo ------------------------------------------------------------------
set.seed(42)
letter_matrix <- gen_letter_matrix()
cat("GAME BOARD:\n")
letter_matrix

valid_words <- c("TOP", "TOTE", "BED", "POT", "TOE")
invalid_words <- c("GTK", "TO", "QUIET")

word <- sample(valid_words, 1)
# 3 letter
find_solutions("TOP", letter_matrix, MIN_WORD_LEN)

# 4 letter
find_solutions("TOTE", letter_matrix, MIN_WORD_LEN)

# Two possible solutions
find_solutions("COT", letter_matrix, MIN_WORD_LEN)

# Letters not on board
find_solutions("QUIET", letter_matrix, MIN_WORD_LEN)

# Too short
find_solutions("TO", letter_matrix, MIN_WORD_LEN)

# Not a word
find_solutions("GTK", letter_matrix, MIN_WORD_LEN)

# Freestyle demo ------------------------------------------------------------------

letter_matrix <- gen_letter_matrix(nrows = 10, ncols = 10)
letter_matrix

# Input your answer here
find_solutions("SPY", letter_matrix, MIN_WORD_LEN)
