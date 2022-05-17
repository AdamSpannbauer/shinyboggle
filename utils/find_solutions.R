library(hunspell)


row_seq <- function(df) {
  seq_len(nrow(df))
}


to_chars <- function(word) {
  strsplit(word, "")[[1]]
}


in_dictionary <- function(word) {
  hunspell_check(word)
}


long_enough <- function(chars, min_len = 3) {
  length(chars) >= min_len
}


all_chars_present <- function(chars, letter_matrix) {
  all(chars %in% letter_matrix)
}


is_leaf_node <- function(node = list(children = list())) {
  length(node$children) == 0
}


find_letter_locs <- function(char, letter_matrix, depth = NA_integer_, prow = NULL, pcol = NULL) {
  locs <- which(letter_matrix == char, arr.ind = TRUE)
  locs <- as.data.frame(locs)

  locs_list <- lapply(row_seq(locs), function(i) {
    r <- locs[i, ]
    list(
      row = r$row,
      col = r$col,
      char = char,
      children = list(),
      depth = depth
    )
  })

  if (is.integer(prow) && is.integer(pcol)) {
    locs_list <- Filter(
      function(loc) {
        drow <- abs(loc$row - prow)
        dcol <- abs(loc$col - pcol)
        if (drow == 0 & dcol == 0) {
          return(FALSE)
        }
        if (drow > 1 | dcol > 1) {
          return(FALSE)
        }
        return(TRUE)
      },
      locs_list
    )
  }

  locs_list
}


extend_tree <- function(tree, chars, letter_matrix) {
  for (i in seq_along(tree$children)) {
    parent <- tree$children[[i]]
    d <- parent$depth + 1

    new_letter_matrix <- letter_matrix
    new_letter_matrix[parent$row, parent$col] <- parent$depth

    if (d > length(chars)) {
      tree$children[[i]]$solution_matrix <- new_letter_matrix
      break
    }

    tree$children[[i]]$children <- find_letter_locs(
      chars[d],
      new_letter_matrix,
      depth = d,
      prow = parent$row,
      pcol = parent$col
    )

    tree$children[[i]] <- extend_tree(tree$children[[i]], chars, new_letter_matrix)
  }

  return(tree)
}


find_item <- function(your_list, key, i_path = c()) {
  if (key %in% names(your_list)) {
    i <- which(names(your_list) == key)
    item <- your_list[[key]]
    res <- list(
      item = item,
      i_path = i
    )

    return(res)
  }

  for (i in seq_along(your_list)) {
    item <- your_list[[i]]
    if (is.list(item)) {
      res <- find_item(item, key, i_path)
      if (!is.null(res)) {
        res$i_path <- c(i, res$i_path)
        return(res)
      }
    }
  }

  return(NULL)
}


null_at_i_path <- function(your_list, i_path) {
  if (length(i_path) == 1) {
    your_list[[i_path]] <- NULL
    return(your_list)
  }

  i <- i_path[1]
  your_list[[i]] <- null_at_i_path(your_list[[i]], i_path[-1])

  return(your_list)
}


extract_solution_matrices <- function(tree) {
  solutions <- list()
  done <- FALSE
  while (!done) {
    res <- find_item(tree, key = "solution_matrix")

    if (!is.null(res)) {
      solutions[[length(solutions) + 1]] <- res$item
      tree <- null_at_i_path(tree, res$i_path)
    } else {
      done <- TRUE
    }
  }

  solutions <- Filter(length, solutions)
  return(solutions)
}


solution_matrix_to_df <- function(solution_matrix) {
  idxs <- list()
  for (i in seq_along(solution_matrix)) {
    idx <- which(solution_matrix == i, arr.ind = TRUE)

    if (nrow(idx) == 0) {
      break
    }

    idxs[[i]] <- idx
  }

  as.data.frame(do.call(rbind, idxs))
}


build_full_tree <- function(chars, letter_matrix) {
  tree <- list(children = list(), depth = 0)
  tree$children <- find_letter_locs(chars[1], letter_matrix, depth = 1)

  extend_tree(tree, chars, letter_matrix)
}


find_solutions <- function(word, letter_matrix, min_len = 3) {
  chars <- to_chars(word)

  if (!long_enough(chars, min_len)) {
    return(NULL)
  }
  if (!all_chars_present(chars, letter_matrix)) {
    return(NULL)
  }

  if (!in_dictionary(word)) {
    return(NULL)
  }

  tree <- build_full_tree(chars, letter_matrix)
  solutions <- extract_solution_matrices(tree)
  solutions <- lapply(solutions, function(solution_matrix) {
    solution_df <- solution_matrix_to_df(solution_matrix)
    solution_df$char <- chars

    list(
      solution_matrix = solution_matrix,
      solution_df = solution_df
    )
  })

  return(solutions)
}
