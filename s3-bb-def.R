
Sys.setenv(LANGUAGE = "en")


new_bb <- function(text) {                        
  # replace ((consonants) (1-2 vowels) (consonants)) with        
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
}

bb <- function(text) {                            # generic
  UseMethod("bb", text)
}

bb.default <- function(text) {                    # for text, vectors, matrices
  if (is.numeric(unlist(text))) {
    stop(bb("non-character objects cannot be turned into bb-objects!"))
  }
  bb_text <- new_bb(text)
  structure(bb_text, class = c("bb", class(text)))
}

bb.list <- function(text) {                       # for lists
  bb_text <- rapply(text, bb, how = "list")
  structure(bb_text, class = c("bb", class(text)))
}

bb.factor <- function(text) {                     # for factors
  levels <- bb(levels(text))
  text_char <- as.character(text)
  bb_text <- new_bb(text_char)
  bb_factor <- factor(bb_text, levels = levels, 
                      labels = levels, ordered = is.ordered(text))
  structure(bb_factor, class = c("bb", class(bb_factor)))
}

