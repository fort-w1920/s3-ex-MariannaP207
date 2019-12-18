
Sys.setenv(LANGUAGE = "en")



new_bb <- function(text) {                                      # constructor
                                                                # here all assertions
  # replace ((consonants) (1-2 vowels) (consonants)) with        
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  structure(bb_text, class = "bb")
}


bb <- function(text) {                            # generic
  UseMethod("bb", text)
}

bb.default <- function(text) {                    # for text, vectors, matrices, factors
  new_bb(text)               
}

bb.list <- function(text) {                       # for lists
  lapply(test_list, new_bb)
}



str(bb(texttest))
str(bb(test_vec))
str(bb(test_matrix))
str(bb(test_list))            # no class list

unlist(test_listoflists)
test_listoflists
str(bb(test_listoflists))


str(bb(test_factor))          # no class factor
str(bb(test_ordered))
class(test_ordered)
