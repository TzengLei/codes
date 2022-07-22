enco_dtec <- function(chars, encodings = NULL, get_ob = FALSE){
  require(dplyr)
  require(readr)
  require(stringr)
  encodings <- encodings %||% "ASCII"
  encod <- sapply(chars, function(x){
    guess_encoding(charToRaw(x))
  })
  non <- encod[1, ] %>% 
    str_detect(encodings, negate = T) %>% 
    encod[1, ][.]
  site <- encod[1, ] %>% 
    grep(pattern = encodings, invert = T)
  if (length(site) > 0) {
    non['site'] <- list(site)
    if (get_ob) assign(paste0("non_", encodings), non, envir = .GlobalEnv) else
      return(non)
  } else print("there is no non-matching character.")
}

# Arguments
# chars: A character to detect.
# encodings: A character of common encodings(e.g., "UTF-8", "ASCII").
# get_ob: Logical. If FALSE, the results of non-matching character and their site will printing on screen, if TRUE, 
# return an object contains the results.