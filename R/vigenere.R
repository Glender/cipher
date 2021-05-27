# Create a Vigenere Square
Vigenere_square <- matrix(data = NA, nrow = 26, ncol = 26)

# Loop over the square and fill each row with a shifted version of the alphabet
for(i in 1:26){
  Vigenere_square[i,] <- unname(LETTERS[substitute_idx(i)])
}

# Give column names to Vigenere Square
colnames(Vigenere_square) <- stringr::str_to_lower(LETTERS)
rownames(Vigenere_square) <- stringr::str_to_lower(Vigenere_square[,1])

#' function to encode plaintext
#' @importFrom magrittr %>%
encode_plaintext <- function(plaintext, table_encode){

  # Loop over the table and get the ciphertext
  empty_con <- vector(mode = "double", length = ncol(table_encode))
  for(i in 1:ncol(table_encode)){
    empty_con[i] <- Vigenere_square[table_encode[1,i],][table_encode[2,i]] %>%
      unname()}

  # Concatenate the results to vector of length one
  ciphertext <- empty_con %>%
    stringr::str_c(collapse = "")

  return(ciphertext)
}

#' function to decode the plaintext
#' @importFrom magrittr %>%
decode_plaintext <- function(plaintext, table_encode){

  # Store the results
  empty_con <- vector(mode = "double", length = ncol(table_encode))
  for(i in 1:ncol(table_encode)){
    x <- Vigenere_square[table_encode[1,i],]
    empty_con[i] <- x[x==stringr::str_to_upper(table_encode[2,i])] %>%
      names()}

  # Concatenate the results
  ciphertext <- empty_con %>%
    stringr::str_c(collapse = "")

  return(ciphertext)
}

validate_vigenere <- function(plaintext, keyword, encryption){
  # stop when..
  if(!is.character(plaintext)){
    stop("plaintext should be a string")
  }
  if(!is.character(keyword)){
    stop("plaintext should be a string")
  }
  stopifnot(encryption == "encode" | encryption == "decode")
}

#' Use the Vigenere cipher.
#'
#' @param plaintext A string. Your message to encode/decode.
#' @param keyword string. The secret keyword.
#' @param encryption Either "encode" "or decode".
#'
#' @importFrom magrittr %>%
#' @export
#' @examples
#'#' # Encode your message
#' vigenere_cipher(plaintext = "diverttroopstoeastridge", keyword = "WHITE", encryption = "encode")
#' # Decode your message
#' vigenere_cipher(plaintext = "ZPDXVPAZHSLZBHIWZBKMZNM", keyword = "WHITE", encryption = "decode")
vigenere_cipher <- function(plaintext, keyword, encryption = "encode"){

  validate_vigenere(plaintext, keyword, encryption)

  # Make cipher table including the keyword, the plaintext and the ciphertext
  key_index <- rep(1:nchar(keyword), length.out = nchar(plaintext))
  keyword_names <- stringr::str_to_lower(split_message(keyword)[key_index])

  # Split each letter of the plaintext
  splitted_plaintext <- stringr::str_to_lower(split_message(plaintext))
  table_encode <- rbind(keyword_names, splitted_plaintext)

  # Return result of the main function
  if(encryption == "encode") return(encode_plaintext(plaintext, table_encode))
  if(encryption == "decode") return(decode_plaintext(plaintext, table_encode))
}

#' Produce a key for encryption purposes
#' @param length_keyword int
#'
#' @return numeric
#' @export
#'
#' @examples
#'# Generate a random key to make Le Chiffre Indechiffrable
#'key <- random_key_gen(nchar("THEEAGLEHASLANDED"))
random_key_gen <- function(length_keyword){
  out <- vector("double", length_keyword)
  for(i in 1:length_keyword){
    out[i] <- sample(LETTERS, size = 1, replace = T)
  }
  return(stringr::str_c(out, collapse = ""))
}
