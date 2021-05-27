LETTERS <- c(
  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
  "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
)

# Function to split a message for each letter in it
split_message <- function(message){
  message <- stringr::str_to_upper(message)
  message <-   stringr::str_split(message, "", simplify = TRUE)
  return(message)
}

# Function performs Caesar's shift by #n shifts
substitute_idx <- function(key){
  key <- key - 1
  ciphertext <- (1:length(LETTERS)) + key # R got a built in vector with all the letters of the alphabet
  ciphertext <- (ciphertext %% 26) + 1
  return(ciphertext)
}

validate_arguments <- function(message, nr_of_shifts, encryption){
  # stop when..
  if(!is.character(message)){
    stop("message should be a string")
  }
  # stop when..
  if(!is.numeric(nr_of_shifts)){
    stop("`nr_of_shits` should be an integer")
  }
  stopifnot(encryption == "encode" | encryption == "decode")
}


#' Function employs Caesar's cipher
#' @param message string
#'
#' @param nr_of_shifts int
#' @param encryption Either "encode" for encryption,
#'  or "decode" for decryption.
#'
#' @export
#' @examples
#' # Encode your message
#' caesars_cipher("theeaglehaslanden", 4, encryption = "encode")
#'
#' # Decode your received message
#' caesars_cipher("XLIIEKPILEWPERHIR", 4, encryption = "decode")
caesars_cipher <- function(message, nr_of_shifts, encryption){

  # stop when..
  validate_arguments(message, nr_of_shifts, encryption)

  # Create dictionary for encryption;swapping letters
  if(encryption == "encode"){
    Alpha_dict <- LETTERS[substitute_idx(nr_of_shifts)]
    names(Alpha_dict) <- LETTERS
  }

  # Create dictionary for decryption;swapping letters
  if(encryption == "decode"){
    Alpha_dict <- LETTERS
    names(Alpha_dict) <- LETTERS[substitute_idx(nr_of_shifts)]
  }

  # Perform caesar's shift
  result <- Alpha_dict[split_message(stringr::str_to_upper(message))]
  result <- unname(result)
  result <- stringr::str_c(result, collapse = "")
  return(result)
}
