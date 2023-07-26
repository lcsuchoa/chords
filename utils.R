
our_get_chords <- function (song_url, nf = FALSE) {
  extract <- function(url) {
    x <- xml2::read_html(paste0("https://www.cifraclub.com.br", url))
    key <- rvest::html_node(x, "#cifra_tom a") %>% rvest::html_text()
    casa <- rvest::html_node(x, "#cifra_capo") %>% rvest::html_text()
    chords <- rvest::html_nodes(x, "pre b") %>% rvest::html_text()
    url <- gsub("-", " ", gsub("^/|/$", "", url))
    if (length(chords)) {
      result <- data.frame(chord = chords, key = key, capo = gsub("[^0-9]", "", casa), song = url, 
                           stringsAsFactors = FALSE)
    }
    else if (nf == TRUE) {
      result <- data.frame(chord = "Not Found", key = "Not Found",
                           capo = "Not Found", song = url, stringsAsFactors = FALSE)
    }
    result
  }
  if (is.data.frame(song_url) & "url" %in% names(song_url)) {
    artist <- unique(song_url$artist)[1]
    song_url <- song_url$url
  }
  saf <- purrr::safely(extract, otherwise = NULL)
  suppressWarnings(df <- song_url %>% purrr::map(saf) %>% purrr::map("result") %>% 
                     purrr::map_dfr(data.frame))
  if (nrow(df) == 0) {
    df <- data.frame(chord = "Not Found", key = "Not Found",
                     capo = "Not Found", song = "Not Found")
    warning("These was an error with the data collection and the chords could not be found.")
    print(dplyr::as_tibble(df))
    return(dplyr::as_tibble(df))
  }
  parsed_names <- strsplit(df$song, "/")
  df <- df %>% dplyr::mutate(artist = sapply(parsed_names, "[", 1),
                             song = sapply(parsed_names, "[", 2)) %>%
    dplyr::mutate_at(c("artist", "song"), list(~stringr::str_to_title(.)))
  return(dplyr::as_tibble(df))
}



read_chords <- function(data) {
  data$url %>%
    map(our_get_chords) %>%
    map_dfr(mutate_if, is.factor, as.character) %>%
    clean(message = F) %>%
    mutate(chord = case_when(
      grepl("Db", chord) ~ gsub("Db", "C#", chord),
      grepl("Eb", chord) ~ gsub("Eb", "D#", chord),
      grepl("Gb", chord) ~ gsub("Gb", "F#", chord),
      grepl("Ab", chord) ~ gsub("Ab", "G#", chord),
      grepl("Bb", chord) ~ gsub("Bb", "A#", chord),
      grepl("Cb", chord) ~ gsub("Cb", "B", chord),
      grepl("Fb", chord) ~ gsub("Fb", "E", chord),
      grepl("E#", chord) ~ gsub("E#", "F", chord),
      grepl("B#", chord) ~ gsub("B#", "C", chord),
      TRUE ~ chord
    )) %>%
    mutate(key = case_when(
      grepl("Db", key) ~ gsub("Db", "C#", key),
      grepl("Eb", key) ~ gsub("Eb", "D#", key),
      grepl("Gb", key) ~ gsub("Gb", "F#", key),
      grepl("Ab", key) ~ gsub("Ab", "G#", key),
      grepl("Bb", key) ~ gsub("Bb", "A#", key),
      grepl("Cb", key) ~ gsub("Cb", "B", key),
      grepl("Fb", key) ~ gsub("Fb", "E", key),
      grepl("E#", key) ~ gsub("E#", "F", key),
      grepl("B#", key) ~ gsub("B#", "C", key),
      TRUE ~ key
    ))
}



transpose_key <- function(data) {
  
  # definição das variáveis de escalas musicais
  notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  minor <- c("Am", "A#m", "Bm", "Cm", "C#m", "Dm", "D#m", "Em", "Fm", "F#m", "Gm", "G#m")
  
  aux <- data %>%
    mutate(
      main = case_when(
        capo == "" ~ NA,
        key %in% notes & grepl("#", key) ~ substr(key, 1, 2),
        key %in% notes & !grepl("#", key) ~ substr(key, 1, 1),
        key %in% minor & grepl("#", key) ~ substr(key, 1, 3),
        key %in% minor & !grepl("#", key) ~ substr(key, 1, 2)
      ),
      rest = case_when(
        capo == "" ~ NA,
        key %in% notes & grepl("#", key) ~ substr(key, 3, nchar(key)),
        key %in% notes & !grepl("#", key) ~ substr(key, 2, nchar(key)),
        key %in% minor & grepl("#", key) ~ substr(key, 5, nchar(key)),
        key %in% minor & !grepl("#", key) ~ substr(key, 4, nchar(key))
      ),
      offset = as.integer(capo),
      key_index = case_when(
        key %in% notes ~ match(main, notes),
        key %in% minor ~ match(main, minor)
      ),
      transposed_index = case_when(
        key_index - offset > 0 ~ key_index - offset,
        TRUE ~ key_index - offset + length(notes)
      ),
      transposed = case_when(
        capo == "" ~ key,
        key %in% notes ~ paste0(notes[transposed_index], rest),
        key %in% minor ~ paste0(minor[transposed_index], rest)
      )
    )
  
  data %>%
    mutate(key = aux$transposed)
}


## CRIAR COLUNA COM GRAU HARMONICO

## AJUSTAR TRANSPOSE_CHORDS
## CRIAR DUAS COLUNAS PARA ACORDES COM BAIXO
## PESQUISAR SOBRE STRINGR

transpose_chords <- function(data, to_key) {
  
  # definição das variáveis de escalas musicais
  notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  minor <- c("Am", "A#m", "Bm", "Cm", "C#m", "Dm", "D#m", "Em", "Fm", "F#m", "Gm", "G#m")
  transposed_chord <- character(length(data$key))
  
  # loop para cada acorde
  for (i in 1:length(data$key)) {
    
    # acordes com baixo
    if (grepl("/", data$chord[i]) & !grepl("[0-9]M?-?/[0-9]", data$chord[i])) {
      
      # primeira parte do acorde
      chord1 <- strsplit(data$chord[i], "/")[[1]][1]
      
      # tons maiores
      if (data$key[i] %in% notes) {
        if (grepl("#", chord1)) {
          chord <- substr(chord1, 1, 2)
          sobra <- substr(chord1, 3, nchar(chord1))
        } else {
          chord <- substr(chord1, 1, 1)
          sobra <- substr(chord1, 2, nchar(chord1))
        }
        from_index <- match(data$key[i], notes)
      }
      
      # tons menores
      if (data$key[i] %in% minor) {
        if (grepl("#", chord1)) {
          chord <- substr(chord1, 1, 2)
          sobra <- substr(chord1, 3, nchar(chord1))
        } else {
          chord <- substr(chord1, 1, 1)
          sobra <- substr(chord1, 2, nchar(chord1))
        }
        from_index <- match(data$key[i], minor)
      }
      
      # cálculo de semitons
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      
      # cálculo do index do acorde
      transposed_chord_index <- chord_index + offset
      if (!is.na(transposed_chord_index) & transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      # transposição da primeira parte do acorde
      chord1 <- paste0(notes[transposed_chord_index], sobra)
      
      # segunda parte do acorde
      chord2 <- strsplit(data$chord[i], "/")[[1]][2]
      
      # tons maiores
      if (data$key[i] %in% notes) {
        if (grepl("#", chord2)) {
          chord <- substr(chord2, 1, 2)
          sobra <- substr(chord2, 3, nchar(chord2))
        } else {
          chord <- substr(chord2, 1, 1)
          sobra <- substr(chord2, 2, nchar(chord2))
        }
        from_index <- match(data$key[i], notes)
      }
      
      # tons menores
      if (data$key[i] %in% minor) {
        if (grepl("#", chord2)) {
          chord <- substr(chord2, 1, 2)
          sobra <- substr(chord2, 3, nchar(chord2))
        } else {
          chord <- substr(chord2, 1, 1)
          sobra <- substr(chord2, 2, nchar(chord2))
        }
        from_index <- match(data$key[i], minor)
      }
      
      # cálculo de semitons
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      transposed_chord_index <- chord_index + offset
      
      # cálculo do index do acorde
      if (!is.na(transposed_chord_index) & transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      # transposição da primeira parte do acorde
      chord2 <- paste0(notes[transposed_chord_index], sobra)
      
      transposed_chord[i] <- paste0(chord1, "/", chord2)
    }
    
    # acordes sem baixo
    else {
      
      # tons maiores
      if (data$key[i] %in% notes) {
        if (grepl("#", data$chord[i])) {
          chord <- substr(data$chord[i], 1, 2)
          sobra <- substr(data$chord[i], 3, nchar(data$chord[i]))
        } else {
          chord <- substr(data$chord[i], 1, 1)
          sobra <- substr(data$chord[i], 2, nchar(data$chord[i]))
        }
        from_index <- match(data$key[i], notes)
      }
      
      # tons menores
      if (data$key[i] %in% minor) {
        if (grepl("#", data$chord[i])) {
          chord <- substr(data$chord[i], 1, 2)
          sobra <- substr(data$chord[i], 3, nchar(data$chord[i]))
        } else {
          chord <- substr(data$chord[i], 1, 1)
          sobra <- substr(data$chord[i], 2, nchar(data$chord[i]))
        }
        from_index <- match(data$key[i], minor)
      }
      
      # cálculo de semitons
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      transposed_chord_index <- chord_index + offset
      
      # cálculo do index do acorde
      if (!is.na(transposed_chord_index) & transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      # transposição do acorde
      transposed_chord[i] <- paste0(notes[transposed_chord_index], sobra)
    }
    
  }
  return(transposed_chord)
}