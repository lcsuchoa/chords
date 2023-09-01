
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



transpose_capo <- function(data) {
  
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

# função com rowwise
transpose_chords <- function(data, to_key = "C") {
  
  notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  minor <- c("Am", "A#m", "Bm", "Cm", "C#m", "Dm", "D#m", "Em", "Fm", "F#m", "Gm", "G#m")
  major_scale <- c("C", "Dm", "Em", "F", "G", "Am", "Bm")
  
  aux <- data %>%
    rowwise() %>%
    mutate(
      chord1 = case_when(
        grepl("/", chord) & !grepl("[0-9]M?-?/[0-9]", chord) ~ str_split(chord, "/")[[1]][1],
        TRUE ~ chord
      ),
      chord2 = case_when(
        grepl("/", chord) & !grepl("[0-9]M?-?/[0-9]", chord) ~ strsplit(chord, "/")[[1]][2],
        TRUE ~ NA
      ),
      main1 = case_when(
        grepl("#", chord1) ~ substr(chord1, 1, 2),
        !grepl("#", chord1) ~ substr(chord1, 1, 1)
      ),
      main2 = case_when(
        is.na(chord2) ~ NA,
        grepl("#", chord2) ~ substr(chord2, 1, 2),
        !grepl("#", chord2) ~ substr(chord2, 1, 1)        
      ),
      rest1 = case_when(
        grepl("#", chord1) ~ substr(chord1, 3, nchar(chord1)),
        !grepl("#", chord1) ~ substr(chord1, 2, nchar(chord1))
      ),
      rest2 = case_when(
        is.na(chord2) ~ NA,
        grepl("#", chord2) ~ substr(chord2, 3, nchar(chord2)),
        !grepl("#", chord2) ~ substr(chord2, 2, nchar(chord2))
      ),
      from_index = case_when(
        key %in% notes ~ match(key, notes),
        key %in% minor ~ match(key, minor)
      ),
      to_index = match(to_key, notes),
      offset = to_index - from_index,
      chord_index1 = match(main1, notes),
      chord_index2 = match(main2, notes),
      transposed_index1 = case_when(
        chord_index1 + offset > 0 ~ chord_index1 + offset,
        TRUE ~ chord_index1 + offset + length(notes)
      ),
      transposed_index2 = case_when(
        chord_index2 + offset > 0 ~ chord_index2 + offset,
        TRUE ~ chord_index2 + offset + length(notes)
      ),
      transposed = case_when(
        is.na(chord2) ~ paste0(notes[transposed_index1], rest1),
        TRUE ~ paste0(notes[transposed_index1], rest1, "/", notes[transposed_index2], rest2)
      )
    )
  
  data %>%
    mutate(
      chordC = aux$transposed,
      keyC = ifelse(grepl("m", key), "Am", "C")
    )
}



# --------------------------
# tentativa para tirar rowwise e deixar mais eficiente
# ainda não está funcionando (erro com acordes tipo G/B)
# suspeita do problema ser no case-when

transpose_chords_temp <- function(data, to_key = "C") {
  
  notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  minor <- c("Am", "A#m", "Bm", "Cm", "C#m", "Dm", "D#m", "Em", "Fm", "F#m", "Gm", "G#m")
  major_scale <- c("C", "Dm", "Em", "F", "G", "Am", "Bm")
  
  aux <- data %>%
    mutate(
      chord1 = case_when(
        str_detect(chord, "/") & !str_detect(chord, "[0-9]M?-?/[0-9]") ~ str_split(chord, "/")[[1]][1],
        TRUE ~ chord
      ),
      chord2 = case_when(
        str_detect(chord, "/") & !str_detect(chord, "[0-9]M?-?/[0-9]") ~ strsplit(chord, "/")[[1]][2],
        TRUE ~ NA
      ),
      main1 = case_when(
        str_detect(chord1, "#") ~ substr(chord1, 1, 2),
        !str_detect(chord1, "#") ~ substr(chord1, 1, 1)
      ),
      main2 = case_when(
        is.na(chord2) ~ NA,
        str_detect(chord2, "#") ~ substr(chord2, 1, 2),
        !str_detect(chord2, "#") ~ substr(chord2, 1, 1)        
      ),
      rest1 = case_when(
        str_detect(chord1, "#") ~ substr(chord1, 3, nchar(chord1)),
        !str_detect(chord1, "#") ~ substr(chord1, 2, nchar(chord1))
      ),
      rest2 = case_when(
        is.na(chord2) ~ NA,
        str_detect(chord2, "#") ~ substr(chord2, 3, nchar(chord2)),
        !str_detect(chord2, "#") ~ substr(chord2, 2, nchar(chord2))
      ),
      from_index = case_when(
        key %in% notes ~ match(key, notes),
        key %in% minor ~ match(key, minor)
      ),
      to_index = match(to_key, notes),
      offset = to_index - from_index,
      chord_index1 = match(main1, notes),
      chord_index2 = match(main2, notes),
      transposed_index1 = case_when(
        chord_index1 + offset > 0 ~ chord_index1 + offset,
        TRUE ~ chord_index1 + offset + length(notes)
      ),
      transposed_index2 = case_when(
        chord_index2 + offset > 0 ~ chord_index2 + offset,
        TRUE ~ chord_index2 + offset + length(notes)
      ),
      transposed = case_when(
        is.na(chord2) ~ paste0(notes[transposed_index1], rest1),
        TRUE ~ paste0(notes[transposed_index1], rest1, "/", notes[transposed_index2], rest2)
      )
    )
  
  data %>%
    mutate(
      chordC = aux$transposed,
      keyC = ifelse(str_detect(key, "m"), "Am", "C")
    )
}