
read_chords <- function(data) {
  data$url %>%
    map(get_chords) %>%
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
      transposed_chord_index <- chord_index + offset
      
      # cálculo do index do acorde
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
