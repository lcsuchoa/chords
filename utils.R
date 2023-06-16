read_chords <- function(data) {
  data$url %>%
    map(get_chords) %>%
    map_dfr(mutate_if, is.factor, as.character) %>%
    clean(message = F) %>%
    mutate(chord = case_when( 
      chord == "Bb" ~ "A#",
      chord == "Ab" ~ "G#",
      chord == "Gb" ~ "F#",
      chord == "Eb" ~ "D#",
      chord == "Db" ~ "C#",
      chord == "Fb" ~ "E",
      chord == "Cb" ~ "B",
      chord == "E#" ~ "F",
      chord == "B#" ~ "C",
      TRUE ~ chord
    )
    )
}


transpose_chords <- function(data, to_key) {
  
  notes <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  minor <- c("Am", "A#m", "Bm", "Cm", "C#m", "Dm", "D#m", "Em", "Fm", "F#m", "Gm", "G#m")
  transposed_chord <- character(length(data$key))
  
  for (i in 1:length(data$key)) {
    if (data$key[i] %in% notes) {
      
      if ("#" %in% data$chord[i]) {
        chord <- substr(data$chord[i], 1, 2)
        sobra <- substr(data$chord[i], 3, nchar(data$chord[i]))
      } else {
        chord <- substr(data$chord[i], 1, 1)
        sobra <- substr(data$chord[i], 2, nchar(data$chord[i]))
      }
      
      from_index <- match(data$key[i], notes)
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      transposed_chord_index <- chord_index + offset
      
      if (!is.na(transposed_chord_index) & transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      transposed_chord[i] <- paste0(notes[transposed_chord_index], sobra)
    }
    
    if (data$key[i] %in% minor) {
      
      if ("#" %in% data$chord[i]) {
        chord <- substr(data$chord[i], 1, 2)
        sobra <- substr(data$chord[i], 3, nchar(data$chord[i]))
      } else {
        chord <- substr(data$chord[i], 1, 1)
        sobra <- substr(data$chord[i], 2, nchar(data$chord[i]))
      }
      
      from_index <- match(data$key[i], minor)
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      transposed_chord_index <- chord_index + offset
      
      if (!is.na(transposed_chord_index) & transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      transposed_chord[i] <- paste0(notes[transposed_chord_index], sobra)
    }
  }
  return(transposed_chord)
}
