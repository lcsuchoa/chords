
transpose_chords <- function(data, to_key) {
  
  notes <- c("C", "C#", "Db", "D", "D#", "Eb", "E", "F", "F#", "Gb",
             "G", "G#", "Ab", "A", "A#", "Bb", "B")
  minor <- c("Cm", "C#m", "Dbm", "Dm", "D#m", "Ebm", "Em", "Fm", "F#m",
             "Gbm", "Gm", "G#m", "Abm", "Am", "A#m", "Bbm", "Bm")
  transposed_chord <- character(length(data$key))

  # tons maiores
  for (i in 1:length(data$key)) {
    if (data$key[i] %in% notes) {
      
      chord <- gsub("^([A-G]#?).*", "\\1", data$chord[i])
      if ("#" %in% data$chord[i]) {
        sobra <- substr(data$chord[i], 3, nchar(data$chord))
      } else {
        sobra <- substr(data$chord[i], 2, nchar(data$chord))
      }
      
      from_index <- match(data$key[i], notes)
      to_index <- match(to_key, notes)
      offset <- to_index - from_index
      chord_index <- match(chord, notes)
      transposed_chord_index <- chord_index + offset
      
      if (transposed_chord_index <= 0) {
        transposed_chord_index <- transposed_chord_index + length(notes)
      }
      
      transposed_chord[i] <- paste0(notes[transposed_chord_index], sobra)
    }
  }
  return(transposed_chord)
}
