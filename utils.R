transpose_chords <- function(chords, from_key, to_key) {
  # Vetor com as notas musicais
  notes <- c("C", "C#", "Db", "D", "D#", "Eb", "E", "F", "F#", "Gb", "G", "G#", "Ab", "A", "A#", "Bb", "B")
  
  # Índices das tonalidades de origem e destino
  from_index <- match(from_key, notes)
  to_index <- match(to_key, notes)
  
  # Verificar se as tonalidades foram encontradas
  if (is.na(from_index) || is.na(to_index)) {
    stop("Tonalidade não encontrada.")
  }
  
  # Calcular o deslocamento necessário para transpor os acordes
  offset <- to_index - from_index
  
  # Transpor cada acorde no vetor de acordes
  transposed_chords <- lapply(chords, function(chord) {
    if (chord != "") {
      # Identificar a nota do acorde
      chord_note <- substring(chord, 1, 1)
      
      # Verificar se a nota está presente no vetor de notas
      note_index <- match(chord_note, notes)
      
      # Transpor a nota do acorde
      transposed_note_index <- note_index + offset
      
      # Lidar com casos de transposição que ultrapassam os limites das notas
      if (transposed_note_index > length(notes)) {
        transposed_note_index <- transposed_note_index - length(notes)
      } else if (transposed_note_index < 1) {
        transposed_note_index <- transposed_note_index + length(notes)
      }
      
      # Construir o novo acorde transposto
      transposed_chord <- paste0(notes[transposed_note_index], substring(chord, 2))
      
      return(transposed_chord)
    } else {
      return(chord)
    }
  })
  
  return(transposed_chords)
}