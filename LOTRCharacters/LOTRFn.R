library(dplyr)
library(stringr)
library(tidyr)

readToLinesFn <- function(raw){
  lines <- data_frame(raw=raw) %>% 
    # Filter off non-dialogue, non-character lines
    filter(raw!="", raw!=" ", !str_detect(raw, "SUPER:"), !str_detect(raw,"IMAGE:"),
           !str_detect(raw, "FADE UP:"), !str_detect(raw, "TEASING SHOTS:"),
           !str_detect(raw, "CONTINUED:"), !str_detect(raw, "WIDE ON:"),
           !str_detect(raw, "ANGLE ON:")) %>%
    # Detect where each new scene starts and generate scene number
    mutate(is_scene = (str_detect(raw, "INT\\.") | str_detect(raw, "EXT\\.")),
           scene = cumsum(is_scene))
  lines$is_scene <- NULL
  return(lines)
}

parseToCharactersFn <- function(lines){
  # Regex search to identify characters
  characterScenes <- lines %>% filter(str_detect(raw, "^ +[A-Z][A-Z]+"))
  characterScenes <- characterScenes %>% filter(str_detect(raw, "[A-Z][A-Z]+$"), !str_detect(raw, ":$"))
  characterScenes$raw <- trimws(characterScenes$raw)
  return(characterScenes)
}