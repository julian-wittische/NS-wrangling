######################## PROJECT: exracting and wrangling Nico Shneider's oak gall wasp data
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Julian Wittische/Nico Schneider
# Start: Fall 2026
# Data: Nico Schneider
# Script objective : supporting functions

############ Read and organize single "Word" ---- 
doc_ls <- list.files(DATAPATH, pattern="*.docx", full.names = TRUE)

doc <- read_docx(doc_ls[[1]])


doc <- read_docx("Neuroterus_albipes.docx")

content <- docx_summary(doc) %>%
  filter(content_type == "paragraph") %>%
  mutate(text = str_squish(text)) %>%
  filter(!is.na(text), text != "")
