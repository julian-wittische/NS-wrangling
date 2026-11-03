source("config.R")
source("Libraries.R")

############ Read Word document ----
# List Word .docXXX documents
files <- list.files(DATAPATH, pattern = "\\.docx?$", full.names = TRUE)

doc <- read_docx(files[[1]])

paragraphs <- docx_summary(doc) %>%
  filter(content_type == "paragraph") %>%
  mutate(text = str_squish(text)) %>%
  filter(!is.na(text), text != "") %>%
  pull(text)


############ Extract species name ----
# First two words of the first paragraph
species <- str_extract(
  paragraphs[1],
  "^[[:alpha:]-]+\\s+[[:alpha:]-]+"
)

if (is.na(species)) {
  stop("Could not identify species in the first paragraph.")
}


############ Define date extraction ----
# Supports individual years and year ranges
year_pattern <- paste0(
  "\\b[12][0-9]{3}",
  "(?:\\s*(?:à|a|[-–])\\s*[12][0-9]{3})?\\b"
)

extract_years <- function(x) {
  
  dates <- str_extract_all(x, year_pattern)[[1]]
  
  years <- unlist(lapply(dates, function(date) {
    
    # Detect year ranges
    if (str_detect(date, "\\s*(à|a|[-–])\\s*")) {
      
      limits <- as.integer(
        str_extract_all(date, "[12][0-9]{3}")[[1]]
      )
      
      seq(limits[1], limits[2])
      
    } else {
      as.integer(date)
    }
    
  }))
  
  as.character(years)
}


############ Parse a single locality entry ----

parse_entry <- function(entry, region, species, form,
                        record_type = "Observation") {
  
  entry <- str_trim(entry)
  
  if (entry == "") return(tibble())
  
  # Remove certain editorial annotations
  entry_clean <- str_remove_all(
    entry,
    regex("\\([^)]*y compris[^)]*\\)", ignore_case = TRUE)
  )
  
  # Find the first date
  first_date <- str_locate(entry_clean, year_pattern)
  
  if (all(is.na(first_date))) return(tibble())
  
  location <- str_sub(
    entry_clean, 1, first_date[1] - 1
  ) %>%
    str_remove("[,;:\\s]+$") %>%
    str_trim()
  
  location_info <- if (!is.na(region)) {
    paste(region, location, sep = " : ")
  } else {
    location
  }
  
  ###### Bibliography records
  
  if (record_type == "Bibliographic") {
    
    year <- extract_years(entry_clean)[1]
    
    recorder <- str_match(
      entry_clean, "\\(([^()]*)\\)"
    )[, 2]
    
    if (!is.na(recorder)) {
      recorder <- str_remove_all(recorder, year_pattern) %>%
        str_remove_all("[,;()]") %>%
        str_trim()
    }
    
    return(tibble(
      Species = species,
      Form = form,
      Year = year,
      Location_information = location_info,
      Recorder = recorder,
      Record_type = record_type,
      Original_entry = entry
    ))
  }
  
 ###### Standard observation records
  
  positions <- str_locate_all(
    entry_clean, "\\([^()]*\\)"
  )[[1]]
  
  results <- list()
  
  # If there is no parenthetical recorder
  if (nrow(positions) == 0) {
    
    years <- extract_years(entry_clean)
    
    if (length(years) == 0) return(tibble())
    
    return(tibble(
      Species = species,
      Form = form,
      Year = years,
      Location_information = location_info,
      Recorder = NA_character_,
      Record_type = record_type,
      Original_entry = entry
    ))
  }
  
  previous_end <- 0
  
  for (i in seq_len(nrow(positions))) {
    
    # Text between the previous recorder and current recorder
    preceding_text <- str_sub(
      entry_clean,
      previous_end + 1,
      positions[i, 1] - 1
    )
    
    years <- extract_years(preceding_text)
    
    recorder <- str_sub(
      entry_clean,
      positions[i, 1] + 1,
      positions[i, 2] - 1
    ) %>%
      str_remove_all(year_pattern) %>%
      str_trim()
    
    # Skip editorial annotations
    if (str_detect(
      recorder,
      regex("y compris", ignore_case = TRUE)
    )) {
      previous_end <- positions[i, 2]
      next
    }
    
    if (length(years) > 0) {
      
      results[[length(results) + 1]] <- tibble(
        Species = species,
        Form = form,
        Year = years,
        Location_information = location_info,
        Recorder = recorder,
        Record_type = record_type,
        Original_entry = entry
      )
    }
    
    previous_end <- positions[i, 2]
  }
  
  bind_rows(results)
}


############ Parse locality paragraphs ----

parse_paragraph <- function(txt, species, form) {
  
  is_bibliographic <- str_detect(
    txt,
    regex("^Donnée[s]? bibliographique\\s*:",
          ignore_case = TRUE)
  )
  
  # Remove bibliographic label
  txt <- str_remove(
    txt,
    regex("^Donnée[s]? bibliographique\\s*:\\s*",
          ignore_case = TRUE)
  )
  
  # Split into locality entries
  entries <- str_split(txt, "\\s*;\\s*")[[1]]
  
  region <- NA_character_
  
  results <- list()
  
  for (entry in entries) {
    
    entry <- str_trim(entry)
    
    # Update region when encountered
    region_match <- str_match(
      entry,
      "^(Ard\\.|Lorr\\.)\\s*:"
    )
    
    if (!is.na(region_match[1, 2])) {
      
      region <- region_match[1, 2]
      
      entry <- str_remove(
        entry,
        "^(Ard\\.|Lorr\\.)\\s*:\\s*"
      )
    }
    
    if (entry == "") next
    
    results[[length(results) + 1]] <- parse_entry(
      entry = entry,
      region = region,
      species = species,
      form = form,
      record_type = ifelse(
        is_bibliographic,
        "Bibliographic",
        "Observation"
      )
    )
  }
  
  bind_rows(results)
}


############ Identify generation and extract records ----

current_form <- NA_character_

all_records <- list()

for (i in seq_along(paragraphs)) {
  
  txt <- paragraphs[i]
  
  # Identify sexual generation
  if (str_detect(txt, "♀\\s*♂")) {
    current_form <- "Sexual"
  }
  
  # Identify asexual generation
  if (str_detect(txt, "♀♀")) {
    current_form <- "Asexual"
  }
  
  # Only process locality or bibliographic paragraphs
  is_locality <- str_detect(
    txt,
    regex(
      "^(Ard\\.|Lorr\\.|Donnée[s]? bibliographique\\s*:)",
      ignore_case = TRUE
    )
  )
  
  if (is_locality && !is.na(current_form)) {
    
    all_records[[length(all_records) + 1]] <-
      parse_paragraph(
        txt = txt,
        species = species,
        form = current_form
      )
  }
}


############ Create final dataframe ----

records <- bind_rows(all_records) %>%
  mutate(
    Species = species,
    Year = as.character(Year),
    Region = str_extract(
      Location_information,
      "^(Ard\\.|Lorr\\.)"
    ),
    Location_information = str_remove(
      Location_information,
      "^(Ard\\.|Lorr\\.)\\s*:\\s*"
    )
  ) %>%
  select(
    Species,
    Form,
    Year,
    Region,
    Location_information,
    Recorder,
    Record_type,
    Original_entry
  )

# Inspect
print(records, n = 100)


############ Export ----

write_excel_csv( # Using this instead of write.csv because it writes with a BOM
  records,
  paste0(DATAPATH, "/RESULTS/",sub(" ", "_", species),"_extracted.csv"))
