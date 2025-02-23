
##############################################################################
# HW 5
# Avery Johnson
##############################################################################

##############################################################################
# Code Task: Compile Data from Essentia
##############################################################################

##############################################################################
# Step 0: install the stringr and jsonlite packages for R
##############################################################################

library("stringr")
library("jsonlite")

##############################################################################
# Step 2: complete step 1 for all .JSON files in the EssentiaOutput Folder
##############################################################################

json_files <- list.files(paste("EssentiaOutput", sep="/"),pattern="\\.json$") # Find all JSON files

#initialize an empty data frame to store the results
df_results <- data.frame(artist = rep(x = NA, times=length(json_files)),
                         album = rep(x = NA, times=length(json_files)),
                         track = rep(x = NA, times=length(json_files)),
                         overall_loudness = rep(x = NA, times=length(json_files)),
                         spectral_energy = rep(x = NA, times=length(json_files)),
                         dissonance = rep(x = NA, times=length(json_files)),
                         pitch_salience = rep(x = NA, times=length(json_files)),
                         bpm = rep(x = NA, times=length(json_files)),
                         beats_loudness = rep(x = NA, times=length(json_files)),
                         danceability = rep(x = NA, times=length(json_files)),
                         tuning_frequency = rep(x = NA, times=length(json_files)))

for (i in 1:length(json_files)) {
  json_file <- json_files[i]
  file_parts <- str_split(json_file, "-", simplify=T)
  artist <- file_parts[1]
  album <- file_parts[2]
  track <- file_parts [3]
  track <- str_sub (track, start=1, end=str_length(track)-5) # Remove .json
  
  # load JSON data
  json_data <- fromJSON(paste("EssentiaOutput", json_file, sep="/"))
  
  #Step 3: extract the required features
  overall.loudness <- json_data$lowlevel$loudness_ebu128$integrated
  spectral_energy <- json_data$lowlevel$spectral_energy
  dissonance <- json_data$lowlevel$dissonance
  pitch_salience <- json_data$lowlevel$pitch_salience
  bpm <- json_data$rhythm$bpm
  beats_loudness <- json_data$rhythm$beats_loudness
  danceability <- json_data$rhythm$danceability
  tuning_frequency <- json_data$tonal$tuning_frequency
  
  df_results[i, ] <- c(artist, 
                       album, 
                       track,
                       overall_loudness,
                       spectral_energy,
                       dissonance,
                       pitch_salience,
                       bpm,
                       beats_loudness,
                       danceability,
                       tuning_frequency)
}
View(df_results)

##############################################################################
# Step 3: Load and clean the data from the Essentia models by completing the 
# following steps
##############################################################################

#substep 1
essentia_model <- read.csv(paste("EssentiaOutput", "EssentiaModelOutput.csv", sep="/"))

#substep 2
essentia_model$valence <- rowMeans(essentia_model[,c("deam_valence", "emo_valence", "muse_valence")])
essentia_model$arousal <- rowMeans(essentia_model[ , c("deam_arousal", "emo_arousal", "muse_arousal")])

#substep 3
essentia_model$aggressive <- rowMeans(essentia_model[,c("eff_aggressive", "nn_aggressive")])
essentia_model$happy <- rowMeans(essentia_model[,c("eff_happy", "nn_happy")])
essentia_model$party <- rowMeans(essentia_model[,c("eff_party", "nn_party")])
essentia_model$relax <- rowMeans(essentia_model[,c("eff_relax", "nn_relax")])
essentia_model$sad <- rowMeans(essentia_model[,c("eff_sad", "nn_sad")])

#substep 4
essentia_model$acoustic <- rowMeans(essentia_model[,c("eff_acoustic", "nn_acoustic")])
essentia_model$electronic <- rowMeans(essentia_model[,c("eff_electronic", "nn_electronic")])

#substep 5
essentia_model$instrumental <- rowMeans(essentia_model[,c("eff_instrumental", "nn_instrumental")])

#substep 6
colnames(essentia_model)[colnames(essentia_model) == "eff_timbre_bright"] <- "timbreBright" #renametherow

#substep 7
cleaned_essentia <- essentia_model[ , c("artist", "album", "track", "valence", 
                                        "arousal","aggressive", "happy", "party",
                                        "relax", "sad", "acoustic", "electronic",
                                        "instrumental", "timbreBright")]
View(cleaned_essentia)
dim(cleaned_essentia)

##############################################################################
# Step 4: Load the data from LIWC and compile the full dataset
##############################################################################

#substep 1
liwc_output <- read.csv(paste("LIWCOutput", "LIWCOutput.csv", sep="/"))

# Merge df_results and cleaned_essentia
merged_df1 <- merge(df_results, cleaned_essentia)
dim(merged_df1)

# Now merge merged_df1 with liwc_output
merged_df <- merge(merged_df1, liwc_output)
dim(merged_df)

# View the merged data frame
View(merged_df)

#substep 3
colnames(merged_df)[colnames(merged_df) == "function."] <- "funct"