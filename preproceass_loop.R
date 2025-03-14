# Install necessary packages if missing
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tuneR, seewave, phonTools, whereami)

# --- Paths ---
# Get main directory from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Please provide the main directory path as the first command-line argument.")
}
main_dir <- normalizePath(args[1])

# Find all .wav files in subdirectories
wav_files <- list.files(main_dir, pattern = "_audio.wav$", recursive = TRUE, full.names = TRUE)
if (length(wav_files) == 0) {
  stop("No .wav files found in the specified directory.")
}

# Create log file
log_file <- file.path(main_dir, "processing_log.txt")
log_con <- file(log_file, open = "wt")
writeLines("Processing Log", log_con)

# Process each audio file
for (audio_file in wav_files) {
  tryCatch({
    # Define corresponding events.tsv file
    events_file <- sub("_audio.wav", "_events.tsv", audio_file)
    if (!file.exists(events_file)) {
      stop(paste("Error: Events file not found for", audio_file))
    }

    # Define output directory
    output_dir <- dirname(audio_file)
    audio_output_dir <- file.path(output_dir, "audio")
    dir.create(audio_output_dir, recursive = TRUE, showWarnings = FALSE)

    # Load Audio
    wave <- readWave(audio_file)
    sampling_rate <- wave@samp.rate

    # Load Events File
    events_df <- read.table(events_file, sep = "\t", header = TRUE)

    # Debugging print
    print(paste("Processing", audio_file))

    # --- Energy Calculation (Existing Logic) ---
    ptrack <- phonTools::powertrack(wave@left, timestep = 5, windowlength = 10, fs = sampling_rate, show = FALSE)
    ptrack_linear <- 10^(ptrack$power / 10)
    ptrack_normalized <- ptrack_linear / max(ptrack_linear)
    ptrack_percentage <- ptrack_normalized * 100

    threshold_percentage <- 20
    min_low_energy_duration <- 1
    max_low_energy_duration <- 3
    min_high_energy_duration <- 1.5
    max_high_energy_duration <- 2.5
    post_high_energy_duration <- 2.255
    num_segments_to_ignore <- 8

    high_energy_segments <- rle(ptrack_percentage > threshold_percentage)
    low_energy_segments <- list()

    in_high_energy_segment <- FALSE
    current_high_energy_start <- 0
    segment_counter <- 0

    for (i in seq_along(high_energy_segments$lengths)) {
      if (high_energy_segments$values[i]) {
        if (!in_high_energy_segment) {
          in_high_energy_segment <- TRUE
          current_high_energy_start <- sum(high_energy_segments$lengths[1:i]) - high_energy_segments$lengths[i] + 1
        }
      } else {
        if (in_high_energy_segment) {
          high_energy_duration <- (ptrack$time[sum(high_energy_segments$lengths[1:i])] - ptrack$time[current_high_energy_start]) / 1000

          if (high_energy_duration >= min_high_energy_duration && high_energy_duration <= max_high_energy_duration) {
            low_energy_segments[[length(low_energy_segments) + 1]] <- c(start = ptrack$time[current_high_energy_start],
                                                                        end = ptrack$time[sum(high_energy_segments$lengths[1:i])])
          }
          in_high_energy_segment <- FALSE
        }
      }
    }

    if (length(low_energy_segments) == 0) {
      stop("Error: No low-energy segments detected.")
    }

    # --- Process Segments ---
    output_df <- data.frame(onset = numeric(), duration = numeric(), trial_type = character(), response_file = character(), stringsAsFactors = FALSE)

    for (i in seq_along(low_energy_segments)) {
      segment_filename <- file.path(audio_output_dir, paste0(i, "_segment.wav"))

      start_sample <- round((low_energy_segments[[i]]["start"] + 200) / 1000 * sampling_rate)
      end_sample <- round((low_energy_segments[[i]]["end"] - 50) / 1000 * sampling_rate)
      segment_wave <- extractWave(wave, from = start_sample, to = end_sample, xunit = "samples")

      writeWave(segment_wave, filename = segment_filename, extensible = FALSE)
      output_df <- rbind(output_df, data.frame(onset = low_energy_segments[[i]]["start"],
                                               duration = low_energy_segments[[i]]["end"] - low_energy_segments[[i]]["start"],
                                               trial_type = "segment",
                                               response_file = segment_filename))
    }

    # Write Results
    write.table(output_df, file.path(output_dir, sub("_audio.wav", "_events.tsv", basename(audio_file))), row.names = FALSE, sep = "\t", quote = FALSE)

    print(paste("Processing complete for", audio_file))

  }, error = function(e) {
    writeLines(paste("Failed to process", audio_file, "-", e$message), log_con)
    print(paste("Error processing", audio_file, ":", e$message))
  })
}

close(log_con)
print("Batch processing complete. Check the log file for errors.")
