
# Luke Pilling -- lcpilling.github.io
# 2025.07.11

# Data is in JSON format. This may differ by era.
# Compile into tidy tabular format

# install required packages
install.packages(setdiff(c(
	"tidyverse","janitor",
	"jsonlite",
	"tidytext"
	), 
	rownames(installed.packages())))

# find files
fls <- list.files("data/raw", pattern=".json", full.names=TRUE) |> stringr::str_subset("Audio")

# load each, spread, append
dat <- NULL
for (fl in fls)  {
	
	# load and clean names
	d <- jsonlite::fromJSON(fl) |>
		janitor::clean_names() 
	
	# make sure dates etc properly formatted
	d <- d |> dplyr::mutate(
		duration = ms_played/1000,
		ts = lubridate::as_datetime(ts),
		start_time = lubridate::with_tz(ts, tzone = "Europe/London"),
		end_time = start_time+duration,
		date = lubridate::date(start_time),
		year=lubridate::year(date),
		day_week = lubridate::wday(start_time),
		weekend = factor(dplyr::case_when(
			day_week %in% (6:7) ~ "Weekend",
			day_week %in% (1:5) ~ "Weekday",
			TRUE ~ NA_character_
		))
	)
	
	# append
	dat <- rbind(dat, d)
}

# arrange by date
dat <- dat |> 
	tibble::as_tibble() |>
	dplyr::arrange(date)

# split into music, episodes, and audiobooks separately
dat_music <- dat |> dplyr::filter(!is.na(master_metadata_track_name))
dat_audiobook <- dat |> dplyr::filter(!is.na(audiobook_title))
dat_episode <- dat |> dplyr::filter(!is.na(episode_name))

# save
readr::write_tsv(dat_music, "data/derived/music.tsv.gz")
readr::write_tsv(dat_audiobook, "data/derived/audiobook.tsv.gz")
readr::write_tsv(dat_episode, "data/derived/episode.tsv.gz")

