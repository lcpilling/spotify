
# Luke Pilling -- lcpilling.github.io
# 2025.07.11

# initial summary of music data


# read
dat_music <- readr::read_tsv("data/derived/music.tsv.gz")
#dat_audiobook <- readr::read_tsv("data/derived/audiobook.tsv.gz")
#dat_episode <- readr::read_tsv("data/derived/episode.tsv.gz")


# tidy ----

# remove the birdsong I used as an alarm for a while
dat_music <- dat_music |> dplyr::filter(master_metadata_album_artist_name != "Nature Sounds")


# play time ----

## time by year ----
dat_time <- dat_music |>
	dplyr::group_by(year) |>
	dplyr::summarise(n=round(sum(duration)/60/60))

# summary
sum(dat_time$n)
sum(dat_time$n) / as.numeric(lubridate::dmy("11-07-2025")-lubridate::dmy("23-03-2012"))


dat_time  |> 
	dplyr::arrange(-n) |>
	ggplot2::ggplot(ggplot2::aes(as.character(year), n, fill=n)) +
	ggplot2::geom_col(ggplot2::aes(y = n), alpha = 0.9) +
	ggplot2::geom_text(ggplot2::aes(y = n, label = n), hjust = -0.2, size = 5, color="black") +
	ggplot2::scale_y_continuous(limits = c(0, 440)) +
	viridis::scale_fill_viridis(option="H") +
	ggplot2::coord_flip() +
	ggplot2::labs(x = "", y = "Hours played", 
								title = "Luke's Spotify music play time", 
								subtitle = "23rd March 2012 to 11th July 2025",
								caption = "Total = 3,245 hours (avg. 0.67 hours per day over the 4,858 days)") +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		axis.text.x = ggplot2::element_blank(),
		legend.position = "none",
		text = ggplot2::element_text(size = 15, color="black"),
		plot.title.position = "plot",
		panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(),		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank()
		)

ggplot2::ggsave("outputs/music_time_byyear.png", width=14, height=14, units="cm", dpi=150, bg="white")



# songs ----

## ever ----
dat_songs <- dat_music |>
	dplyr::count(master_metadata_track_name, master_metadata_album_artist_name) |>
	dplyr::arrange(-n)


dat_songs |>
	head(n=15) |>
	ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(master_metadata_track_name, n), fill=n)) +
	ggplot2::geom_col(ggplot2::aes(y = n), alpha = 0.9) +
	ggplot2::geom_text(ggplot2::aes(y = n, label = n), hjust = -0.2, size = 5, color="black") +
	ggplot2::scale_y_continuous(limits = c(0, 140)) +
	viridis::scale_fill_viridis(option="H") +
	ggplot2::coord_flip() +
	ggplot2::labs(x = "", y = "Number of plays", 
								title = "Luke's top 15 Spotify songs", 
								subtitle = "23rd March 2012 to 11th July 2025") +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		axis.text.x = ggplot2::element_blank(),
		legend.position = "none",
		text = ggplot2::element_text(size = 15, color="black"),
		plot.title.position = "plot",
		panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(),		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank()
	)

ggplot2::ggsave("outputs/songs_top15.png", width=14, height=14, units="cm", dpi=150, bg="white")
	
## by year ----
dat_songs_year <- dat_music |>
	dplyr::group_by(year) |>
	dplyr::count(master_metadata_track_name, master_metadata_album_artist_name) |>
	dplyr::arrange(-n)

# for this I want to shorten some names by removing brackets...
dat_songs_year <- dat_songs_year |>
	dplyr::mutate(
		master_metadata_track_name = stringr::str_replace_all(
			master_metadata_track_name,
			"\\[.*?\\]|\\(.*?\\)", # Regular expression to match text within brackets
			"..."
		)
	)

# plot
dat_songs_year |>
	dplyr::filter(dplyr::row_number() <= 5) |>
	ggplot2::ggplot(ggplot2::aes(tidytext::reorder_within(master_metadata_track_name, n, year), fill=n)) +
	ggplot2::geom_col(ggplot2::aes(y = n), alpha = 0.9) +
	ggplot2::geom_text(ggplot2::aes(y = n, label = n), hjust = -0.2, size = 3, color="black") +
	ggplot2::scale_y_continuous(limits = c(0, 55)) +
	viridis::scale_fill_viridis(option="H") +
	ggplot2::coord_flip() +
	ggplot2::labs(x = "", y = "Number of plays", 
								title = "Luke's top Spotify songs",
								subtitle = "23rd March 2012 to 11th July 2025") +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		axis.text.x = ggplot2::element_blank(),
		legend.position = "none",
		text = ggplot2::element_text(size = 11, color="black"),
		plot.title.position = "plot",
		panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(),		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank()
	) +
	tidytext::scale_x_reordered() +
	ggplot2::facet_grid(factor(year, levels=c(2025:2012)) ~ ., scale="free", space="free")

ggplot2::ggsave("outputs/songs_top5_byyear.png", width=18, height=25, units="cm", dpi=150, bg="white")



# artists ----

## ever ----
dat_artists <- dat_music |>
	dplyr::group_by(master_metadata_album_artist_name) |>
	dplyr::summarise(n=round(sum(duration)/60/60)) |> 
	dplyr::arrange(-n)

dat_artists |>
	head(n=15) |>
	ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(master_metadata_album_artist_name, n), fill=n)) +
	ggplot2::geom_col(ggplot2::aes(y = n), alpha = 0.9) +
	ggplot2::geom_text(ggplot2::aes(y = n, label = n), hjust = -0.2, size = 4, color="black") +
	ggplot2::scale_y_continuous(limits = c(0, 65)) +
	viridis::scale_fill_viridis(option="H") +
	ggplot2::coord_flip() +
	ggplot2::labs(x = "", y = "Hours listend", 
								title = "Luke's top 15 Spotify artists", 
								subtitle = "23rd March 2012 to 11th July 2025") +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		axis.text.x = ggplot2::element_blank(),
		legend.position = "none",
		text = ggplot2::element_text(size = 13, color="black"),
		plot.title.position = "plot",
		panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(),		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank()
	)

ggplot2::ggsave("outputs/artists_top15.png", width=16, height=13, units="cm", dpi=150, bg="white")

## by year ----
dat_artists_year <- dat_music |>
	dplyr::group_by(year, master_metadata_album_artist_name) |>
	dplyr::summarise(n=round(sum(duration)/60/60)) |> 
	dplyr::arrange(-n)

dat_artists_year |>
	dplyr::filter(dplyr::row_number() <= 5) |>
	ggplot2::ggplot(ggplot2::aes(tidytext::reorder_within(master_metadata_album_artist_name, n, year), fill=n)) +
	ggplot2::geom_col(ggplot2::aes(y = n), alpha = 0.9) +
	ggplot2::geom_text(ggplot2::aes(y = n, label = n), hjust = -0.2, size = 3, color="black") +
	ggplot2::scale_y_continuous(limits = c(0, 39)) +
	viridis::scale_fill_viridis(option="H") +
	ggplot2::coord_flip() +
	ggplot2::labs(x = "", y = "Hours listened", 
								title = "Luke's top Spotify artists",
								subtitle = "23rd March 2012 to 11th July 2025") +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		axis.text.x = ggplot2::element_blank(),
		legend.position = "none",
		text = ggplot2::element_text(size = 11, color="black"),
		plot.title.position = "plot",
		panel.grid.major.x = ggplot2::element_blank(), 
		panel.grid.minor.x = ggplot2::element_blank(),		
		panel.grid.major.y = ggplot2::element_blank(), 
		panel.grid.minor.y = ggplot2::element_blank()
	) +
	tidytext::scale_x_reordered() +
	ggplot2::facet_grid(factor(year, levels=c(2025:2012)) ~ ., scale="free", space="free")

ggplot2::ggsave("outputs/artists_top5_byyear.png", width=20, height=25, units="cm", dpi=150, bg="white")


