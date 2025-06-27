# preprocess OS survey data
OS_practices <- c(
  "Open access",
  "Open peer-review",
  "Pre-printing",
  "Open data",
  "Open code",
  "Open educational resources",
  "Pre-registration",
  "Public engagement"
)

# load data
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1JuRPvEEaBwA2nWU8ptUF7Uq8jrtwlt3etAj5PvRcT40/"
dat_raw <- read_sheet(sheet_id, skip = 1, col_types = "c", trim_ws = TRUE)

# clean data
dat_mut <- dat_raw |>
  mutate(
    `Start data collection` = as.numeric(`Start data collection`),
    `End data collection` = as.numeric(`End data collection`)
  ) |>
  mutate(across(
    all_of(OS_practices), as.numeric
  ))

# summarize proportions
dat_props <- data.frame(practice = OS_practices, prop = colMeans(dat_mut[, OS_practices]) * 100)
dat_props <- sort_by(dat_props, dat_props$prop) 
dat_props$ordered <- factor(dat_props$practice, levels = dat_props$practice, ordered = TRUE)

practices_ordered <- rev(levels(dat_props$ordered))

# proportions by year
dat_years <- dat_mut |> 
  group_by(`End data collection`) |>
  summarise(across(OS_practices, sum)) |>
  pivot_longer(OS_practices, names_to = "practice", values_to = "n") 

dat_years$ordered <- factor(dat_years$practice, levels = practices_ordered, ordered = TRUE)

ggplot(dat_years, aes(x = `End data collection`, y = n, fill = ordered)) +
  geom_area(position = 'stack') +
  labs(x = "Time (years)", y = "Number of studies", fill = "") +
  theme_minimal()

ggplot(dat_props, aes(y = ordered, x = prop, fill = rev(ordered))) +
  geom_bar(stat = "identity") +
  #scale_y_discrete(labels = NULL) +
  labs(x = "Inclusion rate (%)", y = "Open Science practice", fill = NULL) +
  theme_minimal()
