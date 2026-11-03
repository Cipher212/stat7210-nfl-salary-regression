
# REQUIRED PACKAGES
library(tidyverse)
library(nflreadr)



# PART 1: IDENTITY & DEMOGRAPHICS (2011-2025)
qb_baseline <- load_player_stats(seasons = 2011:2025) |>
  filter(position == "QB") |>
  distinct(player_id)

demographics <- load_players() |>
  select(gsis_id, player_name = display_name, height, weight, birth_date, draft_year) |>
  distinct(gsis_id, .keep_all = TRUE)

qb_data <- qb_baseline |>
  inner_join(demographics, by = c("player_id" = "gsis_id")) |>
  mutate(
    bmi = (weight / (height^2)) * 703,
    birth_year = as.numeric(substr(birth_date, 1, 4))
  )

rm(qb_baseline, demographics)


# PART 2: CONTRACT MILESTONES (EXCLUDING ROOKIE SCALE)
contract_milestone <- load_contracts() |>
  filter(!is.na(gsis_id)) |>
  select(-any_of("draft_year")) |> 
  inner_join(qb_data |> select(player_id, draft_year), by = c("gsis_id" = "player_id")) |>
  rename(player_id = gsis_id) |> 
  filter(year_signed > draft_year) |> 
  group_by(player_id) |>
  arrange(year_signed) |>
  slice(1) |> 
  ungroup() |>
  mutate(
    contract_year = year_signed,
    prior_year = year_signed - 1 
  ) |>
  select(player_id, contract_year, prior_year, apy, guaranteed)

qb_data <- qb_data |>
  inner_join(contract_milestone, by = "player_id")

rm(contract_milestone)


# PART 3: SEASONAL PERFORMANCE (LOOKBACK TO 2010)
performance_stats <- load_player_stats(seasons = 2010:2025) |>
  filter(position == "QB") |>
  rename(
    interceptions = any_of(c("passing_interceptions", "interception", "interceptions")),
    passing_epa = any_of(c("passing_epa", "epa")),
    cpoe = any_of(c("passing_cpoe", "cpoe")),
    dakota = any_of(c("dakota", "passing_dakota"))
  )

# Initialize missing advanced metrics for older seasons
if (!"dakota" %in% colnames(performance_stats)) performance_stats$dakota <- NA
if (!"cpoe" %in% colnames(performance_stats)) performance_stats$cpoe <- NA
if (!"passing_epa" %in% colnames(performance_stats)) performance_stats$passing_epa <- NA

performance_stats <- performance_stats |>
  group_by(player_id, season) |>
  summarise(
    attempts = sum(attempts, na.rm = TRUE),
    completions = sum(completions, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(interceptions, na.rm = TRUE),
    passing_epa = sum(passing_epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    dakota = mean(dakota, na.rm = TRUE),
    .groups = "drop"
  )

qb_data <- qb_data |>
  inner_join(performance_stats, by = c("player_id", "prior_year" = "season")) |>
  filter(attempts >= 100) |> 
  mutate(age_at_contract = contract_year - birth_year)

rm(performance_stats)




# PART 4: SALARY INFLATION ADJUSTMENT (2011-2026)
cap_history <- tibble(
  year = 2011:2026,
  cap_value = c(120.0, 120.6, 123.0, 133.0, 143.28, 155.27, 167.0, 177.2, 
                188.2, 198.2, 182.5, 208.2, 224.8, 255.4, 279.2, 301.2)
)

anchor_cap_2026 <- 301.2 # Standardized 2026 cap value





#
# 1. Ensure the existing dataset is in Millions (if not already)
# This checks if the max salary is > 1000. If so, it divides by a million.
if(max(qb_data$apy, na.rm = TRUE) > 1000) {
  qb_data <- qb_data |> 
    mutate(
      apy = apy / 1e6,
      guaranteed = guaranteed / 1e6
    )
}

# 2. Add Brock Purdy (The Closed Block)
# Note: No trailing pipe at the end here!
qb_data <- qb_data |>
  add_row(
    player_name = "Brock Purdy",
    player_id = "00-0037834",
    height = 73,
    weight = 220,
    birth_year = 1999,
    contract_year = 2026,
    prior_year = 2025,
    attempts = 455,
    completions = 300,
    passing_yards = 3864,
    passing_tds = 20,
    interceptions = 12,
    passing_epa = 108.2, 
    cpoe = 2.1,
    dakota = 0.16,
    age_at_contract = 26, 
    apy = 53.0,           # Scaled to Millions
    guaranteed = 100.0,   # Scaled to Millions
    cap_value = 301.2     
  ) |>
  mutate(
    # Recalculate adjusted stats for the new row
    apy_cap_pct = apy / cap_value,
    apy_adj_2026 = apy_cap_pct * 301.2
  )

# 3. VERIFY: This will prove he is in there
cat("New Row Count:", nrow(qb_data), "\n")
tail(qb_data |> select(player_name, apy, age_at_contract), 3)
#




# REQUIRED PACKAGES
library(tidyverse)
library(nflreadr)

# ==========================================
# PART 1: IDENTITY & DEMOGRAPHICS (2011-2025)
# ==========================================
wr_baseline <- load_player_stats(seasons = 2011:2025) |>
  filter(position == "WR") |>
  distinct(player_id)

demographics <- load_players() |>
  select(gsis_id, player_name = display_name, height, weight, birth_date, draft_year) |>
  distinct(gsis_id, .keep_all = TRUE)

wr_data <- wr_baseline |>
  inner_join(demographics, by = c("player_id" = "gsis_id")) |>
  mutate(
    bmi = (weight / (height^2)) * 703,
    birth_year = as.numeric(substr(birth_date, 1, 4))
  )

rm(wr_baseline, demographics)


# ==========================================
# PART 2: CONTRACT MILESTONES (EXCLUDING ROOKIE SCALE)
# ==========================================
contract_milestone <- load_contracts() |>
  filter(!is.na(gsis_id)) |>
  select(-any_of("draft_year")) |> 
  inner_join(wr_data |> select(player_id, draft_year), by = c("gsis_id" = "player_id")) |>
  rename(player_id = gsis_id) |> 
  filter(year_signed > draft_year) |> # Drop rookie deals
  group_by(player_id) |>
  arrange(year_signed) |>
  slice(1) |> # Grabs the FIRST non-rookie contract
  ungroup() |>
  mutate(
    contract_year = year_signed,
    prior_year = year_signed - 1 
  ) |>
  select(player_id, contract_year, prior_year, apy, guaranteed)

wr_data <- wr_data |>
  inner_join(contract_milestone, by = "player_id")

rm(contract_milestone)

# ==========================================
# PART 3: SEASONAL PERFORMANCE (LOOKBACK TO 2010) - FIXED
# ==========================================
performance_stats <- load_player_stats(seasons = 2010:2025) |>
  filter(position == "WR") |>
  group_by(player_id, season) |>
  summarise(
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    # FIXED: The column in nflreadr is actually 'receiving_yards_after_catch'
    yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE),
    receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE),
    receiving_epa = sum(receiving_epa, na.rm = TRUE),
    .groups = "drop"
  )

wr_data <- wr_data |>
  inner_join(performance_stats, by = c("player_id", "prior_year" = "season")) |>
  # Very mild filter to drop practice squad guys but keep the pool massive
  filter(targets >= 10) |> 
  mutate(age_at_contract = contract_year - birth_year)

rm(performance_stats)

# ==========================================
# PART 4: SALARY INFLATION ADJUSTMENT (2011-2026)
# ==========================================
cap_history <- tibble(
  year = 2011:2026,
  cap_value = c(120.0, 120.6, 123.0, 133.0, 143.28, 155.27, 167.0, 177.2, 
                188.2, 198.2, 182.5, 208.2, 224.8, 255.4, 279.2, 301.2)
)

anchor_cap_2026 <- 301.2 # Standardized 2026 cap value

wr_data <- wr_data |>
  inner_join(cap_history, by = c("contract_year" = "year"))

# Ensure the existing dataset is in Millions
if(max(wr_data$apy, na.rm = TRUE) > 1000) {
  wr_data <- wr_data |> 
    mutate(
      apy = apy / 1e6,
      guaranteed = guaranteed / 1e6
    )
}

# Adjust APY mathematically
wr_data <- wr_data |>
  mutate(
    apy_cap_pct = apy / cap_value,
    apy_adj_2026 = apy_cap_pct * anchor_cap_2026
  )

# Verify Output
cat("Total Wide Receivers in Dataset:", nrow(wr_data), "\n")

# Drop formula-based and minimum-wage contracts (Adjusted APY < $3M)
wr_data_clean <- wr_data |>
  filter(apy_adj_2026 >= 3.0)

# Verify the new N count
cat("Final Cleaned WR Count:", nrow(wr_data_clean), "\n")

# export 
write_csv(wr_data_clean, "data/wr_data_final.csv")

