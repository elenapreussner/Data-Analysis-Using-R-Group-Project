library(tidyverse)
library(readxl)

# Daten laden
schulen <- read_xlsx("~/Uni/Data Analysis Using R/school_data.xlsx")
hauspreise <- read_csv("~/Uni/Data Analysis Using R/CampusFile_HK_2022.csv")

# Schultypen definieren
abitur <- c(20, 15)                                    
kein_abitur <- c(4, 10, 14)

# Funktion: Finde 5x5 Nachbarn mit Distanz
finde_nachbarn <- function(df, max_dist = 2) {
  df %>%
    separate(ergg_1km, c("x", "y"), "_", remove = FALSE, convert = TRUE) %>%
    rowwise() %>%
    reframe(
      expand.grid(dx = -max_dist:max_dist, dy = -max_dist:max_dist) %>%
        mutate(
          ergg_1km = paste0(x + dx, "_", y + dy),
          distanz = pmax(abs(dx), abs(dy))
        )
    )
}

# Alle Zellen (Schule + Nachbarn) für beide Schultypen finden
zellen_abitur <- schulen %>%
  filter(school_type %in% abitur) %>%
  finde_nachbarn()

zellen_kein_abitur <- schulen %>%
  filter(school_type %in% kein_abitur) %>%
  finde_nachbarn()

# Treatment-Zellen auf Überlappungen prüfen
alle_treatment <- bind_rows(
  zellen_abitur %>% filter(distanz <= 1),
  zellen_kein_abitur %>% filter(distanz <= 1)
)

saubere_treatment <- alle_treatment %>%
  count(ergg_1km) %>%
  filter(n == 1) %>%
  pull(ergg_1km)

# Treatment nach Typ aufteilen
treatment_abitur <- zellen_abitur %>%
  filter(distanz <= 1, ergg_1km %in% saubere_treatment) %>%
  select(ergg_1km) %>%
  mutate(treat_abitur = 1)

treatment_kein_abitur <- zellen_kein_abitur %>%
  filter(distanz <= 1, ergg_1km %in% saubere_treatment) %>%
  select(ergg_1km) %>%
  mutate(treat_kein_abitur = 1)

# Buffer (distanz == 2)
buffer <- bind_rows(
  zellen_abitur %>% filter(distanz == 2),
  zellen_kein_abitur %>% filter(distanz == 2)
) %>%
  distinct(ergg_1km) %>%
  mutate(buffer = 1)

# Alles zusammenführen
grid_treatment <- full_join(treatment_abitur, treatment_kein_abitur, by = "ergg_1km") %>%
  full_join(buffer, by = "ergg_1km") %>%
  replace_na(list(treat_abitur = 0, treat_kein_abitur = 0, buffer = 0))

# Mit Hauspreisen verknüpfen
haeuser <- hauspreise %>%
  filter(ergg_1km != -9) %>%
  left_join(grid_treatment, by = "ergg_1km") %>%
  replace_na(list(treat_abitur = 0, treat_kein_abitur = 0, buffer = 0)) %>%
  select(plz, kaufpreis, ergg_1km, treat_abitur, treat_kein_abitur, buffer)

# Finaler Analyse-Datensatz (ohne Buffer)
haeuser_final <- haeuser %>% 
  filter(buffer == 0) %>% 
  select(plz, kaufpreis, ergg_1km, treat_abitur, treat_kein_abitur) #anpassen!

# =============================================================================
# DIAGNOSTIK
# =============================================================================

cat("\n=== GRID-ZELLEN ===\n")
cat("Treatment (Abitur):", sum(grid_treatment$treat_abitur), "\n")
cat("Treatment (Kein Abitur):", sum(grid_treatment$treat_kein_abitur), "\n")
cat("Buffer-Zonen:", sum(grid_treatment$buffer), "\n")

anzahl_unique_treatment <- n_distinct(alle_treatment$ergg_1km)
cat("Ausgeschlossen (Überlappungen):", anzahl_unique_treatment - length(saubere_treatment), 
    sprintf("(%.1f%%)\n", 100 * (anzahl_unique_treatment - length(saubere_treatment)) / anzahl_unique_treatment))

cat("\n=== HÄUSER (vor Buffer-Filter) ===\n")
cat("Gesamt:", nrow(haeuser), "\n")
cat("Bei Abitur-Schulen:", sum(haeuser$treat_abitur), "\n")
cat("Bei Nicht-Abitur-Schulen:", sum(haeuser$treat_kein_abitur), "\n")
cat("In Buffer-Zone:", sum(haeuser$buffer), "\n")
cat("Control:", sum(haeuser$treat_abitur == 0 & haeuser$treat_kein_abitur == 0 & haeuser$buffer == 0), "\n")

cat("\n=== TREATMENT-VERLUST DURCH BUFFER ===\n")
urspruenglich_abitur <- sum(grid_treatment$treat_abitur == 1)
urspruenglich_nicht_abitur <- sum(grid_treatment$treat_kein_abitur == 1)
abitur_und_buffer <- sum(grid_treatment$treat_abitur == 1 & grid_treatment$buffer == 1)
nicht_abitur_und_buffer <- sum(grid_treatment$treat_kein_abitur == 1 & grid_treatment$buffer == 1)

cat("Abitur ursprünglich:", urspruenglich_abitur, "→ Final:", urspruenglich_abitur - abitur_und_buffer,
    sprintf("(%.1f%% behalten)\n", 100 * (urspruenglich_abitur - abitur_und_buffer) / urspruenglich_abitur))
cat("Nicht-Abitur ursprünglich:", urspruenglich_nicht_abitur, "→ Final:", urspruenglich_nicht_abitur - nicht_abitur_und_buffer,
    sprintf("(%.1f%% behalten)\n", 100 * (urspruenglich_nicht_abitur - nicht_abitur_und_buffer) / urspruenglich_nicht_abitur))

cat("\n=== FINALER DATENSATZ (haeuser_final) ===\n")
cat("Gesamt Häuser:", nrow(haeuser_final), "\n")
cat("Treatment Abitur:", sum(haeuser_final$treat_abitur), "\n")
cat("Treatment Nicht-Abitur:", sum(haeuser_final$treat_kein_abitur), "\n")
cat("Control:", sum(haeuser_final$treat_abitur == 0 & haeuser_final$treat_kein_abitur == 0), "\n")
