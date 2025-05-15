# Figure listing features under 4 major groups

getwd()

library(tidyverse)
library(ggplot2)
library(showtext)

features_lst = list(
  "Nucleotide composition features" = c(
    "AT/GC\ncontent",
    "CpG island\n(CGI) density",
    "isochores",
    "sequence\nsimilarity"
  ),
  "Functional sequence motifs and elements" = c(
    "transcription\nfactor motifs",
    "early replicating\ncontrol elements\n(ERCEs)",
    "enhancers",
    "insulators",
    "facilitators",
    "promoters",
    "housekeeping genes"
  ),
  "Repeat elements" = c(
    "B1/B2/Alu",
    "L1/LINE1",
    "ERV-K",
    "hAT-Charlie",
    "MIR",
    "SVA",
    "tandem repeats",
    "satellite DNAs"
  ),
  "DNA secondary structures" = c(
    "DNA triplexes",
    "G-quadruplexes\n(G4)",
    "i-motifs",
    "R-loops",
    "Z-DNAs"
  )
)

group_colours = c(
  "#0C9AC0",
  "#8054B2",
  "#E56991",
  "#2A936B"
)

group_colours <- c(
  "#72C5DD",  # pastel blue-cyan (Motifs and DNA-level elements)
  "#B39DCC",  # pastel purple (Chromatin fibre and loops)
  "#F29DB5",  # pastel pink-rose (Chromatin domains and TADs)
  "#7FC8A3"   # pastel teal green (Repeats and compositional biases)
)

group_colours <- c(
  "#3FAED2",  # medium blue-cyan
  "#9973C2",  # medium purple
  "#EC7CA6",  # medium pink-rose
  "#4DBB8B"   # medium teal green
)

names(group_colours) <- names(features_lst)

# Add Google font (e.g., Montserrat as a Futura-like alternative)
#font_add_google("Montserrat", "mont")  # "mont" is the alias
font_add("futura", regular = "fonts/futura.ttf")
font_add("Courier New", regular = "fonts/Courier New.ttf")
font_add("Courier", regular = "fonts/Courier.ttf")
font = "Courier"

# Enable showtext
showtext_auto()

# ----- MAIN -----

# Prepare data
df <- tibble(
  category = rep(names(features_lst), times = sapply(features_lst, length)),
  feature = unlist(features_lst)
)
df$category <- factor(df$category, levels = names(features_lst))

# Arrange by category so same-category elements are adjacent
df <- df %>% 
  arrange(factor(category, levels = names(features_lst)))

# Set grid dimensions (e.g., 6 columns)
n_cols <- 6
df <- df %>%
  mutate(
    x = (row_number() - 1) %% n_cols,
    y = -((row_number() - 1) %/% n_cols)
  )

# Plot
p <- ggplot(df, aes(x = x, y = y, fill = category, label = feature)) +
  geom_tile(color = "white", size = 0.8, width = 0.95, height = 0.95) +
  geom_text(size = 5, color = "black", hjust = 0.5, vjust = 0.5, family = font) +
  #scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = group_colours) + 
  theme_void() +
  labs(title = "Sequence features linked to 3D genome organisation") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "mono", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, family = "mono", size = 20)
  )

ggsave("Figure_2.pdf", plot = p, width = 16, height = 6)
ggsave("Figure_2.svg", plot = p, width = 16, height = 6)

# rm(list=ls()); gc()
