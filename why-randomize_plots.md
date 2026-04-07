# Why Randomize Plots


# Setup

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.0     ✔ readr     2.2.0
    ✔ forcats   1.0.0     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ✔ purrr     1.2.1     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggpattern)
library(showtext)
```

    Loading required package: sysfonts
    Loading required package: showtextdb

``` r
## C4R styling defined here ----
view_dpi <- 144
plot_dpi <- 300
options(rstudio.plots.dpi = plot_dpi)
font_add_google("JetBrains Mono", "JetBrains Mono")
showtext_auto()

color_purple <- "#6F00FF"
color_blue <- "#008FFF"
color_dk_grey <- "#333132"
color_int_lt_grey <- "#A2A2A2"
color_int_dk_grey <- "#E0E0E0"
color_lt_grey <- "#F3F3F3"

theme_c4r <- function(
  fontsize = 14,
  plot_dpi = 300,
  solid_axis = FALSE,
  legend.fontsize = 10
) {
  theme_grey() +
    theme(
      # Panel
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      panel.grid = element_line(color = color_int_dk_grey),
      panel.grid.minor = element_blank(),

      # axes
      axis.line = element_line(
        color = color_dk_grey,
        linewidth = ifelse(solid_axis, 0.5, 0)
      ),
      axis.ticks = element_blank(),
      axis.text = element_text(
        family = "JetBrains Mono",
        size = fontsize * plot_dpi / 96
      ),
      axis.title = element_text(
        family = "JetBrains Mono",
        size = fontsize * plot_dpi / 96
      ),

      # Strip (facets)
      strip.background = element_rect(
        fill = color_int_lt_grey,
        colour = "grey20"
      ),

      # Legend
      legend.key = element_rect(fill = "white", colour = NA),
      legend.text = element_text(
        family = "JetBrains Mono",
        size = legend.fontsize * plot_dpi / 96
      ),
      legend.title = element_text(
        family = "JetBrains Mono",
        size = legend.fontsize * plot_dpi / 96
      ),

      # Complete theme
      complete = TRUE
    )
}

c4r_geom_textsize <- function(fontsize = 14, plot_dpi = 300) {
  (fontsize * plot_dpi / 96) / 2.835
}
```

# Data

``` r
dat <- read_csv("data/why-randomize-data.csv") %>%
  rename(condition = group)
```

    Rows: 40 Columns: 8
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (3): group, sex, litter
    dbl (5): mouse_id, age, weight, rotarod, survival

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
names(dat)
```

    [1] "mouse_id"  "condition" "sex"       "litter"    "age"       "weight"   
    [7] "rotarod"   "survival" 

``` r
survival_plot_range <- c(125, 155)
rotarod_plot_range <- c(60, 160)
```

# Plots in Activity

## Screen 1 (differences between condition)

``` r
ggplot(dat, aes(x = condition, y = age, color = condition)) +
  geom_point(
    size = 3,
    alpha = 0.8,
    position = position_jitter(width = 0.2, height = 0)
  ) +
  theme_c4r(plot_dpi = view_dpi) +
  scale_color_manual(values = c(color_blue, color_purple)) +
  coord_cartesian(ylim = c(60, 80))
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
ggplot(dat, aes(x = condition, fill = condition, pattern = fct_rev(sex))) +
  geom_bar_pattern(
    color = "white",
    pattern_fill = "white",
    pattern_colour = "white",
    pattern_angle = 45,
    position = "dodge"
  ) +
  theme_c4r(plot_dpi = view_dpi) +
  scale_pattern_manual(values = c("none", "stripe")) +
  scale_fill_manual(values = c(color_blue, color_purple))
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-5-2.png)

``` r
ggplot(dat, aes(x = condition, y = weight, color = condition)) +
  geom_point(size = 3, position = position_jitter(width = 0.2, height = 0)) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, linewidth = 0.8) +
  theme_c4r(plot_dpi = view_dpi) +
  scale_color_manual(values = c(color_blue, color_purple)) +
  coord_cartesian(ylim = c(22, 28))
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-5-3.png)

``` r
ggplot(dat, aes(x = condition, y = rotarod, color = condition)) +
  geom_point(size = 3, position = position_jitter(width = 0.2, height = 0)) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, linewidth = 0.8) +
  theme_c4r(plot_dpi = view_dpi) +
  scale_color_manual(values = c(color_blue, color_purple)) +
  scale_y_continuous(breaks = seq(0, 160, by = 40)) +
  coord_cartesian(ylim = rotarod_plot_range)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-5-4.png)

``` r
dat %>%
  count(condition, litter) %>%
  ggplot(aes(x = condition, y = n, fill = condition, group = litter)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(
    aes(label = litter, y = n / 2),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    color = "white",
    size = c4r_geom_textsize(10)
  ) +
  theme_c4r(plot_dpi = view_dpi) +
  scale_fill_manual(values = c(color_blue, color_purple)) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 6))
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-5-5.png)

## Screen 2 (correlations with survival)

``` r
ggplot(dat, aes(x = fct_rev(sex), y = survival)) +
  geom_point(
    alpha = 0.8,
    color = color_int_lt_grey,
    size = 3,
    position = position_jitter(width = 0.2, height = 0)
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.5,
    linewidth = 0.8,
    color = color_int_lt_grey
  ) +
  theme_c4r(plot_dpi = view_dpi) +
  coord_cartesian(ylim = survival_plot_range)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
ggplot(dat, aes(x = weight, y = survival)) +
  geom_point(
    alpha = 0.8,
    color = color_int_lt_grey,
    size = 3
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_c4r(plot_dpi = 144) +
  coord_cartesian(xlim = c(22, 28), ylim = survival_plot_range)
```

    `geom_smooth()` using formula = 'y ~ x'

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-6-2.png)

``` r
ggplot(dat, aes(x = rotarod, y = survival)) +
  geom_point(
    alpha = 0.8,
    color = color_int_lt_grey,
    size = 3
  ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_c4r(plot_dpi = view_dpi) +
  coord_cartesian(xlim = rotarod_plot_range, ylim = survival_plot_range)
```

    `geom_smooth()` using formula = 'y ~ x'

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-6-3.png)

``` r
ggplot(dat, aes(x = litter, y = survival)) +
  geom_point(
    alpha = 0.8,
    color = color_int_lt_grey,
    size = 3,
    position = position_jitter(width = 0.2, height = 0)
  ) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.5,
    linewidth = 0.8,
    color = color_int_lt_grey
  ) +
  theme_c4r(plot_dpi = view_dpi) +
  coord_cartesian(ylim = survival_plot_range)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-6-4.png)

# Plots in Slides

``` r
rotarod_label <- "Rotarod Time (s)"
survival_label <- "Survival (days)"
condition_label <- NULL
```

## Survival vs condition

``` r
make_panel_survival_condition <- function(dpi) {
  ggplot(dat, aes(x = condition, y = survival, color = condition)) +
    geom_point(
      size = 3,
      alpha = 0.8,
      position = position_jitter(width = 0.2, height = 0)
    ) +
    stat_summary(
      fun = median,
      geom = "crossbar",
      width = 0.5,
      linewidth = 0.8,
      alpha = 0.8
    ) +
    stat_summary(
      fun = median,
      geom = "text",
      aes(label = after_stat(round(y, 1))),
      hjust = 0,
      position = position_nudge(x = 0.3),
      family = "JetBrains Mono",
      size = c4r_geom_textsize(plot_dpi = dpi)
    ) +
    theme_c4r(plot_dpi = dpi) +
    labs(x = condition_label, y = survival_label) +
    scale_color_manual(values = c(color_blue, color_purple)) +
    coord_cartesian(ylim = survival_plot_range) +
    theme(legend.position = "none")
}

make_panel_survival_condition(view_dpi)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-8-1.png)

``` r
p <- make_panel_survival_condition(plot_dpi)

## export plot as png ----
plot_width <- 5 # inches
plot_height <- 4 # inches

ggsave(
  "plot_survival_condition.png",
  p,
  width = plot_width,
  height = plot_height,
  units = "in",
  dpi = plot_dpi
)
```

## Time to Fall vs condition

``` r
make_panel_rotarod_condition <- function(dpi, df = dat) {
  ggplot(df, aes(x = condition, y = rotarod, color = condition)) +
    geom_point(
      size = 3,
      alpha = 0.8,
      position = position_jitter(width = 0.2, height = 0)
    ) +
    stat_summary(
      fun = median,
      geom = "crossbar",
      width = 0.5,
      linewidth = 0.8,
      alpha = 0.8
    ) +
    theme_c4r(plot_dpi = dpi) +
    labs(x = condition_label, y = rotarod_label) +
    scale_color_manual(values = c(color_blue, color_purple)) +
    scale_y_continuous(breaks = seq(0, 160, by = 40)) +
    coord_cartesian(ylim = rotarod_plot_range) +
    theme(legend.position = "none")
}

make_panel_rotarod_condition(view_dpi)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
p <- make_panel_rotarod_condition(plot_dpi)

## export plot as png ----
plot_width <- 5 # inches
plot_height <- 4 # inches

ggsave(
  "plot_rotarod_condition.png",
  p,
  width = plot_width,
  height = plot_height,
  units = "in",
  dpi = plot_dpi
)
```

## Survival vs Time to Fall

``` r
make_panel_survival_rotarod <- function(dpi) {
  ggplot(dat, aes(x = rotarod, y = survival)) +
    geom_point(
      alpha = 0.8,
      color = color_int_lt_grey,
      size = 3
    ) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    labs(x = rotarod_label, y = survival_label) + 
    theme_c4r(plot_dpi = dpi) +
    coord_cartesian(xlim = rotarod_plot_range, ylim = survival_plot_range)
}

make_panel_survival_rotarod(view_dpi)
```

    `geom_smooth()` using formula = 'y ~ x'

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-10-1.png)

``` r
p <- make_panel_survival_rotarod(plot_dpi)

## export plot as png ----
plot_width <- 5 # inches
plot_height <- 4 # inches

ggsave(
  "plot_survival_rotarod.png",
  p,
  width = plot_width,
  height = plot_height,
  units = "in",
  dpi = plot_dpi
)
```

    `geom_smooth()` using formula = 'y ~ x'

## Corrected Plot (Survival vs. Condition)

``` r
set.seed(42)
rand_condition <- sample(c("Control", "Treatment"), NROW(dat), replace = TRUE)
dat_rand <- dat
dat_rand$condition <- rand_condition

make_panel_rotarod_condition(view_dpi, df = dat_rand)
```

![](why-randomize_plots_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
p <- make_panel_rotarod_condition(plot_dpi, df = dat_rand)

## export plot as png ----
plot_width <- 5 # inches
plot_height <- 4 # inches

ggsave(
  "plot_rotarod_condition_rand.png",
  p,
  width = plot_width,
  height = plot_height,
  units = "in",
  dpi = plot_dpi
)
```
