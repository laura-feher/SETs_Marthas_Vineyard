site_base_plot_mv <- function(df, site, base_size = 12, dark_text = "#1A242F") {
  
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  first_date <- first(df$date)
  last_date <- last(df$date)
  width <- .2 * (as.numeric(max(df$date)-min(df$date))/length(df$date))

  cowplot::ggdraw(ggplot(data = df, aes(x = date, y = mean_site_cumu)) +
    geom_errorbar(aes(x = date, ymin = mean_site_cumu - se_site_cumu, ymax = mean_site_cumu + se_site_cumu), width = width, color = "#333333") +
    geom_point(color = "#333333") +
    geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE, color = "#333333") +
    geom_text(data = aq_linear_mod_site_formatted, aes(x = structure(-Inf, class = "Date"), y = Inf, label = paste(linear_rate, "mm/yr")), vjust = 1.5, hjust = -0.5, color = mid_text) +
    geom_text(data = aq_linear_mod_site_formatted, aes(x = as.Date("2020-01-01"), y = Inf, label = R2_pval_plot), vjust = 3, parse = TRUE, color = mid_text) +
    labs(title = site,
         subtitle = "5.09") +
    scale_x_date(expand = c(0,0), limits = c(first_date - 200, last_date + 200), date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = "Surface elevation change (mm)") +
    theme_mv_sets()) +
    geom_text(data = data.frame(x = 0.15, y = 0.5, label = paste(aq_linear_mod_site_formatted$linear_rate, "mm/yr")),
              aes(x = x, y = y, label = label), size = 11/.pt, color = mid_text)
}
