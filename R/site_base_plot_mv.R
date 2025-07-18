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

plot_fun_station <- function(df, rates = NULL, base_size = 12, dark_text = "#1A242F") {
  
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  first_date <- first(df$date)
  last_date <- last(df$date)
  width <- .2 * (as.numeric(max(df$date)-min(df$date))/length(df$date))
  
  rate_labs <- rates %>% 
    mutate(linear_rate = paste0(format_result_vals(x = rate), " ± ", format_result_vals(x = rate_se)),
           R2 = format_r2(rate_r2, output_type = "plot"), 
           p_val = format_pval(x = rate_p, output_type = "plot")) %>%
    select(linear_rate, R2, p_val)
  
  name_lab <- paste0(unique(df$site), ": ", unique(df$station))
  
  ggplot(data = df, aes(x = date, y = mean_station_cumu)) +
    geom_errorbar(aes(x = date, ymin = mean_station_cumu - se_station_cumu, ymax = mean_station_cumu + se_station_cumu), width = width, color = "#333333") +
    geom_point(color = "#333333") +
    geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE, color = "#333333") +
    geom_text(data = rate_labs, aes(x = structure(-Inf, class = "Date"), y = Inf, label = paste0("Rate: ", linear_rate, " mm/yr\n")), vjust = 1.5, hjust = 0, color = mid_text, size = 11/.pt) +
    geom_text(data = rate_labs, aes(x = structure(-Inf, class = "Date"), y = Inf, label =  paste("list(", R2, ",", p_val, ")")), vjust = 3.5, hjust = 0, color = mid_text, parse = TRUE) +
    labs(title = name_lab) +
    scale_x_date(expand = c(0,0), limits = c(first_date - 200, last_date + 200), date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = "Surface elevation change (mm)") +
    theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = "Didact Gothic", lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = "Didact Gothic", size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 2, 0)),
          axis.text.y = element_text(colour = light_text, size = rel(0.9)),
          axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = mid_text, size = 12),
          axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = 1,
          panel.grid = element_line(colour = "#E8E7E7"),
          plot.background = element_rect(fill = "#F7F7F7"),
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
}

map2(aq_cumu_station, aq_linear_mod_station, ~plot_fun_station(df = .x, rates = .y))