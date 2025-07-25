#' Plotting function for Martha's vineyard SET data
#'
#' @param site_df data frame. A data frame containing site-level cumulative
#'   surface elevation change.
#' @param site_rate_df data frame. A data frame containing the site-level linear
#'   model & estimate of surface elevation change.
#' @param plot_type string. Type of surface elevation change plot. One of:
#'   * `"simple SET plot"`: A plot of surface elevation change with labels for the linear rate of change on the plot panel.
#'   * `"SLR labeled"`: Includes lines for long-term and recent SLR rates and labels for the rates on the right side.
#'   * `"SLR no labels"`: Same as `"SLR labeled"` but without the rate labels on the right.
#'   * `"future SLR labeled"`: A plot of long-term, recent, and predicted future SLR rates and labels for the rates on the right side.
#'   * `"future SLR no labels"`: Same as `"future SLR labeled"` but without the rate labels on the right.
#'
#' @import dplyr
#' @import ggplot2
#' @import here
#' @import ggedit
#' @import ggthemr
#' @import ggtext
#'
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom colorspace lighten
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' # load data from Aquinnah and create a simple SET plot
#' aquinnah <- read_rds(here::here("data", "aquinnah.rds"))
#' mv_plot(site_df = aquinnah$aq_cumu_site, site_rate_df = aquinnah$aq_linear_mod_site, plot_type = "simple SET plot")
#' 
mv_plot <- function(site_df, site_rate_df, plot_type) {
  
  # Setup defaults for all plots
  ## Load roboto condensed font
  sysfonts::font_add_google("Roboto Condensed")
  showtext::showtext_auto()
  
  ## Set plot theme & colors
  ggthemr::ggthemr('flat', layout = "clear", type = "outer")
  base_color <- "#002635"
  panel_color <- "#fafaf8"
  
  ## Get dates for plot x-axis range and errorbar width
  first_date <- first(site_df$date)
  last_date <- last(site_df$date)
  width <- .2 * (as.numeric(max(site_df$date)-min(site_df$date))/length(site_df$date))
  total_days <- as.numeric(last_date - first_date)
  x_adj_factor <- 0.059 * total_days
  
  # Get model results for labeling within the simple SET plot
  SET_rate_lab <- site_rate_df %>% 
    filter(term == "date_num") %>%
    mutate(R2 = format_r2(adj.r.squared, output_type = "plot"),
           p_val = format_pval(x = p.value, output_type = "plot")) 
  
  
  # Setup 'base' plot - a simple plot of just SET data + rate of change
  SET_base_plot <- ggplot(data = site_df, aes(x = date, y = mean_site_cumu)) +
    geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE, color = base_color) +
    geom_errorbar(aes(x = date, ymin = mean_site_cumu - se_site_cumu, ymax = mean_site_cumu + se_site_cumu), width = width, color = base_color) +
    geom_point(color = base_color, size = 2, shape = 21, fill = colorspace::lighten(base_color, amount = 0.7)) +
    geom_text(data = SET_rate_lab, aes(x = first_date + (0.001 * as.numeric(first_date)), y = Inf, label = paste0("Rate: ", linear_rate, " mm/yr\n")), vjust = 1.5, hjust = 0, size = 11/.pt, family = "Roboto Condensed") +
    geom_text(data = SET_rate_lab, aes(x = first_date + (0.001 * as.numeric(first_date)), y = Inf, label =  paste("list(", R2, ",", p_val, ")")), vjust = 3.5, hjust = 0, parse = TRUE, size = 11/.pt, family = "Roboto Condensed") +
    labs(title = unique(site_df$site), subtitle = "Marsh Surface elevation change") +
    scale_x_date(expand = c(0,0), limits = c(first_date - x_adj_factor, last_date + x_adj_factor), date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = "Surface elevation change (mm)") +
    theme(text = element_text(family = "Roboto Condensed", color = base_color),
          plot.title = element_text(size = rel(1.6), margin = margin(12, 0, 8, 0)),
          panel.border = element_rect(fill = NA, color = base_color),
          panel.background = element_rect(fill = panel_color),
          axis.text.y = element_text(size = rel(0.9)),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.position = "none",
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"))
  
  
  # If plot_type == "simple SET plot", return just the base SET plot
  if (plot_type == "simple SET plot") {
    return(SET_base_plot)
  }
  
  
  # If the plot should include SLR lines:
  ## Setup SLR lines/labs 
  if(plot_type %in% c("SLR no labels", "SLR labeled", "future SLR no labels", "future SLR labeled")) {
    
    # Get Woods Hole SLR data
    woods_hole_slr <- read_rds(here::here("data", "woods_hole_slr.rds"))
    
    # Gather the SEC, long-term, recent, and future SLR rates into a single df
    woods_hole_slr_rates <- site_rate_df %>% 
      ungroup() %>% 
      filter(term == "date_num") %>% 
      select("SEC" = estimate) %>%
      bind_cols(., woods_hole_slr$woods_hole_slr_long %>% 
                  select("slr_long" = MSL.Trends.mm.yr.)) %>%
      bind_cols(., woods_hole_slr$woods_hole_slr_recent %>%
                  filter(term == "yr") %>%
                  select("slr_recent" = estimate)) %>%
      bind_cols(., woods_hole_slr$woods_hole_slr_future %>%
                  select(scenario_name, rsl_rate_mm_yr) %>%
                  pivot_wider(., names_from = scenario_name, values_from = rsl_rate_mm_yr, names_prefix = "slr_"))
    
    # Create lines for each rate that span the xaxis range of the plot
    woods_hole_slr_lines <- data.frame(date = seq(first_date - x_adj_factor, last_date + x_adj_factor, by = 1)) %>%
      bind_cols(., woods_hole_slr_rates) %>%
      bind_cols(., site_rate_df %>% 
                  ungroup() %>% 
                  filter(term == "(Intercept)") %>% 
                  select("intercept" = estimate)) %>%
      mutate(date_num = as.numeric(date - first_date)/365.25,
             across(-c(date, intercept, date_num), ~ intercept + .x * date_num)) %>%
      select(date, SEC, slr_long, slr_recent, slr_low, slr_int_low, slr_int, slr_int_high, slr_high) %>%
      pivot_longer(., cols = -date, names_to = "rate_type", values_to = "y_val")
    
    # Set SLR colors, yaxis title, caption, and plot subtitle 
    slr_palette <- c(base_color, "#14747e", "#1abc9c", "#7fc06e", "#ffcc1b", "#f08e48", "#ff5a67", "#c43060") # https://github.com/ajlende/base16-atlas-scheme
    yaxis_title <- "Surface elevation change &\n Sea-Level Rise (mm)"
    plot_subtitle <- "Marsh Surface Elevation Change & Sea-Level Rise"
    
    site_name <- unique(site_df$site)
    
    plot_caption_slr <- SET_rate_lab %>%
      bind_cols(., woods_hole_slr_rates) %>%
      mutate(first_year = map_chr(data, ~as.character(year(first(.x$date)))),
             last_year = map_chr(data, ~as.character(year(last(.x$date))))) %>%
      mutate(lab = paste0("Surface elevation change at ", site_name, " was ", format_result_vals(estimate), " mm/yr (± ", format_result_vals(std.error), ", ", "<i>r^2^</i>", " = ", format_result_vals(adj.r.squared), ", ", "<i>p</i>", " ", format_pval(p.value, output_type = "subtitle"),  "). The long-term rate of relative sea-level rise (RSLR) was ", format_result_vals(slr_long), " mm/yr (1932-2025), whereas the recent rate of RSLR was ", format_result_vals(slr_recent), " mm/yr (2001-2019).")) %>%
      pull(lab)
    
    plot_caption_future_slr <- SET_rate_lab %>%
      bind_cols(., woods_hole_slr_rates) %>%
      mutate(first_year = map_chr(data, ~as.character(year(first(.x$date)))),
             last_year = map_chr(data, ~as.character(year(last(.x$date))))) %>%
      mutate(lab = paste0("Surface elevation change at ", site_name, " was ", format_result_vals(estimate), " mm/yr (± ", format_result_vals(std.error), ", ", "<i>r^2^</i>", " = ", format_result_vals(adj.r.squared), ", ", "<i>p</i>", " ", format_pval(p.value, output_type = "subtitle"),  "). The long-term rate of relative sea-level rise (RSLR) was ", format_result_vals(slr_long), " mm/yr (1932-2025), whereas the recent rate of RSLR was ", format_result_vals(slr_recent), " mm/yr (2001-2019). Future rates of RSLR were estimated at ", round(slr_low), ", ", round(slr_int_low), ", ", round(slr_int), ", ", round(slr_int_high), ", and ", round(slr_high), " mm/yr (respectively) for the low, intermediate-low, intermediate, intermediate-high, and high SLR scenarios.")) %>%
      pull(lab)
    
    # Grab points + errorbars from base plot 
    SET_points <- ggedit::cloneLayer(SET_base_plot$layers[[3]])
    SET_errorbars <- ggedit::cloneLayer(SET_base_plot$layers[[2]])
    
    # Define updated theme elements
    slr_plot_theme <- theme(axis.text.y.right = ggtext::element_markdown(),
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            plot.caption.position = "plot",
                            plot.caption = ggtext::element_textbox_simple(hjust = 0))
    
    
    if (plot_type %in% c("SLR no labels", "SLR labeled")) {
      
      new_smooth <- geom_smooth(data = woods_hole_slr_lines %>% filter(rate_type %in% c("SEC", "slr_long", "slr_recent")), aes(x = date, y = y_val, color = rate_type), method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE)
      plot_caption <- plot_caption_slr
      scale_colors <- scale_color_manual(values = slr_palette[1:3], breaks = c("SEC", "slr_long", "slr_recent"), labels = c("SEC", "Long-term SLR", "Recent SLR"))
      
    } else if (plot_type %in% c("future SLR no labels", "future SLR labeled")) {
      
      new_smooth <- geom_smooth(data = woods_hole_slr_lines, aes(x = date, y = y_val, color = rate_type), method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE)
      plot_caption <- plot_caption_future_slr
      scale_colors <- scale_color_manual(values = slr_palette, breaks = c("SEC", "slr_long", "slr_recent", "slr_low", "slr_int_low", "slr_int", "slr_int_high", "slr_high"),
                                         labels = c("SEC", "Long-term SLR", "Recent SLR", "Future Low SLR", "Future Int-Low SLR", "Future Int SLR", "Future Int-High SLR", "Future High SLR"))
    }
    
    
    if (plot_type %in% c("SLR no labels", "future SLR no labels")) {
      scale_y <- scale_y_continuous(name = yaxis_title)
    }
    
    
    if (plot_type %in% c("SLR labeled", "future SLR labeled")) {
      
      slr_line_labels <- woods_hole_slr_lines %>%
        group_by(rate_type) %>% 
        top_n(1, date) %>%
        left_join(., woods_hole_slr_rates %>%
                    pivot_longer(., cols = everything(), names_to = "rate_type", values_to = "rate"),
                  by = "rate_type") %>%
        mutate(color = case_when(
          rate_type == "SEC" ~ slr_palette[1],
          rate_type == "slr_long" ~ slr_palette[2],
          rate_type == "slr_recent" ~ slr_palette[3],
          rate_type == "slr_low" ~ slr_palette[4],
          rate_type == "slr_int_low" ~ slr_palette[5],
          rate_type == "slr_int" ~ slr_palette[6],
          rate_type == "slr_int_high" ~ slr_palette[7],
          rate_type == "slr_high" ~ slr_palette[8]
        ),
        line_label_color = paste0("<span style = 'color:", color, "'>", paste0(format_result_vals(rate), " mm/yr"), "</span>"))
      
      if (plot_type == "SLR labeled") {
        scale_y <- scale_y_continuous(name = yaxis_title, sec.axis = sec_axis(~ ., labels = slr_line_labels %>% filter(rate_type %in% c("SEC", "slr_long", "slr_recent")) %>% pull(line_label_color),
                                                                              breaks = slr_line_labels %>% filter(rate_type %in% c("SEC", "slr_long", "slr_recent")) %>% pull(y_val)))
      } else if (plot_type == "future SLR labeled") {
        scale_y <- scale_y_continuous(name = yaxis_title, sec.axis = sec_axis(~ ., labels = slr_line_labels %>% pull(line_label_color),
                                                                              breaks = slr_line_labels %>% pull(y_val)))
      }
    }
    
    slr_plot <- SET_base_plot %>%
      ggedit::remove_geom("text", 4) %>%
      ggedit::rgg(., oldGeom = "smooth", oldGeomIdx = 1, newLayer = new_smooth) %>%
      ggedit::rgg(., oldGeom = "errorbar", oldGeomIdx = 2, newLayer = SET_errorbars) %>%
      ggedit::rgg(., oldGeom = "point", oldGeomIdx = 3, newLayer = SET_points) +
      scale_y +
      scale_colors +
      labs(subtitle = plot_subtitle, caption = plot_caption) +
      slr_plot_theme
    
    return(slr_plot)
    
  }
}
