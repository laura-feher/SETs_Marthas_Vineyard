format_result_vals <- function(x) {
  format(round(x, 2), nsmall = 2)
}

format_pval <- function(x) {
  case_when(x > 0.05 ~ "italic(ns)",
            x <= 0.05 & x > 0.01 ~ "italic(p) < 0.05",
            x <= 0.01 & x > 0.001 ~ "italic(p) < 0.01",
            x <= 0.001 ~ "italic(p) < 0.001")
}

format_r2_pval_plot <- function(rate, rate_se, rate_r2, rate_p) {
  paste("list(italic(r[2]) ==", deparse(format_result_vals(rate_r2)), ", ", format_pval(rate_p), ")")
}
