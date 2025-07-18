format_result_vals <- function(x) {
  format(round(x, 2), nsmall = 2, digits = 2)
}

format_pval <- function(x, output_type = "text") {
  
  if (output_type == "plot") {
    case_when(x > 0.05 ~ "italic(ns)",
              x <= 0.05 & x > 0.01 ~ "italic(p) < 0.05",
              x <= 0.01 & x > 0.001 ~ "italic(p) < 0.01",
              x <= 0.001 ~ "italic(p) < 0.001")
  } else if (output_type == "text") {
    case_when(x > 0.05 ~ "ns",
              x <= 0.05 & x > 0.01 ~ "p < 0.05",
              x <= 0.01 & x > 0.001 ~ "p < 0.01",
              x <= 0.001 ~ "p < 0.001")
  } else if (output_type == "subtitle") {
    case_when(x > 0.05 ~ "",
              x <= 0.05 & x > 0.01 ~ "< 0.05",
              x <= 0.01 & x > 0.001 ~ "< 0.01",
              x <= 0.001 ~ "< 0.001")
  }
}

format_r2 <- function(x, output_type = "text") {
  
  if (output_type == "plot") { 
  # paste("list(italic(r[2]) ==", format_result_vals(x), ")")
  paste("italic(r[2]) == ", format_result_vals(x))
  } else if (output_type == "text") {
    format_result_vals(x)
  }
}
