# Manage special characters in LaTeX
LaTeX_escape = function(x){
  x |>
    str_replace_all(c(
      "\\\\" = "\\\\textbackslash{}")) |>
    str_replace_all(c(
      "\\$"  = "\\\\$",
      "\\%"  = "\\\\%",
      "\\&"  = "\\\\&",
      "\\_"  = "\\\\_",
      "\\#"  = "\\\\#"))}

# Trim specified number of lines from a string (application to generated LaTeX)
LaTeX_trim_lines = function(x, drop_top = 0L, drop_bottom = 0L){
  xlines = x |>
    str_split("\n")|>
    unlist()
  
  if(drop_bottom > 0L){
    tlines = length(xlines)
    xlines = xlines |>
      extract(-((tlines - drop_bottom + 1L):tlines))
  }
  
  if(drop_top > 0L){
    xlines = xlines |>
      extract(-(1:drop_top))
    }
  
  xlines |>
    paste0(collapse = "\n")
}


# Add rows to a LaTeX table
LaTeX_pad_rows = function(x, n, m){
  # Detect how many rows in x
  current_rows = str_count(x, pattern = "\\\\\\\\\\n\\\\hline")

  if(current_rows == 0L){
    # Blank table
    paste(c(
      rep(
        paste(c(
          rep(
            " &",
            times = m - 1L),
          "\\\\\n\\hline\n"),
          collapse = ""),
        times = n)),
      collapse = "")
  

  } else if(current_rows |> between(1L, n - 1L)){
    # Add empty rows to achieve n rows
    # where m is number of columns  
    paste(c(
      x,
      rep(
        paste(c(
          rep(
            " &",
            times = m - 1L),
          "\\\\\n\\hline\n"),
          collapse = ""),
        times = n - current_rows)),
      collapse = "")

  } else if(current_rows >= n){
    # Return x if already meets minimum row requirement
    x
  }
}