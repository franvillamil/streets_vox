ttest_to_tex = function(ttout){
  ins = paste0(round(ttout$estimate[1] * 100, 2), "\\%")
  outs = paste0(round(ttout$estimate[2] * 100, 2), "\\%")
  pv = sprintf("%0.3f", ttout$p.value)
  if(ttout$p.value < 0.05){pv = paste0(pv, "*")}
  if(ttout$p.value < 0.01){pv = paste0(pv, "*")}
  if(ttout$p.value < 0.001){pv = paste0(pv, "*")}
  diff = round(ttout$estimate[1] * 100 - ttout$estimate[2] * 100, 2)
  party = gsub("^([A-Z]*)\\d+_\\d+.*", "\\1", ttout$data.name)
  row = paste0(party, " & ", ins, " & ", outs, " & ", diff, " & ", pv, " \\\\")
  return(row)
}

tex_append = function(title, label, midlines,
  var = "Party", cat1 = "In sample", cat2 = "Out of sample"){

  preamble = c(
    "\\begin{table}[!htbp] \\centering",
    paste0("\\caption{", title, "}"),
    paste0("\\label{", label, "}"),
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\\\[-1.8ex]\\hline",
    "\\hline \\\\[-1.8ex]",
    "\\\\[-1.8ex]",
    paste0(var, " & ", cat1, " & ", cat2, " & Diff & P-value \\\\"),
    "\\hline \\\\[-1.8ex]")

  end = c("\\hline",
    "\\hline \\\\[-1.8ex]",
    "\\multicolumn{5}{c}{\\parbox[t]{0.65\\textwidth}{\\textit{Note:} * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}\\\\",
    "\\end{tabular}",
    "\\end{table}")

  return(c(preamble, midlines, end))

}
