# My Stargazer (for the glm)
my_stargazer_glm = function(dest_file, model_list, title, label, order,
  covariate.labels, notes_table,
  add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", length(model_list))))){

  filecon = file(dest_file)
  writeLines(
    stargazer(model_list, title = title, label = label,
      order = order, covariate.labels = covariate.labels, notes = notes_table,
      omit.stat = c("ll"),
      dep.var.caption = "",
      dep.var.labels.include = FALSE,
      intercept.bottom = FALSE,
      column.sep.width = "-20pt",
      multicolumn = FALSE,
      omit = "ccaa",
      font.size = "small",
      digits = 3,
      digits.extra = 0,
      star.char = c("+", "*", "**", "***"),
      star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
      notes.align = "c",
      align = TRUE,
      no.space = TRUE,
      add.lines = add.lines,
      notes.label = "",
      notes.append = FALSE),
  filecon)
  close(filecon)

}
