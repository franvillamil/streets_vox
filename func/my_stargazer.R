# My Stargazer
my_stargazer = function(dest_file, model_list, omit, title, label, order,
  covariate.labels, notes_table, dep.var.labels,
  dep.var.labels.include = TRUE,
  add.lines=list(c("CCAA Fixed Effects", rep("\\multicolumn{1}{c}{Yes}", length(model_list))))){

  filecon = file(dest_file)
  writeLines(
    stargazer(model_list, omit = omit, title = title, label = label,
      order = order, covariate.labels = covariate.labels, notes = notes_table,
      omit.stat = c("f", "ser"),
      intercept.bottom = FALSE,
      column.sep.width = "-20pt",
      multicolumn = FALSE,
      dep.var.caption = "",
      dep.var.labels = dep.var.labels,
      dep.var.labels.include = dep.var.labels.include,
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
