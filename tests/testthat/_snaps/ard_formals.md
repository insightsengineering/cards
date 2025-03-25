# ard_formals() works

    Code
      ard_formals(fun = mcnemar.test, arg_names = "correct")
    Message
      {cards} data frame: 1 x 3
    Output
        stat_name stat_label stat
      1   correct    correct TRUE

---

    Code
      ard_formals(fun = asNamespace("stats")[["t.test.default"]], arg_names = c("mu",
        "paired", "var.equal", "conf.level"), passed_args = list(conf.level = 0.9))
    Message
      {cards} data frame: 4 x 3
    Output
         stat_name stat_label  stat
      1         mu         mu     0
      2     paired     paired FALSE
      3  var.equal  var.equal FALSE
      4 conf.level  conf.lev…   0.9

