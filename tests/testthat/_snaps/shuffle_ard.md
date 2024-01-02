# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
      # A tibble: 8 x 8
        variable context    stat_name stat_label    statistic statistic_fmt_fn warning
        <fct>    <chr>      <chr>     <chr>         <list>    <list>           <list> 
      1 AGE      continuous N         N             <int [1]> <int [1]>        <NULL> 
      2 AGE      continuous mean      Mean          <dbl [1]> <int [1]>        <NULL> 
      3 AGE      continuous sd        SD            <dbl [1]> <int [1]>        <NULL> 
      4 AGE      continuous median    Median        <dbl [1]> <int [1]>        <NULL> 
      5 AGE      continuous p25       25th Percent~ <dbl [1]> <int [1]>        <NULL> 
      6 AGE      continuous p75       75th Percent~ <dbl [1]> <int [1]>        <NULL> 
      7 AGE      continuous min       Min           <dbl [1]> <int [1]>        <NULL> 
      8 AGE      continuous max       Max           <dbl [1]> <int [1]>        <NULL> 
      # i 1 more variable: error <list>

---

    Code
      ard_shuffled
    Output
      # A tibble: 82 x 6
         ARM     variable label         context    stat_name statistic
         <chr>   <fct>    <chr>         <chr>      <chr>         <dbl>
       1 Placebo AGE      N             continuous N             86   
       2 Placebo AGE      Mean(SD)      continuous mean          75.2 
       3 Placebo AGE      Mean(SD)      continuous sd             8.59
       4 Placebo AGE      median        continuous median        76   
       5 Placebo AGE      p25           continuous p25           69   
       6 Placebo AGE      p75           continuous p75           82   
       7 Placebo AGE      min           continuous min           52   
       8 Placebo AGE      max           continuous max           89   
       9 Placebo AGE      Vector Length missing    N_obs         86   
      10 Placebo AGE      N Missing     missing    N_miss         0   
      # i 72 more rows

---

    Code
      ard_shuff_trim
    Output
      # A tibble: 82 x 6
         ARM     variable label         context    stat_name statistic
         <chr>   <fct>    <chr>         <chr>      <chr>         <dbl>
       1 Placebo AGE      N             continuous N             86   
       2 Placebo AGE      Mean(SD)      continuous mean          75.2 
       3 Placebo AGE      Mean(SD)      continuous sd             8.59
       4 Placebo AGE      median        continuous median        76   
       5 Placebo AGE      p25           continuous p25           69   
       6 Placebo AGE      p75           continuous p75           82   
       7 Placebo AGE      min           continuous min           52   
       8 Placebo AGE      max           continuous max           89   
       9 Placebo AGE      Vector Length missing    N_obs         86   
      10 Placebo AGE      N Missing     missing    N_miss         0   
      # i 72 more rows

