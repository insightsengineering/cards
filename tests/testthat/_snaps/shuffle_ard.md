# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
      # A tibble: 8 x 8
        variable stat_name stat_label      statistic context  statistic_fmt_fn warning
        <fct>    <chr>     <chr>           <list>    <chr>    <list>           <list> 
      1 AGE      N         N               <int [1]> continu~ <fn>             <NULL> 
      2 AGE      mean      Mean            <dbl [1]> continu~ <fn>             <NULL> 
      3 AGE      sd        SD              <dbl [1]> continu~ <fn>             <NULL> 
      4 AGE      median    Median          <dbl [1]> continu~ <fn>             <NULL> 
      5 AGE      p25       25th Percentile <dbl [1]> continu~ <fn>             <NULL> 
      6 AGE      p75       75th Percentile <dbl [1]> continu~ <fn>             <NULL> 
      7 AGE      min       Min             <dbl [1]> continu~ <fn>             <NULL> 
      8 AGE      max       Max             <dbl [1]> continu~ <fn>             <NULL> 
      # i 1 more variable: error <list>

---

    Code
      ard_shuffled
    Output
      # A tibble: 82 x 6
         ARM     variable label         stat_name statistic context   
         <chr>   <fct>    <chr>         <chr>         <dbl> <chr>     
       1 Placebo AGE      N             N             86    continuous
       2 Placebo AGE      Mean(SD)      mean          75.2  continuous
       3 Placebo AGE      Mean(SD)      sd             8.59 continuous
       4 Placebo AGE      median        median        76    continuous
       5 Placebo AGE      p25           p25           69.2  continuous
       6 Placebo AGE      p75           p75           81.8  continuous
       7 Placebo AGE      min           min           52    continuous
       8 Placebo AGE      max           max           89    continuous
       9 Placebo AGE      Vector Length N_obs         86    missing   
      10 Placebo AGE      N Missing     N_miss         0    missing   
      # i 72 more rows

---

    Code
      ard_shuff_trim
    Output
      # A tibble: 82 x 6
         ARM     variable label         stat_name statistic context   
         <chr>   <fct>    <chr>         <chr>         <dbl> <chr>     
       1 Placebo AGE      N             N             86    continuous
       2 Placebo AGE      Mean(SD)      mean          75.2  continuous
       3 Placebo AGE      Mean(SD)      sd             8.59 continuous
       4 Placebo AGE      median        median        76    continuous
       5 Placebo AGE      p25           p25           69.2  continuous
       6 Placebo AGE      p75           p75           81.8  continuous
       7 Placebo AGE      min           min           52    continuous
       8 Placebo AGE      max           max           89    continuous
       9 Placebo AGE      Vector Length N_obs         86    missing   
      10 Placebo AGE      N Missing     N_miss         0    missing   
      # i 72 more rows

