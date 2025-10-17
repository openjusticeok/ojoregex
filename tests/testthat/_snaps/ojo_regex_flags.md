# regex flags haven't changed

    Code
      ojoregex::ojo_regex_flags
    Output
      # A tibble: 242 x 8
         flag         regex group criminal_or_civil genre word_boundary examples notes
         <chr>        <chr> <chr> <chr>             <chr> <chr>         <chr>    <chr>
       1 felony_enha~ "\\b~ <NA>  Criminal Charge   Crim~ Yes           W/AFC; ~ "Thi~
       2 maintain_ke~ "\\b~ <NA>  Criminal Charge   Crim~ Yes           keeping~ "\"M~
       3 operate      "\\b~ <NA>  Criminal Charge   Crim~ Yes           operati~ "I k~
       4 manufacture  "\\b~ <NA>  Criminal Charge   Crim~ Yes           manufac~  <NA>
       5 within_x_fe~ "[0-~ <NA>  Criminal Charge   Crim~ No            1000 fe~  <NA>
       6 school       "\\b~ <NA>  Criminal Charge   Crim~ Yes           ...with~ "usu~
       7 park         "\\b~ <NA>  Criminal Charge   Crim~ Yes           ...with~ "usu~
       8 aid_abet     "\\b~ <NA>  Criminal Charge   Crim~ Yes           aid / a~ "Doe~
       9 attempted    "at[~ <NA>  <NA>              Crim~ No            attempt~  <NA>
      10 grand        "\\b~ <NA>  <NA>              Crim~ Yes           grand, ~ "Usu~
      # i 232 more rows

