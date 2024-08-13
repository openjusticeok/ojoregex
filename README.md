# The `ojoRegex` package

## Overview

This package includes functions to clean our data sources. It is part of the `ojoverse`. Currently, it focuses on cleaning hand-entered charge descriptions from court / jail / etc. data.

- Analysts can use the `ojoregex::ojo_apply_regex()` function to clean hand-entered charge description data, from OSCN, OCDC, or elsewhere. It works best with OSCN data, since that's what it was primarily built for thus far.
- It also includes a table of notes on all the charges it covers. This table includes the relevant statutes, the circumstances where the charge is a felony or a misdemeanor, SQ 780 relevance, violent crime status, and other useful notes, all of which are structured such that they can easily be joined onto any dataset with the `ojo_apply_regex(.include_cats = TRUE)` function / argument.
- To make development easier, all the flags and categories / notes are stored and edited in a Google Sheet known as the [Big Ol Regex Sheet](https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?gid=0#gid=0). We don’t want our code directly relying on that connection though, so a copy is saved as a .RDS file in each version of ojoRegex.

![image](https://github.com/user-attachments/assets/9b6c307d-f7fe-4a21-a5ea-fa93370468d9)


## Methodology

The `ojoRegex` package uses **concept flags** to categorize charge descriptions into clean, consistent categories. 

- “Concept flags” are just concepts that commonly appear in charge descriptions — things like “attempted”, “within 1,000 feet”, “robbery”, “deadly weapon”, etc. — and each has an associated regex pattern to find it.
    - For example, the flag **registration** (which is used to find charges like “failure to register as a sex offender”, “driving vehicle without proper registration”, etc.) has the regex pattern `regist|\b(reg|regis)\b` . I created this flag by perusing the charges in the data, and identifying the simplest pattern that would capture all the instances of the concept. In this case, I was able to do so by matching all instances of the string “regist” (this covered all the typos I could find too), and by matching the complete word “reg” or “regis”, which were common shortenings I found in the data.
        - The word boundary \b around the abbreviated words is really important in this one; without it, this flag would get applied to anything with a “reg” in it anywhere, like “**reg**ulate” or “p**reg**nant”.
        - This one is fairly simple, but hopefully it illustrates the process of making these flags.
    - As of Aug 13, 2024, there are nearly 240 concept flags in the `ojoRegex` package.
- Analysts start by calling the `ojoRegex::ojo_apply_regex()` function on their dataset, specifying the column to be cleaned. They can also specify whether they want the notes and categories for each charge included in the result (`.include_cats = TRUE`), and whether they want all 240 concept flags (`.keep_flags = TRUE`).
    - `ojo_apply_regex()` starts by pre-cleaning the data with `regex_pre_clean()`, removing common extraneous things like “IN CONCERT WITH BILLY BOB” that can screw up the regex with false positives. The pre-cleaning steps live in [`pre_clean.R`](https://github.com/openjusticeok/ojoregex/blob/main/R/pre_clean.R).
    - Next, it checks each charge description in the dataset for all of the 200+ concept flags. This is pretty memory intensive and can take a while; it’s probably the single most inefficient part of this whole thing. Afterwards, the data have 200+ new columns with `TRUE` or `FALSE` depending on whether each concept is present.
    - Next, it uses a giant `case_when()`  call to classify each charge description into a cleaned category. It does this by **combining the flags** in meaningful ways.
        - For example, the following line defines the pattern for Grand Larceny:
        `larceny & grand & !petit & !any_drugs ~ "Larceny (Grand)",` 
        In other words, if the uncleaned description says anything about larceny, anything about the word “grand”, nothing about the word “petit”, and nothing about drugs, it’s Grand Larceny.
        - Conversely, here’s the line that defines Larceny of a CDS:
        `any_drugs & larceny ~ "Larceny of a CDS",` 
        Like the concept flags themselves, these were created by going back and forth with the data and tinkering with my definitions until the results looked the way I wanted them to look.
        - This part can be a bit tricky to work with, because ideally we want these to be mutually exclusive categories. There are a few things to remember though:
            - First, the earlier lines will take precedence over the later ones. That’s why `accessory ~ "Accessory to a Felony"` is right at the top — if it says anything about accessory, I want it to be classified as “Accessory to a Felony” regardless of what the actual felony was. You can use this to override things, like in that case, and you can specify things at the end to act as a kind of “if it hasn’t been categorized already, do this” thing (this is the case for “Conspiracy” right now, for example). You should also just generally keep it in mind as you’re working with this function.
            - Second, the hand-entered descriptions will often be ambiguous. Sometimes it will say “LARCENY (GRAND / PETIT)”, for example — how do we categorize that? For these, I’ve included extra “Other / Unspecified” categories, meaning in this case we’d classify it as “Larceny (Other / Unspecified)”.
                - This approach acknowledges the ambiguity, but leaves it up to the analyst to decide what to do about it in each specific case.
                - I am only adding these where necessary, so most charges won’t have one of these. They’re most useful in cases where you’ve got several different kinds of a charge, like grand vs. petit larceny, or Burglary I vs. Burglary II, etc.
    - Finally, it joins categories and notes onto the cleaned charges (like whether the charge is a violent crime, drug crime, property crime, etc., whether it’s one of the charges amended by SQ 780, etc.) and returns the result.
- You can see the results using [the OSCN appendix](https://openjusticeok.github.io/ojoregex/articles/oscn-appendix.html). Currently, it covers around 95% of all charge descriptions in CF / CM / TR cases from 2000 through 2023. We can keep adding new ones as needed until we have almost everything covered.
