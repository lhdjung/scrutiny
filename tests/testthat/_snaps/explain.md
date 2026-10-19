# explanations print as expected

    Code
      grim_explain(x = 5.19, n = 28, digits_x = 2)
    Output
      GRIM test: x = 5.19, n = 28
      
      The mean was reported as 5.19, with 2 decimal places. Before rounding, it could
      have been anything from 5.185 to 5.195.
      
      The mean of 28 whole numbers is their sum divided by 28. So the sum must be
      from 5.185 * 28 = 145.18 to 5.195 * 28 = 145.46.
      
      A sum of whole numbers is itself a whole number, but there is no whole number
      in this range. The nearest ones are 145 and 146. They give 145 / 28 = 5.1786
      and 146 / 28 = 5.2143, which round to 5.18 and 5.21, not to 5.19.
      
      ==> Inconsistent
    Code
      grim_explain(x = 5.19, n = 32, digits_x = 2)
    Output
      GRIM test: x = 5.19, n = 32
      
      The mean was reported as 5.19, with 2 decimal places. Before rounding, it could
      have been anything from 5.185 to 5.195.
      
      The mean of 32 whole numbers is their sum divided by 32. So the sum must be
      from 5.185 * 32 = 165.92 to 5.195 * 32 = 166.24.
      
      A sum of whole numbers is itself a whole number, and 166 is in this range:
      166 / 32 = 5.1875, which rounds to 5.19.
      
      ==> Consistent
    Code
      grim_explain(x = 71, n = 43, digits_x = 0, percent = TRUE)
    Output
      GRIM test: x = 71, n = 43, percent = TRUE
      
      The percentage was reported as 71, with no decimal places. Before rounding, it
      could have been anything from 70.5 to 71.5.
      
      A percentage out of 43 is a count divided by 43, times 100. So the count must
      be from 70.5 / 100 * 43 = 30.315 to 71.5 / 100 * 43 = 30.745.
      
      A count is a whole number, but there is no whole number in this range. The
      nearest ones are 30 and 31. They give 30 / 43 * 100 = 69.7674 and
      31 / 43 * 100 = 72.093, which round to 70 and 72, not to 71.
      
      ==> Inconsistent
    Code
      grim_explain(x = 2.84, n = 16, digits_x = 2, items = 2, rounding = "up")
    Output
      GRIM test: x = 2.84, n = 16, items = 2, rounding = "up"
      
      The mean was reported as 2.84, with 2 decimal places. Before rounding, it could
      have been anything from 2.835 up to, but not including, 2.845.
      
      Each of the 16 scores is the average of 2 whole-number items. Their mean is
      therefore the sum of all 32 item values divided by 32, so this sum must be from
      2.835 * 32 = 90.72 up to, but not including, 2.845 * 32 = 91.04.
      
      A sum of whole numbers is itself a whole number, and 91 is in this range:
      91 / 32 = 2.8438, which rounds to 2.84.
      
      ==> Consistent
    Code
      grim_explain(x = 5.2, n = 20.5, digits_x = 1)
    Output
      GRIM test: x = 5.2, n = 20.5
      
      The test needs n to be a positive whole number, but it is 20.5.
      
      ==> Undecidable
    Code
      grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2)
    Output
      GRIMMER test: x = 5.23, sd = 2.55, n = 35
      
      The mean was reported as 5.23, with 2 decimal places. Before rounding, it could
      have been anything from 5.225 to 5.235.
      
      The mean of 35 whole numbers is their sum divided by 35. So the sum must be
      from 5.225 * 35 = 182.875 to 5.235 * 35 = 183.225.
      
      A sum of whole numbers is itself a whole number, and 183 is in this range:
      183 / 35 = 5.2286, which rounds to 5.23.
      
      The SD was reported as 2.55, with 2 decimal places. Before rounding, it could
      have been anything from 2.545 to 2.555.
      
      Together, the sum and the SD determine the sum of squares, which is what you
      get by squaring each of the 35 values and adding them up. It is
      (35 - 1) * SD^2 + sum^2 / 35. The values are whole numbers, so their squares
      are whole numbers, too, and so is the sum of squares. What's more, a whole
      number and its square are either both odd or both even. So the sum of squares
      must be odd if the sum is odd, and even if the sum is even.
      
      With a sum of 183, the sum of squares is from
      34 * 2.545^2 + 183^2 / 35 = 1177.0474 to 34 * 2.555^2 + 183^2 / 35 = 1178.7814.
      The only whole number in this range is 1178, which is even, but 183 is odd.
      
      ==> Inconsistent
    Code
      grimmer_explain(x = 5.19, sd = 2.55, n = 28, digits_x = 2, digits_sd = 2)
    Output
      GRIMMER test: x = 5.19, sd = 2.55, n = 28
      
      The mean was reported as 5.19, with 2 decimal places. Before rounding, it could
      have been anything from 5.185 to 5.195.
      
      The mean of 28 whole numbers is their sum divided by 28. So the sum must be
      from 5.185 * 28 = 145.18 to 5.195 * 28 = 145.46.
      
      A sum of whole numbers is itself a whole number, but there is no whole number
      in this range. The nearest ones are 145 and 146. They give 145 / 28 = 5.1786
      and 146 / 28 = 5.2143, which round to 5.18 and 5.21, not to 5.19. Since the
      mean alone is impossible, the SD doesn't need to be checked.
      
      ==> Inconsistent
    Code
      grimmer_explain(x = 3.1, sd = 1.3, n = 200, digits_x = 1, digits_sd = 1)
    Output
      GRIMMER test: x = 3.1, sd = 1.3, n = 200
      
      The mean was reported as 3.1, with 1 decimal place. Before rounding, it could
      have been anything from 3.05 to 3.15.
      
      The mean of 200 whole numbers is their sum divided by 200. So the sum must be
      from 3.05 * 200 = 610 to 3.15 * 200 = 630.
      
      A sum of whole numbers is itself a whole number, and the whole numbers from 610
      to 630 are in this range. For example, 620 / 200 = 3.1, which rounds to 3.1.
      
      The SD was reported as 1.3, with 1 decimal place. Before rounding, it could
      have been anything from 1.25 to 1.35.
      
      Together, the sum and the SD determine the sum of squares, which is what you
      get by squaring each of the 200 values and adding them up. It is
      (200 - 1) * SD^2 + sum^2 / 200. The values are whole numbers, so their squares
      are whole numbers, too, and so is the sum of squares. What's more, a whole
      number and its square are either both odd or both even. So the sum of squares
      must be odd if the sum is odd, and even if the sum is even.
      
      With a sum of 610, the sum of squares is from
      199 * 1.25^2 + 610^2 / 200 = 2171.4375 to
      199 * 1.35^2 + 610^2 / 200 = 2223.1775. The whole numbers from 2172 to 2223 are
      in this range, so one of them is even, like 610.
      
      ==> Consistent
    Code
      debit_explain(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)
    Output
      DEBIT test: x = 0.36, sd = 0.11, n = 20
      
      DEBIT is about binary data, where every value is 0 or 1. The mean is then the
      share of ones, and the SD follows from the number of ones and the sample size.
      
      The mean was reported as 0.36, with 2 decimal places. Before rounding, it could
      have been anything from 0.355 to 0.365.
      
      With 20 values, the mean is the number of ones divided by 20, so that number
      must be from 0.355 * 20 = 7.1 to 0.365 * 20 = 7.3. But it has to be a whole
      number, and there is none in this range. So no number of ones fits the mean,
      whatever the SD is.
      
      ==> Inconsistent
    Code
      debit_explain(x = 0.35, sd = 0.49, n = 20, digits_x = 2, digits_sd = 2)
    Output
      DEBIT test: x = 0.35, sd = 0.49, n = 20
      
      DEBIT is about binary data, where every value is 0 or 1. The mean is then the
      share of ones, and the SD follows from the number of ones and the sample size.
      
      The mean was reported as 0.35, with 2 decimal places. Before rounding, it could
      have been anything from 0.345 to 0.355.
      
      With 20 values, the mean is the number of ones divided by 20, so that number
      must be from 0.345 * 20 = 6.9 to 0.355 * 20 = 7.1. It has to be a whole number,
      so it can only be 7.
      
      The SD was reported as 0.49, with 2 decimal places. Before rounding, it could
      have been anything from 0.485 to 0.495.
      
      With k ones among 20 values, the SD is sqrt(k * (20 - k) / (20 * 19)). For
      k = 7, that is sqrt(7 * 13 / 380) = 0.4894, which is in that range.
      
      ==> Consistent
    Code
      debit_explain(x = 0.36, sd = 0.49, n = 1000, digits_x = 2, digits_sd = 2)
    Output
      DEBIT test: x = 0.36, sd = 0.49, n = 1000
      
      DEBIT is about binary data, where every value is 0 or 1. The mean is then the
      share of ones, and the SD follows from the number of ones and the sample size.
      
      The mean was reported as 0.36, with 2 decimal places. Before rounding, it could
      have been anything from 0.355 to 0.365.
      
      With 1000 values, the mean is the number of ones divided by 1000, so that
      number must be from 0.355 * 1000 = 355 to 0.365 * 1000 = 365. It has to be a
      whole number, so it can be anything from 355 to 365.
      
      The SD was reported as 0.49, with 2 decimal places. Before rounding, it could
      have been anything from 0.485 to 0.495.
      
      With k ones among 1000 values, the SD is sqrt(k * (1000 - k) / (1000 * 999)).
      Even the largest of these SDs, for k = 365, is only
      sqrt(365 * 635 / 999000) = 0.4817, which is below that range.
      
      ==> Inconsistent

# `explain_print()` prints as expected

    Code
      explain_print(explain_map(grim_map(pigs1, digits_x = 2)), 1:2)
    Output
      GRIM test: x = 7.22, n = 32
      
      The mean was reported as 7.22, with 2 decimal places. Before rounding, it could
      have been anything from 7.215 to 7.225.
      
      The mean of 32 whole numbers is their sum divided by 32. So the sum must be
      from 7.215 * 32 = 230.88 to 7.225 * 32 = 231.2.
      
      A sum of whole numbers is itself a whole number, and 231 is in this range:
      231 / 32 = 7.2188, which rounds to 7.22.
      
      ==> Consistent
      
      GRIM test: x = 4.74, n = 25
      
      The mean was reported as 4.74, with 2 decimal places. Before rounding, it could
      have been anything from 4.735 to 4.745.
      
      The mean of 25 whole numbers is their sum divided by 25. So the sum must be
      from 4.735 * 25 = 118.375 to 4.745 * 25 = 118.625.
      
      A sum of whole numbers is itself a whole number, but there is no whole number
      in this range. The nearest ones are 118 and 119. They give 118 / 25 = 4.72 and
      119 / 25 = 4.76, which round to 4.72 and 4.76, not to 4.74.
      
      ==> Inconsistent
      
    Code
      explain_print(dplyr::filter(grim_map(pigs1, digits_x = 2), n > 1000))
    Output
      No rows to explain.

