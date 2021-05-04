# the displayed number of digits after the decimal is fixed to user input

    Code
      number(1.1, 2)
    Output
      [1] 1.10

# it can be displayed in scientific notation

    Code
      number(1100, scientific = TRUE)
    Output
      [1] 1.1e+03

# leading zeroes can be trimmed

    Code
      number(0.05, leading_zero = FALSE)
    Output
      [1] .050

