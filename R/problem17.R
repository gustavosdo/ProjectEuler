# If the numbers 1 to 5 are written out in words: one, two, three, four, five,
# then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
#
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
# words, how many letters would be used?
#
# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
# forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
# letters. The use of "and" when writing out numbers is in compliance with
# British usage.

# Converting units to words
convert_units = function(num) {
  # List of words for each of the unit numbers
  numbers = c('', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
  # Returning the corresponding number in word
  return(numbers[num+1])
}

# Convert first dozen to words
convert_first_dozen = function(num) {
  # List of words for each of the unit numbers
  numbers = c('ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen')
  # Returning the corresponding number in word
  return(numbers[num - 9])
}

# Converting all dozens to words
convert_dozens = function(num) {
  # Converting first dozen
  if (num/20 < 1) {return(convert_first_dozen(num))}
  # List of words for each dozen (above 10)
  numbers = c('twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')
  # Get the leading number of num (the dozen)
  dozen = as.numeric(unlist(strsplit(as.character(num), split = ""))[1])
  # Get the last number of num (the unit)
  unit = as.numeric(unlist(strsplit(as.character(num), split = ""))[2])
  if (unit > 0) {
    return(paste0(numbers[dozen-1], convert_units(unit)))
  } else {
    return(numbers[dozen-1])
  }
}

# Converting all hundreds to words
convert_hundreds = function(num) {
  # List of words for each of the hundred numbers
  numbers = c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
  # Get the leading number of num (the hundred)
  hundred = as.numeric(unlist(strsplit(as.character(num), split = ""))[1])
  # Retrieve the dozen and the unit
  rest = as.numeric(paste0(unlist(strsplit(as.character(num), split = ""))[2:3], collapse = ""))
  # Converting dozen and unit to words
  rest = ifelse(test = rest >= 10, yes = convert_dozens(rest), no = convert_units(rest))
  compliance = ifelse(test = (rest != ""), yes = "hundredand", no = "hundred")
  return(paste0(numbers[hundred], compliance, rest))
}

# This function converts an integer number (from 1 to 1000) to words
# (without hyphen and space)
convert_to_words = function(num) {
  if (num/1000 == 1) {
    return("onethousand")
  } else if (num/100 >= 1) {
    return(convert_hundreds(num))
  } else if (num/10 >= 1) {
    return(convert_dozens(num))
  } else {
    return(convert_units(num))
  }
}

char_count = 0
for (num in 1:1000) {
  #print(paste(num, convert_to_words(num)))
  char_count = char_count + length(unlist(strsplit(x = convert_to_words(num), split = "")))
}

print(char_count)