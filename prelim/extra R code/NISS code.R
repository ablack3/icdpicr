# NISS code
# NISS is calculated as the sum of the squared three highest severity varaiables for a given person
# We need to exclude severity values of 9 in this calculation since 9 represents unknown.
df$niss <- apply(df, 1, function(row){
      # select severity variables for each row
      temp <- row[grepl("^sev_[0-9]+$", names(row))]

      # for some reason apply is converting these to char. We will convert them back to numeric.
      temp <- as.numeric(temp)

      # exclude severity values of 9 (unknown)
      temp <- temp[temp != 9]

      # Take the three highest, square them, and sum the result
      sum(temp[order(-temp)[1:3]]^2, na.rm = T) # what if there are only two. will probably cause error.
})


# Replace NISS value with 75 if maximum severity is 6.
df[df$maxais == 6, "niss"] <- 75

# Replace ISS value with 99 if maximum severity is 9.
df[df$maxais == 9, "niss"] <- 99

