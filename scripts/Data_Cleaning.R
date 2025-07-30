movies %>%
  filter(revenue <= 0) %>%
  nrow()

movies %>%
  filter(budget <= 0) %>%
  nrow()

movies %>%
  filter(budget <= 0 | revenue <= 0) %>%
  nrow()

#Realize that my previous cleaning left movies that could possible have one or the other, fix it by making it say "and"
cleaned_moviestwo <- movies %>%
  filter(budget > 0, revenue > 0) 

CMTO <- Cleaned_moviestwo

# Check this level of cleaned movies

CMTO %>%
  filter(revenue <= 0) %>%
  nrow()

CMTO %>%
  filter(budget <= 0) %>%
  nrow()

# Check for unreleased/Released
CMTO %>%
  filter(status != "Released") %>%
  nrow()

CMTO %>%
  filter(status == "Released") %>%
  nrow()

what_movies <- CMTO %>%
  filter(status != "Released") %>%
  head()

# Attempt a bigger clean

CMTOF <- CMTO %>%
  filter(status == "Released", Runtime > 0, Vote_count > 100, Vote_average >0) %>%
  head(CMTOF)

# everything came up as no Zeroes, move onto checking for blanks
CMTOF %>%
  +     summarise(across(where(is.numeric), ~sum(.x == 0, na.rm = TRUE)))

CMTOF %>%
  summarise(across(where(is.character), ~sum(.x == "", na.rm = TRUE)))

> CMTFOR <- CMFTO %>%
  +     filter(revenue >= 50000, budget >= 10000 )

save it
CMTFOR <- CMFTO %>%
      filter(revenue >= 50000, budget >= 10000 )