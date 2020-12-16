library(tidyverse)

#14.2.1-------------------------------------------------

#In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? 
#The paste() separates strings by spaces by default, while paste0() does not separate strings with spaces by default.
#What stringr function are they equivalent to? 
#str_c() does not separate strings with spaces by default it is closer in behavior to paste0().
#How do the functions differ in their handling of NA?
str_c("HI", NA )
#[1] "HI NA"
paste("HI", NA)
#[1] "HINA"
paste0("HI", NA)
#[1] NA

#In your own words, describe the difference between the sep and collapse arguments to str_c().
#The sep argument is the string inserted between arguments to str_c() .
#The collapse is the string used to separate any elements of the character vector into a character vector of length one.

#Use str_length() and str_sub() to extract the middle character from a string. 
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
Cel <- ceiling(L /2)
str_sub(x, Cel, Cel)
#What will you do if the string has an even number of characters?
#The choice is arbitrary .

#What does str_wrap() do? When might you want to use it?
#The function str_wrap() wraps text so that it fits within a certain width. This is useful for wrapping long strings of text to be typeset.

#What does str_trim() do? What’s the opposite of str_trim()?
#str_trim() trims the space from a string.
#The opposite is str_pad() which adds space to each side.

#Write a function that turns c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.
string_commasep <- function(x, delim = ",") 
  {
  n <- length(x)
  if (n == 0) {

  } else if (n == 1) {
    x
  } else if (n == 2) {

        str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {

        not_last <- str_c(x[seq_len(n - 1)], delim)

            last <- str_c("and", x[[n]], sep = " ")

                str_c(c(not_last, last), collapse = " ")
  }
}
string_commasep("")
#[1] ""
string_commasep("a")
#[1] "a"
string_commasep(c("a", "b"))
#[1] "a and b"
string_commasep(c("a", "b", "c"))
#[1] "a, b, and c"
string_commasep(c("a", "b", "c", "d"))
#[1] "a, b, c, and d"

#14.3.1.1--------------------------------------------------------------

#Explain why each of these strings don’t match a \: "\", "\\", "\\\".
#\:Will escape the next character .
#\\:Will escape the next character in the regular expression.
#\\\:The first two backslashes will resolve to a literal backslash in the regular expression, the third will escape the next character. 

#How would you match the sequence "'\ ?
str_view("\"'\\", "\"'\\\\", match = TRUE)

#What patterns will the regular expression \..\..\.. match? How would you represent it as a string?
#It will match any patterns that are a dot followed by any character, repeated three times.
str_view(c(".a.b.c", ".a.b", "....."),c("\\..\\..\\.."), match = TRUE)

#14.3.2.1------------------------------------------------------------------

#How would you match the literal string "$^$"?
str_view(c("$^$", "ab$^$sfas"), "^\\$\\^\\$$", match = TRUE)

#Given the corpus of common words in stringr::words, create regular expressions that find all words that:
#Start with “y”.
str_view(stringr::words, "^y", match = TRUE)
#End with “x”
str_view(stringr::words, "x$", match = TRUE)
#Are exactly three letters long.
str_view(stringr::words, "^...$", match = TRUE)
#The words that have seven letters or more:
str_view(stringr::words, ".......", match = TRUE)

#14.3.3.1-----------------------------------------------------------------------

#Create regular expressions to find all words that:
#Start with a vowel.
str_subset(stringr::words, "^[aeiou]")
#That only contain consonants. 
str_view(stringr::words, "[aeiou]", match=FALSE)
#End with ed, but not with eed.
str_subset(stringr::words, "[^e]ed$")
#End with ing or ise.
str_subset(stringr::words, "i(ng|se)$")

#Empirically verify the rule “i before e except after c”.
length(str_subset(stringr::words, "(cei|[^c]ie)"))

#Is “q” always followed by a “u”?
#In the stringr::words dataset, yes.But in the English language— no.
str_view(stringr::words, "q[^u]", match = TRUE)

#Write a regular expression that matches a word if it’s probably written in British English, not American English.
x <- c("966-123 4567", "(+966)123 4567", "+966 123 4567")
x

#14.3.4.1------------------------------------------------------------------------

#Create regular expressions to find all words that:
#Start with three consonants.
str_view(words, "^[^aeiou]{3}", match = TRUE)
#Have three or more vowels in a row.
str_view(words, "[aeiou]{3,}", match = TRUE)
#Have two or more vowel-consonant pairs in a row.
str_view(words, "([aeiou][^aeiou]){2,}", match = TRUE)

#14.3.5.1---------------------------------------------------------------------

#Describe, in words, what these expressions will match:
#(.)\1\1 > The same character appearing three times in a row.
#"(.)(.)\\2\\1" > A pair of characters followed by the same pair of characters in reversed order. 
#(..)\1 > Any two characters repeated.
#"(.).\\1.\\1" > A character followed by any character, the original character, any other character, the original character again. 
#"(.)(.)(.).*\\3\\2\\1" > Three characters followed by zero or more characters of any kind followed by the same three characters but in reverse order. 

#Construct regular expressions to match words that:
#Start and end with the same character.
str_subset(words, "^(.)((.*\\1$)|\\1?$)")
#Contain a repeated pair of letters .
str_subset("church", "([A-Za-z][A-Za-z]).*\\1")
#Contain one letter repeated in at least three places .
str_subset("eleven", "([a-z]).*\\1.*\\1")


#14.4.1.1-------------------------------------------------------------------------------

#For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.
#Find all words that start or end with x.
words[str_detect(words, "^x|x$")]
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
#Find all words that start with a vowel and end with a consonant.
words[str_detect(words, "^x|x$")]
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
#Are there any words that contain at least one of each different vowel?
pattern <-
  cross(rerun(5, c("a", "e", "i", "o", "u")),
        .filter = function(...) {
          x <- as.character(unlist(list(...)))
          length(x) != length(unique(x))
        }
  ) %>%
  map_chr(~str_c(unlist(.x), collapse = ".*")) %>%
  str_c(collapse = "|")

#What word has the highest number of vowels? What word has the highest proportion of vowels? 
vowels <- str_count(words, "[aeiou]")
words[which(vowels == max(vowels))]












