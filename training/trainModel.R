cleanInput = function(input)
{
  input = tolower(input)
  input = stripWhitespace(input)
  input = removeNumbers(input)
  input = removePunctuation(input)
  #input = removeWords(input, stopwords("SMART"))
  #input = stemDocument(input)
  input = strsplit(input, " ")[[1]]
  input = input[input != ""]
  return (input)
}

predictWord = function(input)
{
  pred = character()
  input = cleanInput(input)
  temp = unlist(strsplit(input, " ", fixed=TRUE))
  if (length(temp) > 2)
  {
    temp = temp[(length(temp) - 1):length(temp)]
    searchstr = paste("^", temp[(l-1)], " ", temp[l], " ", sep="")
    searchres = head(wc3[grepl(searchstr, wc3$ngram), ], 1)
    if (length(searchres$ngram) > 0)
    {
      pred = unlist(strsplit(as.character(searchres$ngram[1]), " ", fixed=TRUE))[3]
      return (pred)
    }
  }
  if (length(temp) == 2)
  {
    temp = temp[2]
    searchstr = paste("^", temp, " ", sep="")
    searchres = head(wc2[grepl(searchstr, wc2$ngram), ], 1)
    if (length(searchres$ngram) > 0)
    {
      pred = unlist(strsplit(as.character(searchres$ngram[1]), " ", fixed=TRUE))[2]
      return (pred)
    }
  }
  searchres = head(wc1, 1)
  if (length(searchres$ngram) > 0)
  {
    pred = as.character(searchres$ngram[1])
    return (pred)
  }
}

# remove non-alphanumeric characters
removeNonAlphaNumericCharacters = function(x) gsub("[^[:alnum:] ]", "", x)
oneG = function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
twoG = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
threeG = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}

library(tm)
library(RWeka)
library(markovchain)

setwd("d:/personal/coursera/exploratory analysis/")
usCorpus = VCorpus(DirSource(directory = "final/en_US/", encoding = "UTF-8"))

corpora = list()
pieces = 50
for (i in 1:pieces)
{
  corpora[[i]] = usCorpus
}

l1 = floor(length(usCorpus[[1]]$content) / pieces)
l2 = floor(length(usCorpus[[2]]$content) / pieces)
l3 = floor(length(usCorpus[[3]]$content) / pieces)
c = 0
for (i in 1:pieces)
{
  print(i)
  corpora[[i]][[1]]$content = usCorpus[[1]]$content[(1 + (c*l1)):((c*l1) + l1)]
  corpora[[i]][[2]]$content = usCorpus[[2]]$content[(1 + (c*l2)):((c*l2) + l2)]
  corpora[[i]][[3]]$content = usCorpus[[3]]$content[(1 + (c*l3)):((c*l3) + l3)]
  c = c + 1
}

rm(usCorpus)
gc()

temp = tempfile()
download.file("http://www.bannedwordlist.com/lists/swearWords.txt", temp)
profanity = read.table(temp)
unlink(temp)

for (i in 1:length(corpora))
{
  print(i)
  # remove nonalphanumeric characters
  corpora[[i]] = tm_map(corpora[[i]], content_transformer(removeNonAlphaNumericCharacters))
  # convert to lower case
  corpora[[i]] = tm_map(corpora[[i]], content_transformer(tolower))
  # eliminate extra whitespace
  corpora[[i]] = tm_map(corpora[[i]], stripWhitespace)
  # remove numbers
  corpora[[i]] = tm_map(corpora[[i]], removeNumbers)
  # remove punctuation
  corpora[[i]] = tm_map(corpora[[i]], removePunctuation)
  # remove stopwords
  # corpora[[i]] = tm_map(corpora[[i]], removeWords, stopwords("SMART"))
  # stem document
  # corpora[[i]] = tm_map(corpora[[i]], stemDocument)
  # remove profanity
  corpora[[i]] = tm_map(corpora[[i]], removeWords, as.character(profanity$V1))
}

gc()

# RWeka needs a lot of memory
options(mc.cores=1)
options( java.parameters="-Xmx12g")

dtm1List = list()
dtm2List = list()
dtm3List = list()

for (i in pieces:1)
{
  print(i)
  dtm1 = DocumentTermMatrix(corpora[[i]], control = list(tokenize = oneG))
  dtm2 = DocumentTermMatrix(corpora[[i]], control = list(tokenize = twoG))
  dtm3 = DocumentTermMatrix(corpora[[i]], control = list(tokenize = threeG))
  dtm1List[[i]] = dtm1
  dtm2List[[i]] = dtm2
  dtm3List[[i]] = dtm3
  rm(dtm1)
  rm(dtm2)
  rm(dtm3)
  gc()
}

rm(corpora)
gc()

dtm1 = c(dtm1List[[(pieces)]], dtm1List[[(pieces - 1)]])
dtm2 = c(dtm2List[[(pieces)]], dtm2List[[(pieces - 1)]])
dtm3 = c(dtm3List[[(pieces)]], dtm3List[[(pieces - 1)]])

for (i in (pieces - 2):1)
{
  print (i)
  dtm1 = c(dtm1, dtm1List[[i]])
  dtm2 = c(dtm2, dtm2List[[i]])
  dtm3 = c(dtm3, dtm3List[[i]])
  dtm1List[[i]] = NULL
  dtm2List[[i]] = NULL
  dtm3List[[i]] = NULL
  gc()
}

rm(dtm1List)
rm(dtm2List)
rm(dtm3List)

gc()

dtm1_d = removeSparseTerms(dtm1, 0.9)
dtm2_d = removeSparseTerms(dtm2, 0.9)
dtm3_d = removeSparseTerms(dtm3, 0.9)

gc()

freq1 = sort(colSums(as.matrix(dtm1_d)), decreasing = TRUE)
wc1 = data.frame(ngram=names(freq1), count=freq1)
rm(freq1)
gc()

freq2 = sort(colSums(as.matrix(dtm2_d), na.rm=TRUE), decreasing = TRUE)
wc2 = data.frame(ngram=names(freq2), count=freq2)
rm(freq2)
gc()

freq3 = sort(colSums(as.matrix(dtm3_d)), decreasing = TRUE)
wc3 = data.frame(ngram=names(freq3), count=freq3)
rm(freq3)
gc()


row.names(wc1) = NULL
row.names(wc2) = NULL
row.names(wc3) = NULL
gc()

write.csv(wc1, file="wc1.csv", row.names = FALSE, quote = FALSE)
write.csv(wc2, file="wc2.csv", row.names = FALSE, quote = FALSE)
write.csv(wc3, file="wc3.csv", row.names = FALSE, quote = FALSE)

saveRDS(wc1, file="wc1.RDS")
saveRDS(wc2, file="wc2.RDS")
saveRDS(wc3, file="wc3.RDS")
