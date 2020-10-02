library(readr) # read files

setwd('~/Documents/data_science_jhu/10_capstone')

#download source data files
url='https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
target_filename='input/coursera-swiftkey.zip'

# Download and unzip data
if (!file.exists(target_file_name)){
        download.file(url=url, destfile=target_file_name)
        unzip(zipfile=downloaded_file_name, exdir='./input')
}

url='https://raw.githubusercontent.com/jiludvik/Google-profanity-words/master/list.txt'
target_file_name='input/bad_words.txt'

if (!file.exists(target_file_name)){ download.file(url=url, destfile=target_file_name)}



