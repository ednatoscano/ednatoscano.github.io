# -*- coding: utf-8 -*-
"""
Created on Mon Oct  3 10:25:38 2022

@author: ednat
"""

#%% Get the content of a page using the requests library
import requests
from os.path import basename
from os.path import join
from os import makedirs
from urllib.request import urlopen
from urllib.parse import urljoin
from concurrent.futures import ThreadPoolExecutor
from concurrent.futures import as_completed

mywebpage_url='https://www.gutenberg.org/'
mywebpage_html=requests.get(mywebpage_url)

#%% Parse the HTML content using beautifulsoup
import bs4
mywebpage_parsed=bs4.BeautifulSoup(mywebpage_html.content,'html.parser')

#%% Get the text content of the page
def getpagetext(parsedpage):
    # Remove HTML elements that are scripts
    scriptelements=parsedpage.find_all('script')
    # Concatenate the text content from all table cells
    for scriptelement in scriptelements:
        # Extract this script element from the page.
        # This changes the page given to this function!
        scriptelement.extract()
    pagetext=parsedpage.get_text()
    return(pagetext)
mywebpage_text=getpagetext(mywebpage_parsed)

#%% Find linked pages in Gutenberg website
def getpageurls(webpage_parsed):
    # Find elements that are hyperlinks
    pagelinkelements=webpage_parsed.find_all('a')
    pageurls=[];
    for pagelink in pagelinkelements:  
        pageurl_isok=1
        try:
            pageurl=pagelink['href']
        except:
            pageurl_isok=0
        if pageurl_isok==1: 
            #If the word "top" or "ebook" is found 
            if (pageurl.find('top')!=-1) | (pageurl.find('ebook')!=-1): 
                #Check if it doesn't have http in url, then add http url
                if (pageurl.find('http')==-1): 
                    pageurl = 'https://www.gutenberg.org' + pageurl 
                    pageurl_isok=1 # add it
            # If the word i not found, don't add it
            if (pageurl.find('http')==-1): 
                pageurl_isok=0            
        if pageurl_isok==1:
            pageurls.append(pageurl)
    return(pageurls)
mywebpage_urls=getpageurls(mywebpage_parsed)
print(mywebpage_urls)

top_url = ""
for page in mywebpage_urls:
    if page.find("top")!=-1: #If it finds it
        top_url=page
print(top_url) # https://www.gutenberg.org/browse/scores/top


#%% Basic web crawler
def basicwebcrawler(seedpage_url,maxpages):
    # Store URLs crawled and their text content
    num_pages_crawled=0
    crawled_urls=[]
    crawled_texts=[]
    # Remaining pages to crawl: start from a seed page URL
    pagestocrawl=[seedpage_url]
    # Process remaining pages until a desired number
    # of pages have been found
    while (num_pages_crawled<maxpages)&(len(pagestocrawl)>0):
        # Retrieve the topmost remaining page and parse it
        pagetocrawl_url=pagestocrawl[0]
        print('Getting page:')
        print(pagetocrawl_url)
        pagetocrawl_html=requests.get(pagetocrawl_url)
        pagetocrawl_parsed=bs4.BeautifulSoup(pagetocrawl_html.content,
                                             'html.parser')
        # Get the text and URLs of the page
        pagetocrawl_text=getpagetext(pagetocrawl_parsed)
        pagetocrawl_urls=getpageurls(pagetocrawl_parsed)
        # Store the URL and content of the processed page
        num_pages_crawled=num_pages_crawled+1
        crawled_urls.append(pagetocrawl_url)
        crawled_texts.append(pagetocrawl_text)
        # Remove the processed page from remaining pages,
        # but add the new URLs
        pagestocrawl=pagestocrawl[1:len(pagestocrawl)]
        pagestocrawl.extend(pagetocrawl_urls)
    return(crawled_urls,crawled_texts)

mycrawled_urls_and_texts=basicwebcrawler(top_url,300) 
mycrawled_urls=mycrawled_urls_and_texts[0]
mycrawled_texts=mycrawled_urls_and_texts[1]

#len(mycrawled_texts)
#len(mycrawled_urls)

# Remove empty spaces from my crawled texts
crawled_text_1=''.join(mycrawled_texts)
crawled_text_2 = crawled_text_1.split("\n")

for text in crawled_text_2:
    if text == ' '   or text == '':
        crawled_text_2.remove(text)
        
import re   
crawled_text2=[]     
for text in crawled_text_2: 
    crawled_text2.append(re.sub(' +', ' ', text))
    
print(crawled_text2)

# Find the indexe of the top 100 Ebooks to find the top 20 links
def find_index(list_of_text, string):
    index = []
    for i in range(0,len(list_of_text)):
        if list_of_text[i].find(string)!=-1:
            index.append(i)
    return index

index = find_index(crawled_text2, "Top 100 EBooks last 30 days")
title_index=index[5]

top20book_titles=[]
for i in range(1,21):
    top20book_titles.append(crawled_text2[title_index+i])
    
len(top20book_titles)
for i in range(0,len(top20book_titles)):
    print(i+1, top20book_titles[i])

#%% Remove the parenthesis from the book title texts
def remove_elements_from_strings(list_of_strings, separator):
    index_list=[]
    new_list=[]
    for string in list_of_strings:
        for i in range(0,len(string)):
            if string[i] == separator:
                index_list.append(i)
                
    for i in range(0,20):
        title=list_of_strings[i]
        index=index_list[i]
        new_list.append(title[0:index-1])
    return new_list

# Remove the number in parenthesis
top20book_titles_no_parenthesis=remove_elements_from_strings(top20book_titles, 
                                                             '(')

# Find the top 20 links
top20_links=[]
for element in range(0,20):
    top20_links.append('')
    
for i in range(0,20):
    for j in range(0,300):
        crawled_title=mycrawled_texts[j] #1,2,...
        book_title = top20book_titles_no_parenthesis[i] # 0,0,0,...       
        # convert the text        
        test1=''.join(crawled_title)
        test2 = test1.split("\n")
        for text in test2:
            if text == ' ' or text == '':
                test2.remove(text)
        if test2[0].find(book_title)!=-1: # if the book title is found
            top20_links[i]=mycrawled_urls[j]
            break

# We get the top 20 most downloaded books URLS
for i in range(0,len(top20_links)):
    print(i+1, top20_links[i])        

#%% 2.2 Using the crawler, download the top-20 ebooks (k=20). Report the names 
#       and addresses of the books
#DOWNLOAD BOOKS
import bs4
import os

# Extract the book ID
def get_book_id(list_of_strings, separator):
    new_list=[]
    length_sep=len(separator)   
    for i in range(0,len(list_of_strings)):
        index= list_of_strings[i].find(separator)
    for i in range(0,20):
        title=list_of_strings[i]
        length_string=len(title)
        new_list.append(title[index+length_sep:length_string])
    return new_list

book_id_list=[]
book_id_list = get_book_id(top20_links, 'ebooks/')
for i in range(0,len(book_id_list)):
    print(i+1, book_id_list[i]) 

# # Extract the URLs
# def get_urls_from_html(content):
#     # decode the provided content as ascii text
#     html = content.decode('utf-8')
#     requests.get(mywebpage_url)
#     # parse the document as best we can
#     soup = bs4.BeautifulSoup(html, 'html.parser')
#     # find all all of the <a href=""> tags in the document
#     atags = soup.find_all('a')
#     # get all links from a tags
#     return [tag.get('href') for tag in atags]
    
# Download html and return parsed doc or None on error
def download_url(urlpath):
    try:
        # open a connection to the server
        with urlopen(urlpath, timeout=5) as connection:
            # read the contents of the html doc
            return connection.read()
    except:
        # bad url, socket timeout, http forbidden, etc.
        return None

# Download one book from project gutenberg
def download_book(book_id, save_path):
    # construct the download url
    url = f'https://www.gutenberg.org/files/{book_id}/{book_id}-0.txt'
    # download the content
    data = download_url(url)
    if data is None:
        return f'Failed to download {url}'
    # create local path
    save_file = join(save_path, f'{book_id}.txt')
    # save book to file
    with open(save_file, 'wb') as file:
        file.write(data)
    return f'Success to download {url}'

for book in book_id_list:
    print(download_book(book, 'C:\\Users\ednat\Documents\project_gutenberg'))

for i in range(0,len(top20_links)):
    print(i+1, top20_links[i]) 
for i in range(0,len(top20book_titles_no_parenthesis)):
    print(i+1, top20book_titles_no_parenthesis[i]) 

#%% REMOVE THE HEADER AND FOOTER IN BOOK TEXTS
header1 = '*** START OF THIS PROJECT GUTENBERG'
header2 = '*** START OF THE PROJECT GUTENBERG'
footer1 = '*** END OF THIS PROJECT GUTENBERG'
footer2 = '*** END OF THE PROJECT GUTENBERG'
directory_path= "C:/Users/ednat/Documents/project_gutenberg/"

def open_file(filename):
    try:
        file = open(filename, mode="r", encoding='utf-8')
    except OSError:
        print("There was an error in reading the file.")
        return
    
    sentence_list = []
    mybookcontent = ''
    counter = 0
    header_index = 0
    footer_index = 0
    sentence = ''

    for line in file:
        sentence = line.replace("\n", "") #taking out the line breaks
        # skipping the empty lines.
        if sentence == "":
            continue
        # if header is found, save the index
        if (sentence.find(header1,0,-1)!=-1)|(sentence.find(header2,0,-1)!=-1): 
            header_index = counter     
        # if footer is found, save the index
        elif(sentence.find(footer1,0,-1)!=-1)|(sentence.find(footer2,0,-1)!=-1): 
            footer_index = counter  
        sentence_list.append(sentence) 
        counter += 1
        
    sentence_list = sentence_list[header_index+1:footer_index] 
    mybookcontent=' '.join(sentence_list)
    file.close()
    
    return mybookcontent

# Book contents in a list without Header or Footer
list_of_books_content = []
for i in range(0,20):
    book_id = book_id_list[i]
    dir_file = join(directory_path, f'{book_id}.txt')
    book = open_file(dir_file)
    list_of_books_content.append(book)

#%% (c) Use the processing pipeline described on the lecture to tokenize and lemmatize the downloaded books    
#Tokenize loaded texts and change them to NLTK  format

import nltk
nltk.download('punkt')

# Remove the item from the list of books that were not downloaded
list_of_books_content[3]
mycrawled_texts_books = []
for book in list_of_books_content:
    if book is not None:
        book_no_spaces=' '.join(book.split())
        mycrawled_texts_books.append(book_no_spaces)
        
len(list_of_books_content)
len(mycrawled_texts_books)

mycrawled_nltktexts=[]
for k in range(len(mycrawled_texts_books)):    
    temp_tokenizedtext=nltk.word_tokenize(mycrawled_texts_books[k])
    temp_nltktext=nltk.Text(temp_tokenizedtext)
    mycrawled_nltktexts.append(temp_nltktext)

#%% Make all crawled texts lowercase
mycrawled_lowercasetexts=[]
for k in range(len(mycrawled_nltktexts)):    
    temp_lowercasetext=[]
    for l in range(len(mycrawled_nltktexts[k])):
        lowercaseword=mycrawled_nltktexts[k][l].lower()
        temp_lowercasetext.append(lowercaseword)
    temp_lowercasetest=nltk.Text(temp_lowercasetext)
    mycrawled_lowercasetexts.append(temp_lowercasetext)

mycrawled_lowercasetexts[0]
len(mycrawled_lowercasetexts)

#%% Stem the loaded texts
stemmer=nltk.stem.porter.PorterStemmer()    
def stemtext(nltktexttostem):
    stemmedtext=[]
    for l in range(len(nltktexttostem)):
        # Stem the word
        wordtostem=nltktexttostem[l]
        stemmedword=stemmer.stem(wordtostem)
        # Store the stemmed word
        stemmedtext.append(stemmedword)
    return(stemmedtext)

mycrawled_stemmedtexts=[]
for k in range(len(mycrawled_lowercasetexts)):    
    temp_stemmedtext=stemtext(mycrawled_lowercasetexts[k])
    temp_stemmedtext=nltk.Text(temp_stemmedtext)
    mycrawled_stemmedtexts.append(temp_stemmedtext)

#%% Convert a POS tag for WordNet
def tagtowordnet(postag):
    wordnettag=-1
    if postag[0]=='N':
        wordnettag='n'
    elif postag[0]=='V':
        wordnettag='v'
    elif postag[0]=='J':
        wordnettag='a'
    elif postag[0]=='R':
        wordnettag='r'
    return(wordnettag)

#%% POS tag and lemmatize the loaded texts
# Download tagger and wordnet resources if you do not have them already
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')

lemmatizer=nltk.stem.WordNetLemmatizer()

def lemmatizetext(nltktexttolemmatize):
    # Tag the text with POS tags
    taggedtext=nltk.pos_tag(nltktexttolemmatize)
    # Lemmatize each word text
    lemmatizedtext=[]
    for l in range(len(taggedtext)):
        # Lemmatize a word using the WordNet converted POS tag
        wordtolemmatize=taggedtext[l][0]
        wordnettag=tagtowordnet(taggedtext[l][1])
        if wordnettag!=-1:
            lemmatizedword=lemmatizer.lemmatize(wordtolemmatize,wordnettag)
        else:
            lemmatizedword=wordtolemmatize
        # Store the lemmatized word
        lemmatizedtext.append(lemmatizedword)
    return(lemmatizedtext)

mycrawled_lemmatizedtexts=[]
for k in range(len(mycrawled_lowercasetexts)):
    lemmatizedtext=lemmatizetext(mycrawled_lowercasetexts[k])
    lemmatizedtext=nltk.Text(lemmatizedtext)
    mycrawled_lemmatizedtexts.append(lemmatizedtext)

mycrawled_lemmatizedtexts[1]
    
#%% Find the vocabulary, in a distributed fashion
import numpy
myvocabularies=[]
myindices_in_vocabularies=[]
# Find the vocabulary of each document
for k in range(len(mycrawled_lemmatizedtexts)):
    # Get unique words and where they occur
    temptext=mycrawled_lemmatizedtexts[k]
    uniqueresults=numpy.unique(temptext,return_inverse=True)
    uniquewords=uniqueresults[0]
    wordindices=uniqueresults[1]
    # Store the vocabulary and indices of document words in it
    myvocabularies.append(uniquewords)
    myindices_in_vocabularies.append(wordindices)
    
len(myvocabularies)
myvocabularies[0]
myindices_in_vocabularies[0]

#%% (d) Create a unified vocabulary from the ebooks; report the top-100 words.

# Unify the vocabularies.
# First concatenate all vocabularies
tempvocabulary=[]  
for k in range(len(mycrawled_lemmatizedtexts)):
    tempvocabulary.extend(myvocabularies[k])
    
# Find the unique elements among all vocabularies
uniqueresults=numpy.unique(tempvocabulary,return_inverse=True)
unifiedvocabulary=uniqueresults[0]
wordindices=uniqueresults[1]

len(unifiedvocabulary)
unifiedvocabulary[0]
wordindices[0]

# Translate previous indices to the unified vocabulary. Must keep track where 
# each vocabulary started in the concatenated one.
vocabularystart=0
myindices_in_unifiedvocabulary=[]
for k in range(len(mycrawled_lemmatizedtexts)):
    # In order to shift word indices, we must temporarily
    # change their data type to a Numpy array
    tempindices=numpy.array(myindices_in_vocabularies[k])
    tempindices=tempindices+vocabularystart
    tempindices=wordindices[tempindices]
    myindices_in_unifiedvocabulary.append(tempindices)
    vocabularystart=vocabularystart+len(myvocabularies[k])

len(myindices_in_unifiedvocabulary)
myindices_in_unifiedvocabulary[0]    
# A piece or the unified vocabulary (words 1000-1050):    
unifiedvocabulary[1000:1050]

#Documents can be represented as a vector of indices to the unified vocabulary. 
# Words 600-650 words in document 1:
#myindices_in_unifiedvocabulary[1][600:650]
#unifiedvocabulary[myindices_in_unifiedvocabulary[1][600:650]]

#%% Count the numbers of occurrences of each unique word
# Let's count also various statistics over the documents
unifiedvocabulary_totaloccurrencecounts=numpy.zeros((len(unifiedvocabulary),1))
unifiedvocabulary_documentcounts=numpy.zeros((len(unifiedvocabulary),1))
unifiedvocabulary_meancounts=numpy.zeros((len(unifiedvocabulary),1))
unifiedvocabulary_countvariances=numpy.zeros((len(unifiedvocabulary),1))

# First pass: count occurrences
for k in range(len(mycrawled_lemmatizedtexts)):
    print(k)
    occurrencecounts=numpy.zeros((len(unifiedvocabulary),1))
    for l in range(len(myindices_in_unifiedvocabulary[k])):
        occurrencecounts[myindices_in_unifiedvocabulary[k][l]]= \
            occurrencecounts[myindices_in_unifiedvocabulary[k][l]]+1
        unifiedvocabulary_totaloccurrencecounts= \
            unifiedvocabulary_totaloccurrencecounts+occurrencecounts
    unifiedvocabulary_documentcounts= unifiedvocabulary_documentcounts + \
        (occurrencecounts>0)

# Mean occurrence counts over documents
unifiedvocabulary_meancounts= unifiedvocabulary_totaloccurrencecounts/ \
    len(mycrawled_lemmatizedtexts)

# Second pass to count variances
for k in range(len(mycrawled_lemmatizedtexts)):
    print(k)
    occurrencecounts=numpy.zeros((len(unifiedvocabulary),1))
    for l in range(len(myindices_in_unifiedvocabulary[k])):
        occurrencecounts[myindices_in_unifiedvocabulary[k][l]]= \
            occurrencecounts[myindices_in_unifiedvocabulary[k][l]]+1
    unifiedvocabulary_countvariances=unifiedvocabulary_countvariances+ \
        (occurrencecounts-unifiedvocabulary_meancounts)**2
unifiedvocabulary_countvariances=unifiedvocabulary_countvariances/ \
    (len(mycrawled_lemmatizedtexts)-1) 

#Resulting words can be inspected: sort words by each statistic, print words 
# with highest value of the statistic
#%% Inspect frequent words
# Sort words by largest total (or mean) occurrence count
highest_totaloccurrences_indices=\
    numpy.argsort(-1*unifiedvocabulary_totaloccurrencecounts,axis=0)
print(numpy.squeeze(unifiedvocabulary[highest_totaloccurrences_indices[1:100]]))
print(numpy.squeeze(unifiedvocabulary_totaloccurrencecounts[highest_totaloccurrences_indices[1:100]]))

word_frequence = unifiedvocabulary[highest_totaloccurrences_indices[1:100]]

# Sort words by largest total document count
highest_documentoccurrences_indices=\
    numpy.argsort(-1*unifiedvocabulary_documentcounts,axis=0)
print(numpy.squeeze(unifiedvocabulary[highest_documentoccurrences_indices[1:100]]))
print(numpy.squeeze(unifiedvocabulary_documentcounts[highest_documentoccurrences_indices[1:100]]))

# Sort by largest variance of count over documents
highest_countvariances_indices=\
    numpy.argsort(-1*unifiedvocabulary_countvariances,axis=0)
print(numpy.squeeze(unifiedvocabulary[highest_countvariances_indices[1:100]]))
print(numpy.squeeze(unifiedvocabulary_countvariances[highest_countvariances_indices[1:100]]))

#%%############################################################################
# Exercise 2.3: Zipf's law.
# Use the top-20 books from Project Gutenberg to examine whether Zipf's law 
# holds here too.

# (a) Compute a plot of the frequencies of all words in the vocabulary (= count
# of each word, divided by the total count of all words together), sorted by 
# frequency. Report the plot.

# Make a frequency plot of the words
# Import the plotting library
import matplotlib
import matplotlib.pyplot
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
# Tell the library we want each plot in its own window
%matplotlib auto

# Create a figure and an axis
myfigure, myaxes = matplotlib.pyplot.subplots();

# Plot the sorted occurrence counts of the words against their ranks
horizontalpositions=range(len(unifiedvocabulary))
verticalpositions=\
    numpy.squeeze(unifiedvocabulary_totaloccurrencecounts[highest_totaloccurrences_indices])
myaxes.plot(horizontalpositions,verticalpositions, 'tab:orange');
myaxes.set_title("All words against their ranks")
myaxes.set(xlabel="ranks", ylabel="counts")

# Plot the top-100 occurrence counts of the words against their ranks
myfigure, myaxes = matplotlib.pyplot.subplots();
horizontalpositions=range(100)
verticalpositions=\
    numpy.squeeze(unifiedvocabulary_totaloccurrencecounts[highest_totaloccurrences_indices[0:100]])
myaxes.plot(horizontalpositions,verticalpositions, 'tab:orange');
myaxes.set_title("Top 100 occurrence words against their ranks")
myaxes.set(xlabel="ranks", ylabel="counts")

#%% (b) Then plot the frequency distribution according to Zipf's law (slide 36 of lecture 2) in the
# same plot as the real frequencies. Try several choices of the exponent a, and try to find one
# that in your subjective opinion fits best. Report the plots for different choices of a and your
# choice for best a.

import numpy as np
all_words = ''
words_list = []
for i in range(0,len(myindices_in_unifiedvocabulary)):
    words_nparray = unifiedvocabulary[myindices_in_unifiedvocabulary[i]]
    words_list += words_nparray.tolist() # for each book

words_list[1000:1100]

all_words_str = ''
for i in range(0, len(words_list)):
    all_words_str += words_list[i] + ' '
    
all_words_str = all_words_str[0: len(all_words_str)-1] 
all_words_str[0:500]

# Lower text
def lowerCase(text):
    return text.lower()

# Numbers removal
def numberRemoval(text):
    return re.sub(pattern = '\d', repl = ' ', string = text)

# Non-alphanumeric removal
def nonAlphanumericRemoval(text):
    return re.sub(pattern = '[^a-zA-Z\d]', repl = ' ', string = text)

# Whitespaces removal
def whitespacesRemoval(text):
    # Returns string with leading whitespaces removed
    x = str(text).lstrip()
    # Returns string with trailing whitespaces removed
    x = str(x).rstrip()
    # Substitute multiple whitespaces with single whitespace
    x = re.sub(pattern = ' +',repl = ' ',string = str(x))
    return x

# Compile text preprocessing into one function
def textPreprocessing(text):
    x = lowerCase(text)
    x = numberRemoval(x)
    x = nonAlphanumericRemoval(x)
    x = whitespacesRemoval(x)
    return x
  
# Perform simple text preprocessing
text_clean = textPreprocessing(all_words_str)

# How to get a list of top words
import collections
import pandas as pd

def getTopWords(text: str):
    # Split text by its whitespace
    list_words = text.split()
    # Count the word frequencies
    word_freq = collections.Counter(list_words)
    # Get top n words that have highest frequencies
    top_words = word_freq.most_common()
    return top_words
  
top_words = getTopWords(text_clean)
top_words[0:99]

# Highest frequency
max_freq = top_words[0][1]

# Total count of words in all books
total_freq = 0
for i in range(0,len(top_words)):
    total_freq += top_words[i][1]
total_freq   
        
# Alpha values
alpha_list = [1, 1.2, 1.5, 2]

# Pre-calculate the summation in zipf's law equation
summation=0
summation_list=[]
for alpha in alpha_list:
    for n in range(1, len(top_words)+1):
        summation += (1/n**alpha)
    summation_list.append(summation)
    
# List of data
list_data = []

for index, item in enumerate(iterable = top_words, start = 1):
    # New data
    j_data = {
        'rank': index,
        'word': item[0],
        'actual_freq': item[1],
        'rank_freq': item[1]/total_freq,
        'relative_freq': '1/{}'.format(index),
        'zipf_freq_alpha=1': (1/(index ** alpha_list[0]))/(summation_list[0]),
        'zipf_freq_alpha=1.2':(1/(index ** alpha_list[1]))/(summation_list[1]),
        'zipf_freq_alpha=1.5':(1/(index ** alpha_list[2]))/(summation_list[2]),
        'zipf_freq_alpha=2': (1/(index ** alpha_list[3]))/(summation_list[3])  
    }
    # Append new data
    list_data.append(j_data)

# Convert list of dictionary into data frame
df_words = pd.DataFrame(data = list_data)

# Show the data
df_words.head()

#%% Data visualization
import plotnine
from plotnine import *
import pylab as plt

plotnine.options.figure_size = (10, 4.8)
(
    ggplot(data = df_words[:100] )+
    geom_line(aes(x='rank',y='rank_freq',group=1,color="'red'"),size=1.5)+
    geom_line(aes(x='rank',y='zipf_freq_alpha=1',\
                  group=1,color="'blue'"),size=0.8)+
    geom_line(aes(x='rank',y='zipf_freq_alpha=1.2',\
                  group=1,color="'green'"),size=0.8)+
    geom_line(aes(x='rank',y='zipf_freq_alpha=1.5',\
                  group=1,color="'orange'"),size=0.8)+
    geom_line(aes(x='rank',y='zipf_freq_alpha=2',\
                  group=1,color="'purple'"),size=0.8)+
    labs(title = 'Rank-frequency Distribution vs Zipf Distribution')+
    xlab(xlab = 'rank(w)')+
    ylab( ylab = 'p(w)') +
    scale_color_identity(guide='legend',name=' ', 
                         breaks=['red','blue','green', 'orange', 'purple'],
                        labels=['real data','zipf(a=1)','zipf(a=1.2)', 
                                'zipf(a=1.5)', 'zipf(a=2)'])
)

 
#%% Exercise 2.4: Vocabulary pruning for Project Gutenberg books.
# Prune the vocabulary to remove stopwords, overly short and long words, the 
# top 1% most frequent words and words occurring less than 4 times. 
# Report the top-100 words after pruning.

nltk.download('stopwords')
print(nltk.corpus.stopwords.words('english'))

# Vocabulary pruning
nltkstopwords=nltk.corpus.stopwords.words('english')
pruningdecisions=numpy.zeros((len(unifiedvocabulary),1))

# Obtain the word frequency of the vocabulary of all the books together.
temp_vocabulary=[]
all_vocabulary=[]

# Getting the vocabulary of each book
for k in range(len(mycrawled_lemmatizedtexts)):
    temp_vocabulary.extend(mycrawled_lemmatizedtexts[k])
    all_vocabulary.append(temp_vocabulary)

# Unifying each book's vocabulary into ona and obtaining the frequency of each word.
all_vocab_list = []
for each in all_vocabulary:
    for word in each:
        all_vocab_list.append(word)
      
freq = nltk.FreqDist(all_vocab_list)

for k in range(len(unifiedvocabulary)):
    # Rule 1: check the nltk stop word list
    if (unifiedvocabulary[k] in nltkstopwords):
        pruningdecisions[k]=1
    # Rule 2: if the word is in the top 1% of frequent words
    if (k in highest_totaloccurrences_indices[\
        0:int(numpy.floor(len(unifiedvocabulary)*0.01))]):
        pruningdecisions[k]=1
    # # Rule 3: if the word is in the bottom 65% of frequent words
    # if (k in highest_totaloccurrences_indices[(int(numpy.floor(\
    #     len(unifiedvocabulary)*0.35))):len(unifiedvocabulary)]):
        pruningdecisions[k]=1
    # Rule 4: if the word is too short
    if len(unifiedvocabulary[k])<2:
        pruningdecisions[k]=1
    # Rule 5: if the word is too long
    if len(unifiedvocabulary[k])>20:
        pruningdecisions[k]=1
    # Rule 6: if the word has unwanted characters
    # (here for simplicity only a-z allowed)
    if unifiedvocabulary[k].isalpha()==False:
        pruningdecisions[k]=1
    # Rule 7: if the word is occurring less than 4 times
    if freq[unifiedvocabulary[k]] < 4:
        pruningdecisions[k]=1


#%% Get indices of documents to remaining words
oldtopruned=[]
tempind=-1
for k in range(len(unifiedvocabulary)):
    if pruningdecisions[k]==0:
        tempind=tempind+1
        oldtopruned.append(tempind)
    else:
        oldtopruned.append(-1)
        
#%% Create pruned texts
mycrawled_prunedtexts=[]
myindices_in_prunedvocabulary=[]
for k in range(len(mycrawled_lemmatizedtexts)):
    print(k)
    temp_newindices=[]
    temp_newdoc=[]
    for l in range(len(mycrawled_lemmatizedtexts[k])):
        temp_oldindex=myindices_in_unifiedvocabulary[k][l]            
        temp_newindex=oldtopruned[temp_oldindex]
        if temp_newindex!=-1:
            temp_newindices.append(temp_newindex)
            temp_newdoc.append(unifiedvocabulary[temp_oldindex])
    mycrawled_prunedtexts.append(temp_newdoc)
    myindices_in_prunedvocabulary.append(temp_newindices)

#%% Inspect remaining frequent words
# Sort remaining words by largest total (or mean) occurrence count
remainingindices=numpy.squeeze(numpy.where(pruningdecisions==0)[0])
remainingvocabulary=unifiedvocabulary[remainingindices]
remainingvocabulary_totaloccurrencecounts= \
    unifiedvocabulary_totaloccurrencecounts[remainingindices]
remaining_highest_totaloccurrences_indices= \
    numpy.argsort(-1*remainingvocabulary_totaloccurrencecounts,axis=0)
print(numpy.squeeze(remainingvocabulary[remaining_highest_totaloccurrences_indices[1:500]]))
print(numpy.squeeze(remainingvocabulary_totaloccurrencecounts[ \
    remaining_highest_totaloccurrences_indices[1:500]]))

print(numpy.squeeze(remainingvocabulary[remaining_highest_totaloccurrences_indices[1:100]]))
