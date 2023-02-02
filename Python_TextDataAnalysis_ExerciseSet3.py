# -*- coding: utf-8 -*-
"""
Created on Mon Oct 10 11:39:56 2022

@author: ednat
"""

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


# Get indices of documents to remaining words
oldtopruned=[]
tempind=-1
for k in range(len(unifiedvocabulary)):
    if pruningdecisions[k]==0:
        tempind=tempind+1
        oldtopruned.append(tempind)
    else:
        oldtopruned.append(-1)
        
# Create pruned texts
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

# Inspect remaining frequent words
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

#%% Exercise 3.1: Lexical dispersion in Pride and Prejudice.
# (a) Download the Project Gutenberg .TXT ebook of Jane Austen's "Pride and 
# Prejudice", and process it using the pipeline as in Exercise 2.2 (a)-(d), 
# no need to prune the vocabulary. 
# (See # https://en.wikipedia.org/wiki/Pride_and_Prejudice for information about the work.)

import requests
import bs4
# from os.path import basename
from os.path import join
# from os import makedirs
from urllib.request import urlopen
# from urllib.parse import urljoin
# from concurrent.futures import ThreadPoolExecutor
# from concurrent.futures import as_completed

mywebpage_url='https://www.gutenberg.org/'
mywebpage_html=requests.get(mywebpage_url)
mywebpage_parsed=bs4.BeautifulSoup(mywebpage_html.content,'html.parser')

# Get the text content of the page
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

# Find linked pages in Gutenberg website
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

# Basic web crawler
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
        
        # # Check if the url is not already in the list
        # if pagetocrawl_url not in crawled_urls:
        #     # Get the text and URLs of the page
        #     pagetocrawl_text=getpagetext(pagetocrawl_parsed)
        #     pagetocrawl_urls=getpageurls(pagetocrawl_parsed)
            
        #     # Store the URL and content of the processed page
        #     num_pages_crawled=num_pages_crawled+1
        #     crawled_urls.append(pagetocrawl_url)
        #     crawled_texts.append(pagetocrawl_text
                                 
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

# Find the ebook link of "Pride and Prejudice"
def find_index(list_of_text, string):
    index = []
    for i in range(0,len(list_of_text)):
        if list_of_text[i].find(string)!=-1:
            index.append(i)
    return index

# Find the crawled text containing the book Pride and Prejudice
indexes = find_index(mycrawled_texts, "Pride and Prejudice")
pride_index=indexes[2]
mycrawled_texts[pride_index]

# Find ebook url of Pride and Prejudice
pride_url = mycrawled_urls[pride_index]

# Download book
import bs4

# Extract the book ID
def get_book_id(string, separator):
    length_sep=len(separator)   
    for i in range(0,len(string)):
        index= string.find(separator)
    book_id = string[index+length_sep:len(string)]
    return book_id

book_id_Pride = get_book_id(pride_url, 'ebooks/')

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

directory_path ='C:\\Users\ednat\Documents\project_gutenberg\set3'
print(download_book(book_id_Pride, directory_path))

# REMOVE THE HEADER AND FOOTER IN BOOK TEXTS
header1 = '*** START OF THIS PROJECT GUTENBERG'
header2 = '*** START OF THE PROJECT GUTENBERG'
footer1 = '*** END OF THIS PROJECT GUTENBERG'
footer2 = '*** END OF THE PROJECT GUTENBERG'
directory_path= "C:/Users/ednat/Documents/project_gutenberg/set3"

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
pride_book_content = ""
dir_file = join(directory_path, f'{book_id_Pride}.txt')
pride_book_content = open_file(dir_file)

# Processing pipeline
import nltk
nltk.download('punkt')

tokenizedtext=nltk.word_tokenize(pride_book_content)
nltktext=nltk.Text(tokenizedtext)
#lowercaseword=nltktext.lower()
temp_lowercasetext=[]

for l in range(len(nltktext)):
    lowercaseword=nltktext[l].lower()
    temp_lowercasetext.append(lowercaseword)
    
lowercasetext_nltk=nltk.Text(temp_lowercasetext)
    
# Stem the loaded texts
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

temp_stemmedtext=stemtext(lowercasetext_nltk)
temp_stemmedtext=nltk.Text(temp_stemmedtext)

# Convert a POS tag for WordNet
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

# POS tag and lemmatize the loaded texts
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

lemmatizedtext=lemmatizetext(lowercasetext_nltk)
lemmatizedtext=nltk.Text(lemmatizedtext)

import nltk
import matplotlib
from nltk.draw.dispersion import dispersion_plot

# pip install -U matplotlib
lemmatizedtext.dispersion_plot(['pride', 'prejudice','elizabeth', 'darcy', 
                                'charlotte', 'love', 'hate', 'marriage', 
                                'husband', 'wife', 'father', 'mother',
                                'daughter', 'family', 'dance', 'happy'])

lemmatizedtext.dispersion_plot(['pride', 'prejudice'])
lemmatizedtext.dispersion_plot(['elizabeth', 'darcy','charlotte'])
lemmatizedtext.dispersion_plot(['love', 'hate'])
lemmatizedtext.dispersion_plot(['marriage','husband','wife','father','mother',
                                'daughter', 'family'])
lemmatizedtext.dispersion_plot(['dance', 'happy'])


#%% Exercise 3.2: The concordances of Frankenstein.

# (a) Download the Project Gutenberg .TXT ebook of Mary Wollstonecraft 
#     Shelley's "Frankenstein; Or, The Modern Prometheus", and process it using 
#     the pipeline as in Exercise 2.2 (a)-(d), no need to prune the vocabulary. 
#     (See https://en.wikipedia.org/wiki/Frankenstein for information about the work.)

# Find the crawled text containing the book Frankenstein; Or, The Modern Prometheus
book_title = 'Frankenstein; Or, The Modern Prometheus'
indexes = find_index(mycrawled_texts, book_title)
frankenstein_index=indexes[2]
mycrawled_texts[frankenstein_index]

# Find ebook url of Frankenstein
frankenstein_url = mycrawled_urls[frankenstein_index]

book_id_Frankenstein = get_book_id(frankenstein_url, 'ebooks/')
print(download_book(book_id_Frankenstein, directory_path))

# Book contents in a list without Header or Footer
frankenstein_book_content = ""
dir_file = join(directory_path, f'{book_id_Frankenstein}.txt')
frankenstein_book_content = open_file(dir_file)

tokenizedtext=nltk.word_tokenize(frankenstein_book_content)
nltktext=nltk.Text(tokenizedtext)
#lowercaseword=nltktext.lower()
temp_lowercasetext=[]

for l in range(len(nltktext)):
    lowercaseword=nltktext[l].lower()
    temp_lowercasetext.append(lowercaseword)
    
lowercasetext_nltk=nltk.Text(temp_lowercasetext)
    
# Stem the loaded texts
stemmer=nltk.stem.porter.PorterStemmer()    

temp_stemmedtext=stemtext(lowercasetext_nltk)
temp_stemmedtext=nltk.Text(temp_stemmedtext)

# POS tag and lemmatize the loaded texts
# Download tagger and wordnet resources if you do not have them already
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')

lemmatizer=nltk.stem.WordNetLemmatizer()

lemmatizedtext=lemmatizetext(lowercasetext_nltk)
lemmatizedtext=nltk.Text(lemmatizedtext)

# (b) Create a concordance view of the following words each: 'science', 
#     'horror', 'monster', 'fear'. Comment on the results.
lemmatizedtext.concordance('science')
lemmatizedtext.concordance('horror')
lemmatizedtext.concordance('monster')
lemmatizedtext.concordance('fear')

#%% Exercise 3.4: Regular expressions of Frankenstein. 
# Use the Python regular expression syntax to find occurrences of the phrase "for ... years" where ...
# denotes one or more words in the middle of the phrase in "Frankenstein; Or, The Modern
# Prometheus". Print the resulting matches.

import re

from nltk.tokenize import sent_tokenize

# Split each sentence of the book into a list
book_in_sentences = sent_tokenize(frankenstein_book_content)

pattern = r"(?i)(?:\b(for)\b)[^\w\r\n]+(?:\w+[^\w\r\n]+){0,10}?\b(years)\b.*"
pattern=re.compile(pattern)

counter=1
for sentence in book_in_sentences:
    allmatches=re.finditer(pattern,sentence)
    for tempmatch in allmatches:
        print(f'\nMatch {counter}:')
        print("Sentence:")
        print(tempmatch.group(),tempmatch.span(), "\n")
        
        print("Positions of each word:")
        print(tempmatch.group(1),tempmatch.span(1))
        print(tempmatch.group(2),tempmatch.span(2))
        
        counter+=1
    