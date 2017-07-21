# Coursera NLP Project 

###Questions to consider

 - What do the data look like?
 - Where do the data come from?
 - Can you think of any other data sources that might help you in this project?
 - What are the common steps in natural language processing?
 - What are some common issues in the analysis of text data?
 - What is the relationship between NLP and the concepts you have learned in the Specialization?
 
## About the Corpora
 
	The corpora are collected from publicly available sources by a web crawler. The crawler checks for language, so as to mainly get texts consisting of the desired language*.

	Each entry is tagged with it's date of publication. Where user comments are included they will be tagged with the date of the main entry.

	Each entry is tagged with the type of entry, based on the type of website it is collected from (e.g. newspaper or personal blog) 
	If possible, each entry is tagged with one or more subjects based on the title or keywords of the entry 
	e.g. if the entry comes from the sports section of a newspaper it will be tagged with "sports" subject).
	In many cases it's not feasible to tag the entries (for example, it's not really practical to tag each individual Twitter entry, 
	though I've got some ideas which might be implemented in the future) or no subject is found by the automated process, 
	in which case the entry is tagged with a '0'.

	To save space, the subject and type is given as a numerical code.

	Once the raw corpus has been collected, it is parsed further, to remove duplicate entries and split into individual lines. 
	Approximately 50% of each entry is then deleted. Since you cannot fully recreate any entries, the entries are anonymised and this 
	is a non-profit venture I believe that it would fall under Fair Use.

