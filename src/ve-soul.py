#!/usr/bin/python

from ve_engine import Ve

import sys
import getopt
import glob
import os

class Options(object):
	def __init__(self, argv):
		self.input = None
		self.debug = False
		try:
			opts, self.args = getopt.getopt(argv, "h", ["help"])
		except getopt.GetoptError:
			self.usage()
		for opt, arg in opts:
			if opt in ("-h", "--help"):
				self.usage()

		if len(self.args)==1:
			self.input = self.args[0]
		elif len(self.args)==0:
			self.input = None
		else:
			self.usage

	def usage(self):
		print """
    Usage: ve-soul [options] [<FILE>]

    Prints status for files in current directory or for <FILE> if specified.

    Options:
    -h, --help                  Show usage
		"""
		sys.exit(2)



# Function that prints soul status for infile
def print_status(soul, verbose, parent_verbose):
	if soul.has_key('alias_id'):
		print "        -> Is an alias of soul with id " + soul['id']
	else:
		print "        -> Have a soul with id " + soul['id']

	if soul['substance']:
		meta = ve.get_meta_from_soul(soul)
		if soul['substance']=='T':
			print "        -> Have the following text metadata"
		if soul['substance']=='A':
			print "        -> Have the following audio metadata"
		if soul['substance']=='V':
			print "        -> Have the following video metadata"
		if soul['substance']=='I':
			print "        -> Have the following image metadata"

		print "        Title: " + meta['title']
		print "        Author: " + meta['author']
		if verbose:
			print "        Year of creation: " + meta['year_of_creation']
			print "        Date of upload: " + meta['date_of_upload']
			print "        Place of creation: " + meta['place_of_creation']
			print "        Filename: " + meta['filename']
			print "        Subject: " + meta['subject']
			print "        Language: " + meta['language']
			print "        Institution: " + meta['institution']
			print "        Company: " + meta['company']
			print "        Publisher: " + meta['publisher']
			print "        Purchaser: " + meta['purchaser']
			print "        Collection: " + meta['collection']
			if soul['substance'] == 'T':
				print "        Pages: " + meta['pages']
				print "        First edition year: " + meta['first_edition_year']
				print "        ISBN: " + meta['isbn']
			if soul['substance'] == 'V':
				print "        Duration: " + meta['duration']
			if soul['substance'] == 'I':
				print "        Size: " + meta['size']
			print "        Keywords: " + meta['keywords']
			print "        Type: " + meta['type']
			print "        Genre: " + meta['genre']
			print "        Format: " + meta['format']
			print "        Natural born: " + meta['natural_born']
			if meta['natural_born'] == 'D':
				print "        Creation software: " + meta['creation_software']
			else:	
				print "        Analog carrier: " + meta['carrier']
				print "        Digitisation process: " + meta['digitisation_process']
			if soul['substance'] == 'V':
				print "        System: " + meta['system']
				print "        Short content: " + meta['short_content']
				print "        Sound: " + meta['sound']
				print "        Color/BW: " + meta['color']
				print "        Announcing titles: " + meta['announcing_titles']
				print "        Credits: " + meta['credits']
			print "        Related URL: " + meta['url']
			print "        Fruition suggestions: " + meta['fruition_suggestions']
			print "        Observations: " + meta['observations']
			print "        Remark: " + meta['remark']
			print "        Future plans: " + meta['future_plans']
			print "        VE status: " + meta['ve_status']
			if meta['ve_status'] == 'N':
				print "        License: " + meta['license']
	
	if verbose:
		annotations = ve.get_annotations_from_soul(soul)
		for ann in annotations:
			print "        Note inserted " + ann['date'] + ": " + ann['text']

	cousins = ve.get_cousins_from_soul(soul)
	if len(cousins)>0:
		for cousin in cousins:
			if parent_verbose:
				print "----------------------------------------------"
				print "        -> Have the following related soul:"
				print "----------------------------------------------"
				print_status(cousin, True, False)
				print "----------------------------------------------"
			else:
				parent_meta = ve.get_meta_from_soul(cousin)
				print "        -> Have the following related soul:"
				print "            Title: " + parent_meta['title']
				print "            Author: " + parent_meta['author']

	parents = ve.get_parents_from_soul(soul)
	if len(parents)>0:
		for parent in parents:
			if parent_verbose:
				print "----------------------------------------------"
				print "        -> Have the following parent soul:"
				print "----------------------------------------------"
				print_status(parent, True, False)
				print "----------------------------------------------"
			else:
				parent_meta = ve.get_meta_from_soul(parent)
				print "        -> Have the following parent soul:"
				print "            Title: " + parent_meta['title']
				print "            Author: " + parent_meta['author']

	children = ve.get_children_from_soul(soul)
	if len(children)>0:
		for child in children:
			if parent_verbose:
				print "----------------------------------------------"
				print "        -> Have the following child soul:"
				print "----------------------------------------------"
				print_status(child, True, False)
				print "----------------------------------------------"
			else:
				child_meta = ve.get_meta_from_soul(child)
				print "        -> Have the following child soul:"
				print "            Title: " + child_meta['title']
				print "            Author: " + child_meta['author']

# Main program

if __name__ == "__main__":
	opts = Options(sys.argv[1:])
	ve = Ve()
	ve.connect("db.virtualentity.org")

	# lists all soul metadata for files in current directory
	if opts.input:
		soul = ve.get_soul(opts.input)
		if not soul:
			print opts.input + " soul not found"
		else:
			print opts.input + ":"
			print_status(soul, True, True)

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add an annotation to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Annotation text:",
				text = raw_input()

				ve.add_annotation(text, opts.input, None)
				print "Annotation added."
				resp = ""

	else:
		for infile in glob.glob('*'):
			if os.path.isfile(infile):
				soul = ve.get_soul(infile)
				if not soul:
					print infile + " soul not found"
				else:
					print infile + ":"
					print_status(soul, False, False)
		
# vi:ts=4
