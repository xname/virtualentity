#!/usr/bin/python

from ve_engine import Ve
from ve_db import VeDB, VeDBException

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

		if len(self.args)<1:
			self.usage()
		self.command = self.args[0]

		if self.command == 'create':
			if len(self.args)!=2:
				self.usage()
			self.input = self.args[1]

		if self.command == 'twin':
			if len(self.args)!=3:
				self.usage()
			self.alias = self.args[1]
			self.orig = self.args[2]

		if self.command == 'edit':
			if len(self.args)!=2:
				self.usage()
			self.input = self.args[1]


                if self.command == 'scan':
                        if len(self.args)==1:
                                self.input = None
                        elif len(self.args)==2:
				self.input = self.args[1]


		if self.command == 'expand':
			if len(self.args)<2 or len(self.args)>3:
				self.usage()
			self.input = self.args[1]

			if len(self.args)==3:
				self.input2 = self.args[2]
			else:
				self.input2 = None

		if self.command == 'parentize':
			if len(self.args)!=3:
				self.usage()
			self.input = self.args[1]
			self.input2 = self.args[2]


                if self.command == 'aura':
			if len(self.args)<2 or len(self.args)>3:
				self.usage()
			self.input = self.args[1]		

        def usage(self):
		print """
    Welcome to Virtual Entity!

    Usage: soul [options] <command> ...

    Commands:
    create <FILE>               Create Soul of <FILE>
    scan [<FILE>]               Scan for Souls in current directory or visualize soul of FILE 
    twin <FILE1> <FILE2>        Attaches Soul of FILE2 to FILE1, where the two are identical [ex foo.mov, foo.ogg]
    edit <FILE>                 Add metadata or edit soul metadata of FILE        
    expand <FILE>               Expand Soul of <FILE> with aura annotation      
    parentize <FILE1> <FILE2>   Annotate genetic relation where <FILE1> is ancestor of <FILE2>
    aura <FILE>			Insert information in Aura: genetic and/or semantic relations and annotations

    Options:
    -h, --help                  Show usage
		"""
		sys.exit(2)




def raw_input_field(meta, key):
        if (not meta) or (not meta.has_key(key)):
                return raw_input(' : ')
        result = raw_input('['+str(meta[key])+'] : ')
        if result == "":
                return meta[key]
        return result




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
                                print "        -> Have the following semantic relation:"
                                print "----------------------------------------------"
                                print_status(cousin, True, False)
                                print "----------------------------------------------"
                        else:
                                parent_meta = ve.get_meta_from_soul(cousin)
                                print "        -> Have the following semantic relation:"
                                print "            Title: " + parent_meta['title']
                                print "            Author: " + parent_meta['author']

        parents = ve.get_parents_from_soul(soul)
        if len(parents)>0:
                for parent in parents:
                        if parent_verbose:
                                print "----------------------------------------------"
                                print "        -> Have the following ancestor:"
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
                                print "        -> Have the following descendent:"
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




	# Get Soul information interactively from console...
	if opts.command == 'create':
		print "Initialising Soul for " + opts.input + "."

	if opts.command == 'edit' or opts.command == 'create':
		edited_soul = ve.get_soul(opts.input)
		if edited_soul:
			if not ve.check_ownership(edited_soul, ""):
				print "Soul exists. Please insert password:",
				opts.password = raw_input()
				if not ve.check_ownership(edited_soul, opts.password):
					print "Wrong password. Access denied."
					sys.exit(0)
			else:
				print "This Soul is open!"
				print "Please enter new password (or press ENTER to leave Soul open):",
				opts.password = raw_input()
				ve.change_soul_password(edited_soul, opts.password)
				
			meta = ve.get_meta_from_soul(edited_soul)
		else:
			meta = { }

		natural_born = ""
		color = ""
		ve_status = ""

		if not edited_soul:
			substance = ""
			while (not substance in ('text', 'audio', 'video', 'image')):
				print "Substance [text, audio, video, image]:",
				substance = raw_input()
			print "Define " + substance + " Soul."
		else:
			substance = edited_soul['substance']
			if substance=='T': substance='text'
			if substance=='A': substance='audio'
			if substance=='V': substance='video'
			if substance=='I': substance='image'
			print "Define " + substance + " soul."

		print "Title",
		meta['title'] = raw_input_field(meta, 'title')
		print "Author",
		meta['author'] = raw_input_field(meta, 'author')
		print "Year (of creation)",
		meta['year_of_creation'] = raw_input_field(meta, 'year_of_creation')
		print "Date (of upload) (format: YYYY-DD-MM HH:MM:SS)",
		meta['date_of_upload'] = raw_input_field(meta, 'date_of_upload')
		print "Place (of creation)",
		meta['place_of_creation'] = raw_input_field(meta, 'place_of_creation')
		print "Filename",
		meta['filename'] = raw_input_field(meta, 'filename')
		print "Subject",
		meta['subject'] = raw_input_field(meta, 'subject')
		print "Language (main)",
		meta['language'] = raw_input_field(meta, 'language')
		print "Institution",
		meta['institution'] = raw_input_field(meta, 'institution')
		print "Company",
		meta['company'] = raw_input_field(meta, 'company')
		print "Publisher",
		meta['publisher'] = raw_input_field(meta, 'publisher')
		print "Purchaser",
		meta['purchaser'] = raw_input_field(meta, 'purchaser')
		print "Collection",
		meta['collection'] = raw_input_field(meta, 'collection')

		if substance == 'text':
			print "Pages",
			meta['pages'] = raw_input_field(meta, 'pages')
			print "First edition year",
			meta['first_edition_year'] = raw_input_field(meta, 'first_edition_year')
			print "ISBN",
			meta['isbn'] = raw_input_field(meta, 'isbn')
		if substance == 'audio':
			print "Duration",
			meta['duration'] = raw_input_field(meta, 'duration')
		if substance == 'video':
			print "Duration",
			meta['duration'] = raw_input_field(meta, 'duration')
		if substance == 'image':
			print "Size",
			meta['size'] = raw_input_field(meta, 'size')

		print "Keywords",
		meta['keywords'] = raw_input_field(meta, 'keywords')
		print "Type",
		meta['type'] = raw_input_field(meta, 'type')
		print "Genre",
		meta['genre'] = raw_input_field(meta, 'genre')
		print "Format (original)",
		meta['format'] = raw_input_field(meta, 'format')

		while natural_born not in ('D', 'A'):
			print "Natural Born ('D' for digital or 'A' for analogue)",
			natural_born = raw_input_field(meta, 'natural_born')
			meta['natural_born'] = natural_born
		if meta['natural_born'] == 'A':
			print "Material Carrier",
			meta['carrier'] = raw_input_field(meta, 'carrier')
			print "Digitisation Process",
			meta['digitisation_process'] = raw_input_field(meta, 'digitisation_process')
			meta['creation_software'] = ""
		else:
			print "Creation software",
			meta['creation_software'] = raw_input_field(meta, 'creation_software')
			meta['carrier'] = ""
			meta['digitisation_process'] = ""

		if substance == 'audio':
			print "Short content",
			meta['short_content'] = raw_input_field(meta, 'short_content')
			print "Sound (stereo/mono)",
			meta['sound'] = raw_input_field(meta, 'sound')

		if substance == 'video':
			print "System (PAL, NTSC)",
			meta['system'] = raw_input_field(meta, 'system')
			print "Short content",
			meta['short_content'] = raw_input_field(meta, 'short_content')
			print "Sound",
			meta['sound'] = raw_input_field(meta, 'sound')
			while not color in ('C', 'BW'):
				print "Color ('C' for color or 'BW' for black/white)",
				color = raw_input_field(meta, 'color')
				meta['color'] = color
			print "Announcing titles",
			meta['announcing_titles'] = raw_input_field(meta, 'announcing_titles')
			print "Credits",
			meta['credits'] = raw_input_field(meta, 'credits')

		print "Related URL",
		meta['url'] = raw_input_field(meta, 'url')
		print "Fruition suggestions",
		meta['fruition_suggestions'] = raw_input_field(meta, 'fruition_suggestions')
		print "Observations",
		meta['observations'] = raw_input_field(meta, 'observations')
		print "Remark",
		meta['remark'] = raw_input_field(meta, 'remark')
		print "Future Plans",
		meta['future_plans'] = raw_input_field(meta, 'future_plans')

		while ve_status not in ('Y', 'N'):
			print "Virtual Entity Status ('Y' or 'N')",
			ve_status = raw_input_field(meta, 've_status')
			meta['ve_status'] = ve_status
		if meta['ve_status'] == 'N':
			print "License",
			meta['license'] = raw_input_field(meta, 'license')
		else:
			meta['license'] = ""
			print "Entity Liberated!"

	if opts.command == 'create' and not edited_soul:
		print "Please provide an access password for Soul (or press ENTER to leave Soul open):",
		opts.password = raw_input()

	# ...and create Soul in database.
	if opts.command == 'create':
		try:
			if not edited_soul:
				ve.create(opts.input, opts.password)
				print "Soul Created."
		except IOError, (errno, strerror):
			print "Error: %s" % (strerror)
		except VeDBException, (errno, strerror):
			print "Error: %s" % (strerror)

	if opts.command == 'edit' or opts.command == 'create':
		if edited_soul:
			ve.remove_meta(edited_soul, opts.password)
		if substance == 'text':
			ve.add_text_meta(opts.input, opts.password, meta)
		if substance == 'audio':
			ve.add_audio_meta(opts.input, opts.password, meta)
		if substance == 'video':
			ve.add_video_meta(opts.input, opts.password, meta)
		if substance == 'image':
			ve.add_image_meta(opts.input, opts.password, meta)
		if edited_soul:
			print "Soul updated."
                 	print "May the Entity be with you!"				


	if opts.command == 'aura':
		print "You are now writing in the Aura of the Soul of " + opts.input

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add a descendent to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert child filename:",
				child = raw_input()

				print "Making " + child + " descendent of " + opts.input
				ve.parentize(opts.input, child)
				resp = ""

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add ancestors to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert ancestor filename:",
				parent = raw_input()

				print "Making " + parent + " ancestor of " + opts.input
				ve.parentize(parent, opts.input)
				resp = ""

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add a semantic relation to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert related filename:",
				related = raw_input()

				print "Making " + related + " semantically connected to " + opts.input
				ve.add_annotation('', opts.input, related)
				resp = ""

		resp = ""
		while (resp not in('Y','N')):
		 	print "Do you want to expand soul of " + opts.input + " with annotation (Y/N)?",
			resp =raw_input() 
			if resp=='Y':		
				print "Insert text:",
                 		text = raw_input()
                 		ve.add_annotation(text, opts.input, '')
				resp = ""

		print "Aura expanded!"
		print "May the Entity be with you!"
         


	if opts.command == 'twin':
		soul = ve.add_alias(opts.orig, opts.alias)


        if opts.command == 'scan':
		print "Scanning ",
		if opts.input:
			print "\n",
			soul = ve.get_soul(opts.input)
			if not soul:
				print opts.input + " soul not found"
			else:
				print opts.input + ":"
				print_status(soul, True, True)
		else:
			print "directory for souls:"
			for infile in glob.glob('*'):
				if os.path.isfile(infile):
					soul = ve.get_soul(infile)
					if not soul:
						print infile + " soul not found"
					else:
						print infile + ":"
						print_status(soul, False, False)


	if opts.command == 'parentize':
		print "Making " + opts.input + " ancestor of " + opts.input2
		ve.parentize(opts.input, opts.input2)
	
	if opts.command == 'expand':
		print "Adding annotation to expand soul of " + opts.input,
		if opts.input2:
			print " and " + opts.input2
		else:
			print
		print "Insert text:",
		text = raw_input()
		ve.add_annotation(text, opts.input, opts.input2)

# vi:ts=4











