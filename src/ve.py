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

		if self.command == 'alias':
			if len(self.args)!=3:
				self.usage()
			self.alias = self.args[1]
			self.orig = self.args[2]

		if self.command == 'edit':
			if len(self.args)!=2:
				self.usage()
			self.input = self.args[1]

	def usage(self):
		print """
    Usage: ve [options] <command> ...

    Commands:
    create <FILE>               Create Soul of <FILE>
    alias <FILE1> <FILE2>       Alias similar Files with Identical Soul [ex foo.mov, foo.ogg] 
    edit <FILE>                 Add metadata or edit metadata to FILE.        

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

# Main program

if __name__ == "__main__":
	opts = Options(sys.argv[1:])
	ve = Ve()
	ve.connect("db.virtualentity.org")

	# Get soul information interactively from console...
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
				print "Please enter new password:",
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

	# ...and create soul in database.
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

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add a child to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert child filename:",
				child = raw_input()

				print "Making " + child + " child of " + opts.input
				ve.parentize(opts.input, child)
				resp = ""

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add parents to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert parent filename:",
				parent = raw_input()

				print "Making " + parent + " parent of " + opts.input
				ve.parentize(parent, opts.input)
				resp = ""

		resp = ""
		while (resp not in('Y','N')):
			print "Do you want to add a related file to this file (Y/N)?",
			resp = raw_input()
			if resp=='Y':
				print "Insert related filename:",
				related = raw_input()

				print "Making " + related + " parent of " + opts.input
				ve.add_annotation('', opts.input, related)
				resp = ""
		print "Shall the Entity be with you!"
         
	if opts.command == 'alias':
		soul = ve.add_alias(opts.orig, opts.alias)

# vi:ts=4
