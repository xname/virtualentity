#!/usr/bin/python

from ve_db import VeDB, VeDBException

import hashlib

class Ve(object):
	def __init__(self):
		pass

	def connect(self, host):
		self.db = VeDB(host)

	def create(self, infile, password):
		md5sum = self.md5file(infile)
		self.db.insert_soul(md5sum, infile, password)

	def get_soul(self, infile):
		'''Retrieve soul information for a file'''
		md5sum = self.md5file(infile)
		return self.db.get_soul(md5sum)

	def change_soul_password(self, soul, password):
		'''Change password of soul'''
		self.db.set_soul_password(soul, password)

	def add_alias(self, origfile, aliasfile):
		md5orig = self.md5file(origfile)
		md5alias = self.md5file(aliasfile)
		return self.db.add_alias(md5orig, md5alias)

	def md5file(self, infile):
		'''Returns an md5 hash for an object with read() method.'''
		fobj = file(infile, 'rb')
		m = hashlib.md5()
		while True:
			d = fobj.read(8096)
			if not d:
				break
			m.update(d)
		fobj.close()
		return m.hexdigest()

	def add_text_meta(self, infile, password, meta):
		soul = self.get_soul(infile)
		if not soul:
			return None
		if not self.db.check_ownership(soul, password):
			print "Can't set metadata. Wrong password."
			return None
		if soul['substance']:
			print "Soul already have metadata"
			return None
		self.db.add_text_meta(soul['id'], meta)

	def add_audio_meta(self, infile, password, meta):
		soul = self.get_soul(infile)
		if not soul:
			return None
		if not self.db.check_ownership(soul, password):
			print "Can't set metadata. Wrong password."
			return None
		if soul['substance']:
			print "Soul already have metadata"
			return None
		self.db.add_audio_meta(soul['id'], meta)

	def add_video_meta(self, infile, password, meta):
		soul = self.get_soul(infile)
		if not soul:
			return None
		if not self.db.check_ownership(soul, password):
			print "Can't set metadata. Wrong password."
			return None
		if soul['substance']:
			print "Soul already have metadata"
			return None
		self.db.add_video_meta(soul['id'], meta)

	def add_image_meta(self, infile, password, meta):
		soul = self.get_soul(infile)
		if not soul:
			return None
		if not self.db.check_ownership(soul, password):
			print "Can't set metadata. Wrong password."
			return None
		if soul['substance']:
			print "Soul already have metadata"
			return None
		self.db.add_image_meta(soul['id'], meta)

	def get_meta_from_soul(self, soul):
		if soul['substance']=='T':
			return self.get_text_meta_from_soul(soul)
		if soul['substance']=='A':
			return self.get_audio_meta_from_soul(soul)
		if soul['substance']=='V':
			return self.get_video_meta_from_soul(soul)
		if soul['substance']=='I':
			return self.get_image_meta_from_soul(soul)

	def get_text_meta_from_soul(self, soul):
		return self.db.get_text_meta(soul['id'])

	def get_audio_meta_from_soul(self, soul):
		return self.db.get_audio_meta(soul['id'])

	def get_video_meta_from_soul(self, soul):
		return self.db.get_video_meta(soul['id'])

	def get_image_meta_from_soul(self, soul):
		return self.db.get_image_meta(soul['id'])

	def remove_meta(self, soul, password):
		'''Remove metadata from soul. Password authentication needed'''
		if not self.db.check_ownership(soul, password):
			print "Can't remove metadata. Wrong password."
			return None
		if not soul['substance']:
			print "Soul doesn't have metadata"
			return None
		if soul['substance'] == 'T':
			self.db.remove_text_meta(soul['id'])
		if soul['substance'] == 'A':
			self.db.remove_audio_meta(soul['id'])
		if soul['substance'] == 'V':
			self.db.remove_video_meta(soul['id'])
		if soul['substance'] == 'I':
			self.db.remove_image_meta(soul['id'])

	def add_annotation(self, text, file_1, file_2):
		soul_1 = self.get_soul(file_1)
		if file_2:
			soul_2 = self.get_soul(file_2)
		else:
			soul_2 = None
		self.db.add_annotation(text, soul_1, soul_2)
	
	def get_annotations_from_soul(self, soul):
		'''Retrieve all annotation for a specific soul'''
		return self.db.get_annotations(soul)
	
	def parentize(self, file_1, file_2):
		'''Make file_1's soul parent of file_2's soul'''
		soul_1 = self.get_soul(file_1)
		soul_2 = self.get_soul(file_2)
		if not soul_1:
			print file_1 + " Soul not found!"
			return None
		if not soul_2:
			print file_2 + " Soul not found!"
			return None
		self.db.parentize(soul_1, soul_2)

	def get_parents_from_soul(self, soul):
		return self.db.get_parents(soul['id'])

	def get_children_from_soul(self, soul):
		return self.db.get_children(soul['id'])

	def get_cousins_from_soul(self, soul):
		return self.db.get_cousins(soul)

	def check_ownership(self, soul, password):
		return self.db.check_ownership(soul, password)

# vi:ts=4
