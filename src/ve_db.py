#!/usr/bin/python

import _mysql
import _mysql_exceptions

class VeDBException(Exception):
	def __init__(self, errno, strerror):
		self.args = (errno, strerror)
	def __str__(self):
		return repr(self.args)
 
class VeDB(object):
	def __init__(self, dbhost):
		self.db = _mysql.connect(host=dbhost, user="ve", passwd="ve", db="ve")

	def esc(self, string):
		return self.db.escape_string(string)

	def insert_soul(self, md5, filename, password):
		try:
			_filename = self.esc(filename)
			_password = self.esc(password)
			self.db.query("INSERT INTO souls (md5, filename, password) VALUES ('" + 
				md5 + "','" + _filename + "'," + " PASSWORD('" + _password + "'))")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			if errno==1062:
				raise VeDBException, (1, "VE soul insert error: Duplicate soul entry.")
			raise

	def get_alias(self, md5):
		self.db.query("""
			SELECT id, md5, substance, password, filename FROM souls
				WHERE id = (
					SELECT soul_id FROM aliases
					WHERE md5='""" + md5 + """'
				)
		""");
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return None
		row = row[0]
		return { 'alias_id':0, 'id':row[0], 'md5':row[1], 'substance':row[2], 'password':row[3], 'filename':row[4] }

	def get_soul(self, md5):
		self.db.query("SELECT id, md5, substance, password, filename FROM souls WHERE md5='" + md5 + "'");
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return self.get_alias(md5)
		row = row[0]
		return { 'id':row[0], 'md5':row[1], 'substance':row[2], 'password':row[3], 'filename':row[4] }

	def set_soul_password(self, soul, password):
		soul_id = soul['id']
		password = self.esc(password)
		self.db.query("UPDATE souls SET password=PASSWORD('" + password + "') " +
			"WHERE id='" + soul_id + "'")

	def get_soul_by_id(self, id):
		self.db.query("SELECT id, md5, substance, password, filename FROM souls WHERE id='" + id + "'");
		r = self.db.store_result()
		row = r.fetch_row()
		row = row[0]
		return { 'id':row[0], 'md5':row[1], 'substance':row[2], 'password':row[3], 'filename':row[4] }

	def add_alias(self, md5_orig, md5_alias):
		orig = self.get_soul(md5_orig)
		if not orig:
			raise VeDBException, (3, "VE alias insert error: Soul not found.")
		try:
			self.db.query("INSERT INTO aliases (soul_id, md5) VALUES ('" + orig['id'] + "', '" + md5_alias + "')")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			if errno==1062:
				raise VeDBException, (2, "VE alias insert error: Duplicate alias entry.")
			raise

	def add_text_meta(self, soul_id, meta):
		title = self.esc(meta['title'])
		author = self.esc(meta['author'])
		year_of_creation = self.esc(meta['year_of_creation'])
		date_of_upload = self.esc(meta['date_of_upload'])
		place_of_creation = self.esc(meta['place_of_creation'])
		filename = self.esc(meta['filename'])
		subject = self.esc(meta['subject'])
		language = self.esc(meta['language'])
		institution = self.esc(meta['institution'])
		company = self.esc(meta['company'])
		publisher = self.esc(meta['publisher'])
		purchaser = self.esc(meta['purchaser'])
		collection = self.esc(meta['collection'])
		pages = self.esc(meta['pages'])
		first_edition_year = self.esc(meta['first_edition_year'])
		isbn = self.esc(meta['isbn'])
		keywords = self.esc(meta['keywords'])
		type = self.esc(meta['type'])
		genre = self.esc(meta['genre'])
		format = self.esc(meta['format'])
		natural_born = self.esc(meta['natural_born'])
		carrier = self.esc(meta['carrier'])
		digitisation_process = self.esc(meta['digitisation_process'])
		creation_software = self.esc(meta['creation_software'])
		url = self.esc(meta['url'])
		fruition_suggestions = self.esc(meta['fruition_suggestions'])
		observations = self.esc(meta['observations'])
		remark = self.esc(meta['remark'])
		future_plans = self.esc(meta['future_plans'])
		ve_status = self.esc(meta['ve_status'])
		license = self.esc(meta['license'])
		
		try:
			self.db.query("INSERT INTO text_meta " +
				"(soul_id, title, author, year_of_creation, date_of_upload, place_of_creation, " + 
				"filename, subject, language, institution, company, publisher, purchaser, collection, " +
				"pages, first_edition_year, isbn, keywords, type, genre, format, natural_born, carrier, " + 
				"digitisation_process, creation_software, url, fruition_suggestions, observations, remark, " +
				"future_plans, ve_status, license) VALUES " +
				"('" + soul_id + "','" + title + "','" + author + "','" + year_of_creation + "','" +
				date_of_upload + "',' " + place_of_creation + "','" + filename + "','" + subject + "','" +
				language + "','" + institution + "','" + company + "','" + publisher + "','" + purchaser + "','" +
				collection + "','" + pages + "','" + first_edition_year + "','" + isbn + "','" + keywords + "','" +
				type + "','" + genre + "','" + format + "','" + natural_born + "','" + carrier + "','" +
				digitisation_process + "','" + creation_software + "','" + url + "','" +
				fruition_suggestions + "','" + observations + "','" + remark + "','" + future_plans + "','" +
				ve_status + "','" + license + "')")
			self.db.query("UPDATE souls SET substance='T' WHERE id='" + soul_id + "'")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			raise

	def remove_text_meta(self, soul_id):
		self.db.query("DELETE FROM text_meta WHERE soul_id='" + soul_id + "'")
		self.db.query("UPDATE souls SET substance=NULL WHERE id='" + soul_id + "'")

	def add_audio_meta(self, soul_id, meta):
		title = self.esc(meta['title'])
		author = self.esc(meta['author'])
		year_of_creation = self.esc(meta['year_of_creation'])
		date_of_upload = self.esc(meta['date_of_upload'])
		place_of_creation = self.esc(meta['place_of_creation'])
		filename = self.esc(meta['filename'])
		subject = self.esc(meta['subject'])
		language = self.esc(meta['language'])
		institution = self.esc(meta['institution'])
		company = self.esc(meta['company'])
		publisher = self.esc(meta['publisher'])
		purchaser = self.esc(meta['purchaser'])
		collection = self.esc(meta['collection'])
		duration = self.esc(meta['duration'])
		keywords = self.esc(meta['keywords'])
		type = self.esc(meta['type'])
		genre = self.esc(meta['genre'])
		format = self.esc(meta['format'])
		natural_born = self.esc(meta['natural_born'])
		carrier = self.esc(meta['carrier'])
		digitisation_process = self.esc(meta['digitisation_process'])
		creation_software = self.esc(meta['creation_software'])
		short_content = self.esc(meta['short_content'])
		sound = self.esc(meta['sound'])
		url = self.esc(meta['url'])
		fruition_suggestions = self.esc(meta['fruition_suggestions'])
		observations = self.esc(meta['observations'])
		remark = self.esc(meta['remark'])
		future_plans = self.esc(meta['future_plans'])
		ve_status = self.esc(meta['ve_status'])
		license = self.esc(meta['license'])
		
		try:
			self.db.query("INSERT INTO audio_meta " +
				"(soul_id, title, author, year_of_creation, date_of_upload, place_of_creation," + 
				"filename, subject, language, institution, company, publisher, purchaser, collection, duration," +
				"keywords, type, genre, format, natural_born, carrier, digitisation_process, creation_software, " + 
				"short_content, sound, url, fruition_suggestions, observations, remark, " +
				"future_plans, ve_status, license) VALUES " +
				"('" + soul_id + "','" + title + "','" + author + "','" + year_of_creation + "','" +
				date_of_upload + "',' " + place_of_creation + "','" + filename + "','" + subject + "','" +
				language + "','" + institution + "','" + company + "','" + publisher + "','" + purchaser + "','" +
				collection + "','" + duration + "','" + keywords + "','" + type + "','" + genre + "','" + 
				format + "','" + natural_born + "','" + carrier + "','" + digitisation_process + "','" + 
				creation_software + "','" + short_content + "','" + sound + "','" + url + "','" + 
				fruition_suggestions + "','" + observations + "','" + remark + "','" + future_plans + "','" + 
				ve_status + "','" + license + "')")
			self.db.query("UPDATE souls SET substance='A' WHERE id='" + soul_id + "'")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			raise

	def remove_audio_meta(self, soul_id):
		self.db.query("DELETE FROM audio_meta WHERE soul_id='" + soul_id + "'")
		self.db.query("UPDATE souls SET substance=NULL WHERE id='" + soul_id + "'")

	def add_video_meta(self, soul_id, meta):
		title = self.esc(meta['title'])
		author = self.esc(meta['author'])
		year_of_creation = self.esc(meta['year_of_creation'])
		date_of_upload = self.esc(meta['date_of_upload'])
		place_of_creation = self.esc(meta['place_of_creation'])
		filename = self.esc(meta['filename'])
		subject = self.esc(meta['subject'])
		language = self.esc(meta['language'])
		institution = self.esc(meta['institution'])
		company = self.esc(meta['company'])
		publisher = self.esc(meta['publisher'])
		purchaser = self.esc(meta['purchaser'])
		collection = self.esc(meta['collection'])
		duration = self.esc(meta['duration'])
		keywords = self.esc(meta['keywords'])
		type = self.esc(meta['type'])
		genre = self.esc(meta['genre'])
		format = self.esc(meta['format'])
		natural_born = self.esc(meta['natural_born'])
		carrier = self.esc(meta['carrier'])
		digitisation_process = self.esc(meta['digitisation_process'])
		creation_software = self.esc(meta['creation_software'])
		system = self.esc(meta['system'])
		short_content = self.esc(meta['short_content'])
		sound = self.esc(meta['sound'])
		color = self.esc(meta['color'])
		announcing_titles = self.esc(meta['announcing_titles'])
		credits = self.esc(meta['credits'])
		url = self.esc(meta['url'])
		fruition_suggestions = self.esc(meta['fruition_suggestions'])
		observations = self.esc(meta['observations'])
		remark = self.esc(meta['remark'])
		future_plans = self.esc(meta['future_plans'])
		ve_status = self.esc(meta['ve_status'])
		license = self.esc(meta['license'])
		
		try:
			self.db.query("INSERT INTO video_meta " +
				"(soul_id, title, author, year_of_creation, date_of_upload, place_of_creation," + 
				"filename, subject, language, institution, company, publisher, purchaser, collection, duration," +
				"keywords, type, genre, format, natural_born, carrier, digitisation_process, creation_software, system, " + 
				"short_content, sound, color, announcing_titles, credits, url, fruition_suggestions, observations, remark, " +
				"future_plans, ve_status, license) VALUES " +
				"('" + soul_id + "','" + title + "','" + author + "','" + year_of_creation + "','" +
				date_of_upload + "',' " + place_of_creation + "','" + filename + "','" + subject + "','" +
				language + "','" + institution + "','" + company + "','" + publisher + "','" + purchaser + "','" +
				collection + "','" + duration + "','" + keywords + "','" +
				type + "','" + genre + "','" + format + "','" + natural_born + "','" + carrier + "','" +
				digitisation_process + "','" + creation_software + "','" + system + "','" + short_content + "','" +
				sound + "','" + color + "','" + announcing_titles + "','" + credits + "','" + url + "','" +
				fruition_suggestions + "','" + observations + "','" + remark + "','" + future_plans + "','" +
				ve_status + "','" + license + "')")
			self.db.query("UPDATE souls SET substance='V' WHERE id='" + soul_id + "'")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			raise

	def remove_video_meta(self, soul_id):
		self.db.query("DELETE FROM video_meta WHERE soul_id='" + soul_id + "'")
		self.db.query("UPDATE souls SET substance=NULL WHERE id='" + soul_id + "'")

	def add_image_meta(self, soul_id, meta):
		title = self.esc(meta['title'])
		author = self.esc(meta['author'])
		year_of_creation = self.esc(meta['year_of_creation'])
		date_of_upload = self.esc(meta['date_of_upload'])
		place_of_creation = self.esc(meta['place_of_creation'])
		filename = self.esc(meta['filename'])
		subject = self.esc(meta['subject'])
		language = self.esc(meta['language'])
		institution = self.esc(meta['institution'])
		company = self.esc(meta['company'])
		publisher = self.esc(meta['publisher'])
		purchaser = self.esc(meta['purchaser'])
		collection = self.esc(meta['collection'])
		size = self.esc(meta['size'])
		keywords = self.esc(meta['keywords'])
		type = self.esc(meta['type'])
		genre = self.esc(meta['genre'])
		format = self.esc(meta['format'])
		natural_born = self.esc(meta['natural_born'])
		carrier = self.esc(meta['carrier'])
		digitisation_process = self.esc(meta['digitisation_process'])
		creation_software = self.esc(meta['creation_software'])
		url = self.esc(meta['url'])
		fruition_suggestions = self.esc(meta['fruition_suggestions'])
		observations = self.esc(meta['observations'])
		remark = self.esc(meta['remark'])
		future_plans = self.esc(meta['future_plans'])
		ve_status = self.esc(meta['ve_status'])
		license = self.esc(meta['license'])
		
		try:
			self.db.query("INSERT INTO image_meta " +
				"(soul_id, title, author, year_of_creation, date_of_upload, place_of_creation," + 
				"filename, subject, language, institution, company, publisher, purchaser, collection, size," +
				"keywords, type, genre, format, natural_born, carrier, digitisation_process, creation_software, " + 
				"url, fruition_suggestions, observations, remark, " +
				"future_plans, ve_status, license) VALUES " +
				"('" + soul_id + "','" + title + "','" + author + "','" + year_of_creation + "','" +
				date_of_upload + "',' " + place_of_creation + "','" + filename + "','" + subject + "','" +
				language + "','" + institution + "','" + company + "','" + publisher + "','" + purchaser + "','" +
				collection + "','" + size + "','" + keywords + "','" + type + "','" + genre + "','" + 
				format + "','" + natural_born + "','" + carrier + "','" + digitisation_process + "','" + 
				creation_software + "','" + url + "','" + 
				fruition_suggestions + "','" + observations + "','" + remark + "','" + future_plans + "','" + 
				ve_status + "','" + license + "')")
			self.db.query("UPDATE souls SET substance='I' WHERE id='" + soul_id + "'")
			return self.db.insert_id()
		except _mysql_exceptions.IntegrityError, (errno, strerror):
			raise

	def remove_image_meta(self, soul_id):
		self.db.query("DELETE FROM image_meta WHERE soul_id='" + soul_id + "'")
		self.db.query("UPDATE souls SET substance=NULL WHERE id='" + soul_id + "'")

	def get_text_meta(self, soul_id):
		self.db.query("SELECT id, title, author, year_of_creation, date_of_upload, place_of_creation," +
			"filename, subject, language, institution, company, publisher, purchaser, collection," +
			"pages, first_edition_year, isbn, keywords, type, genre, format, natural_born, carrier," +
			"digitisation_process, creation_software, url, fruition_suggestions, observations, remark," +
			"future_plans, ve_status, license FROM text_meta WHERE soul_id='" + soul_id + "'")
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return None
		row = row[0]
		return { 'id':row[0], 'title':row[1], 'author':row[2], 'soul_id':soul_id,
			'year_of_creation':row[3], 'date_of_upload':row[4], 'place_of_creation':row[5], 'filename':row[6],
			'subject':row[7], 'language':row[8], 'institution':row[9], 'company':row[10], 'publisher':row[11],
			'purchaser':row[12], 'collection':row[13], 'pages':row[14], 'first_edition_year':row[15],
			'isbn':row[16], 'keywords':row[17], 'type':row[18], 'genre':row[19], 'format':row[20],
			'natural_born':row[21], 'carrier':row[22], 'digitisation_process':row[23],
			'creation_software':row[24], 'url':row[25], 'fruition_suggestions':row[26], 'observations':row[27],
			'remark':row[28], 'future_plans':row[29], 've_status':row[30], 'license':row[31] }

	def get_audio_meta(self, soul_id):
		self.db.query("SELECT id, title, author, year_of_creation, date_of_upload, place_of_creation," +
			"filename, subject, language, institution, company, publisher, purchaser, collection," +
			"keywords, type, genre, format, natural_born, carrier," +
			"digitisation_process, creation_software, url, fruition_suggestions, observations, remark," +
			"future_plans, ve_status, license, duration, short_content, sound " +
			"FROM audio_meta WHERE soul_id='" + soul_id + "'")
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return None
		row = row[0]
		return { 'id':row[0], 'title':row[1], 'author':row[2], 'soul_id':soul_id,
			'year_of_creation':row[3], 'date_of_upload':row[4], 'place_of_creation':row[5], 'filename':row[6],
			'subject':row[7], 'language':row[8], 'institution':row[9], 'company':row[10], 'publisher':row[11],
			'purchaser':row[12], 'collection':row[13],
			'keywords':row[14], 'type':row[15], 'genre':row[16], 'format':row[17],
			'natural_born':row[18], 'carrier':row[19], 'digitisation_process':row[20],
			'creation_software':row[21], 'url':row[22], 'fruition_suggestions':row[23], 'observations':row[24],
			'remark':row[25], 'future_plans':row[26], 've_status':row[27], 'license':row[28],
			'duration':row[29], 'short_content':row[30], 'sound':row[31] }

	def get_image_meta(self, soul_id):
		self.db.query("SELECT id, title, author, year_of_creation, date_of_upload, place_of_creation," +
			"filename, subject, language, institution, company, publisher, purchaser, collection," +
			"keywords, type, genre, format, natural_born, carrier," +
			"digitisation_process, creation_software, url, fruition_suggestions, observations, remark," +
			"future_plans, ve_status, license, size " +
			"FROM image_meta WHERE soul_id='" + soul_id + "'")
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return None
		row = row[0]
		return { 'id':row[0], 'title':row[1], 'author':row[2], 'soul_id':soul_id,
			'year_of_creation':row[3], 'date_of_upload':row[4], 'place_of_creation':row[5], 'filename':row[6],
			'subject':row[7], 'language':row[8], 'institution':row[9], 'company':row[10], 'publisher':row[11],
			'purchaser':row[12], 'collection':row[13],
			'keywords':row[14], 'type':row[15], 'genre':row[16], 'format':row[17],
			'natural_born':row[18], 'carrier':row[19], 'digitisation_process':row[20],
			'creation_software':row[21], 'url':row[22], 'fruition_suggestions':row[23], 'observations':row[24],
			'remark':row[25], 'future_plans':row[26], 've_status':row[27], 'license':row[28],
			'size':row[29] }

	def get_video_meta(self, soul_id):
		self.db.query("SELECT id, title, author, year_of_creation, date_of_upload, place_of_creation," +
			"filename, subject, language, institution, company, publisher, purchaser, collection," +
			"keywords, type, genre, format, natural_born, carrier," +
			"digitisation_process, creation_software, url, fruition_suggestions, observations, remark," +
			"future_plans, ve_status, license, " + 
			"duration, system, short_content, sound, color, announcing_titles, credits " +
			"FROM video_meta WHERE soul_id='" + soul_id + "'")
		r = self.db.store_result()
		row = r.fetch_row()
		if not row:
			return None
		row = row[0]
		return { 'id':row[0], 'title':row[1], 'author':row[2], 'soul_id':soul_id,
			'year_of_creation':row[3], 'date_of_upload':row[4], 'place_of_creation':row[5], 'filename':row[6],
			'subject':row[7], 'language':row[8], 'institution':row[9], 'company':row[10], 'publisher':row[11],
			'purchaser':row[12], 'collection':row[13],
			'keywords':row[14], 'type':row[15], 'genre':row[16], 'format':row[17],
			'natural_born':row[18], 'carrier':row[19], 'digitisation_process':row[20],
			'creation_software':row[21], 'url':row[22], 'fruition_suggestions':row[23], 'observations':row[24],
			'remark':row[25], 'future_plans':row[26], 've_status':row[27], 'license':row[28],
			'duration':row[29], 'system':row[30], 'short_content':row[31], 'sound':row[32], 'color':row[33],
			'announcing_titles':row[34], 'credits':row[35] }

	def check_ownership(self, soul, password):
		'''Check if password match the soul's password'''
		_password = self.esc(password)
		self.db.query("SELECT PASSWORD('" + _password + "')")
		r = self.db.store_result()
		row = r.fetch_row()
		password = row[0][0]
		return soul['password'] == password

	def add_annotation(self, note, soul_1, soul_2):
		'''Insert an annotation for soul_1 and eventually for soul_2'''
		_note = self.esc(note)
		soul_id_1 = "'" + soul_1['id'] + "'"
		if soul_2:
			soul_id_2 = "'" + soul_2['id'] + "'"
		else:
			soul_id_2 = 'NULL'
		self.db.query("INSERT INTO annotations (note, soul_id_1, soul_id_2, inserted_at) VALUES " + 
			"('" + _note + "'," + soul_id_1 + "," + soul_id_2 + ", NOW())")
		return self.db.insert_id()

	def get_annotations(self, soul):
		'''Retrieve annotations for a specific soul'''
		soul_id = soul['id']
		self.db.query("SELECT note, inserted_at FROM annotations " +
			#"WHERE soul_id_1='" + soul_id + "' OR soul_id_2='" + soul_id + "' " +
			"WHERE soul_id_1='" + soul_id + "' AND soul_id_2 IS NULL " +
			"ORDER BY inserted_at ASC")
		r = self.db.store_result()
		row = r.fetch_row(0)
		res = []
		for i in row:
			res.append( { 'text':i[0], 'date':i[1] } )
		return res

	def get_cousins(self, soul):
		'''Retrieve cousins for a specific soul'''
		soul_id = soul['id']
		self.db.query("SELECT note, inserted_at, soul_id_1, soul_id_2 FROM annotations " +
			"WHERE (soul_id_1='" + soul_id + "' AND NOT soul_id_2 IS NULL) OR " +
			"soul_id_2='" + soul_id + "' " +
			"ORDER BY inserted_at ASC")
		r = self.db.store_result()
		row = r.fetch_row(0)
		res = []
		for i in row:
			if i[2]==soul['id']:
				soul_id = i[3]
			else:
				soul_id = i[2]
			res.append( self.get_soul_by_id(soul_id) )#{ 'text':i[0], 'date':i[1] } )
		return res

	def parentize(self, parent, child):
		'''Make soul_1 parent of soul_2'''
		parent_id = parent['id']
		child_id = child['id']
		self.db.query("INSERT INTO parent_soul_rel (soul_id, parent_soul_id) VALUES " +
			"('" + child_id + "','" + parent_id + "')")
	
	def get_parents(self, soul_id):
		'''Get parents of soul'''
		self.db.query("SELECT parent_soul_id FROM parent_soul_rel WHERE soul_id='" + soul_id + "'") 
		r = self.db.store_result()
		row = r.fetch_row(0)
		res = []
		for i in row:
			res.append( self.get_soul_by_id(i[0]) )
		return res

	def get_children(self, soul_id):
		'''Get children of soul'''
		self.db.query("SELECT soul_id FROM parent_soul_rel WHERE parent_soul_id='" + soul_id + "'") 
		r = self.db.store_result()
		row = r.fetch_row(0)
		res = []
		for i in row:
			res.append( self.get_soul_by_id(i[0]) )
		return res

# vi:ts=4
