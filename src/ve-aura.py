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
		if len(self.args)<1:
			self.usage()
		self.command = self.args[0]

		if self.command == 'annotate':
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

	def usage(self):
		print """
    Usage: ve-aura [options] <command> ...

    Commands:
    annotate <FILE1> [<FILE2>]  Add annotation to FILE1 (and eventually to FILE2 too)
    parentize <FILE> <CHILD>    Make FILE file parent of CHILD file.

    Options:
    -h, --help                  Show usage
		"""
		sys.exit(2)



# Main program

if __name__ == "__main__":
	opts = Options(sys.argv[1:])
	ve = Ve()
	ve.connect("db.virtualentity.org")

	if opts.command == 'parentize':
		print "Making " + opts.input + " parent of " + opts.input2
		ve.parentize(opts.input, opts.input2)

	if opts.command == 'annotate':
		print "Adding annotation for " + opts.input,
		if opts.input2:
			print " and " + opts.input2
		else:
			print
		print "Insert annotation text:",
		text = raw_input()
		ve.add_annotation(text, opts.input, opts.input2)

# vi:ts=4
