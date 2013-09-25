notr
====

A simple note taker.

Introduction
============

The aim of Noter is to create a simple text editor for taking notes. It should have a range of appropriate affordances that support this task. 

Details
========
User interface:
  * purely keyboard driven
  * text, gui or web interfaces
  * fast, incremental search with ranked results
  * modeless maybe
  * follow the principle of instant-declaration once I work out what that is.



Query/title box
===============

- typing searches and displays a list of matching notes (matched by title/query)
- on enter creates new or edits existing text note
- what does enter do to the altenate matches in the list box?

list box
========

- list of matching items based on current query
- clicking an item selects that as the current note
- this does not change the query to alow switching between matching notes

text% box
=========

- text of currently selected item
- saved on every keypress
- ?undo
- pressing enter gets a new line
? tab moves back to search box
? shift tab back to list box
