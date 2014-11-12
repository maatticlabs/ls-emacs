;-*- coding: utf-8 -*-
 
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-command
;;;;
;;;; Purpose
;;;;    Add commands to lse-command completion
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (of comment)
;;;;    26-Aug-1994 (CT) swing-kartei:*:change-directory added
;;;;--
(provide 'swing-command)

(require 'lse-command)

(lse-command:add "bib add"                    'swing-kartei:bib:add)
(lse-command:add "bib change"                 'swing-kartei:bib:change)
(lse-command:add "bib change directory"       'swing-kartei:bib:change-directory)
(lse-command:add "bib make-summary"           'swing-kartei:bib:make-summary)
(lse-command:add "bib sort"                   'swing-kartei:bib:sort)
(lse-command:add "firma add"                  'swing-kartei:firma:add)
(lse-command:add "firma change"               'swing-kartei:firma:change)
(lse-command:add "firma change directory"     'swing-kartei:firma:change-directory)
(lse-command:add "firma make-summary"         'swing-kartei:firma:make-summary)
(lse-command:add "firma add"                  'swing-kartei:firma:add)
(lse-command:add "person add"                 'swing-kartei:person:add)
(lse-command:add "person change"              'swing-kartei:person:change)
(lse-command:add "person change directory"    'swing-kartei:person:change-directory)
(lse-command:add "person make-summary"        'swing-kartei:person:make-summary)
(lse-command:add "person sort"                'swing-kartei:person:sort)
(lse-command:add "quotation add"              'swing-kartei:quot:add)
(lse-command:add "quotation change"           'swing-kartei:quot:change)
(lse-command:add "quotation change directory" 'swing-kartei:quot:change-directory)
(lse-command:add "quotation make-summary"     'swing-kartei:quot:make-summary)
(lse-command:add "quotation sort"             'swing-kartei:quot:sort)
