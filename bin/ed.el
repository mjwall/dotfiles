#!/usr/bin/env emacs --script
(print "Hi mike")
(require 'server)
(print (server-running-p))
