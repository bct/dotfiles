#!/usr/bin/env ruby

require 'time'

xdh = ENV['XDG_DATA_HOME'] + '/uzbl'
fn  = xdh + '/stack.html'

uri   = ENV['UZBL_URI']
title = ENV['UZBL_TITLE']

entry = "<li><span class='font-family: monospace'>#{Time.now.iso8601[0,10]}</span> <a href='#{uri}'>#{title}</a>"

File.open(fn, 'a') do |f|
  f.puts entry
end
