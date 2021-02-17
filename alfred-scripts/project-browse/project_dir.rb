#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'json'

CACHE_LOCATION = '~/.cache/project_browse_cache.json'
PROJECTS_DIR_ROOT = '~/pj/'

cache_path = File.expand_path(CACHE_LOCATION)
FileUtils.touch(cache_path) unless File.exist?(cache_path)

cache = File.new(cache_path, 'r+')
cached_data = cache.read

cache_empty = cached_data.size.zero?

now = Time.now.to_i
last_time = cache.mtime.to_i

diff_in_minutes = (now - last_time) / 60

$stderr.write "Diff: ##{diff_in_minutes}"

if cache_empty || diff_in_minutes > 4 * 60 # if 4 hours old, rebuild the cache
  $stderr.write 'cache is old, recreating'
  root = File.expand_path(PROJECTS_DIR_ROOT)

  list =  Dir["#{root}/**/.git"].map { |x| File.dirname x }

  items = list.map do |path|
    name = File.basename(path)
    {
      uid: name,
      title: name,
      subtitle: '',
      arg: path,
      autocomplete: name,
      icon: { type: 'fileicon', path: path }
    }
  end

  json = { items: items }.to_json

  cache.write json

  cached_data = json
end
cache.close

puts JSON.parse(cached_data).to_json
