#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'json'

CACHE_LOCATION = '~/.cache/project_browse_cache.json'
PROJECTS_DIR_ROOT = '~/pj/'

CACHE_PATH = File.expand_path(CACHE_LOCATION)
FileUtils.mkdir_p File.expand_path '~/.cache'
FileUtils.touch(CACHE_PATH) unless File.exist?(CACHE_PATH)

cached_data = File.read(CACHE_PATH)

cache_empty = cached_data.size.zero?

now = Time.now.to_i
cache = File.open(CACHE_PATH)
last_time = cache.mtime.to_i
cache.close

diff_in_minutes = (now - last_time) / 60

$stderr.write "Diff: ##{diff_in_minutes}"

if cache_empty || diff_in_minutes > 24 * 60 # if 24 hours old, rebuild the cache
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

  File.write(CACHE_PATH, json)

  cached_data = json
end

puts JSON.parse(cached_data).to_json
