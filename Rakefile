require 'rake/clean'

INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include("ebin/*.beam")

directory 'ebin'

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "Compiles all .erl files in src to .beam files in ebin"
task :compile => ['ebin'] + OBJ
task :default => :compile

namespace :btcp do
  desc "Starts btcp server"
  task :start do
    sh "erl -pa ebin -run btcp start -noshell"
  end
end

desc "Run all tests" 
task :test => :compile do
  modules = FileList['src/**/*.erl'].exclude(/_tests.erl$/).map {|file| File.basename(file, ".erl")}
  sh "erl -noshell -pa ebin -eval 'eunit:test([#{modules.join(",")}], [verbose])' -s init stop"
end
