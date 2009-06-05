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

namespace :test do
  desc "Run the dateutils tests"
  task :dateutils => :compile do
    sh "erl -pa ebin -s date_utils test -s init stop"
  end
end
