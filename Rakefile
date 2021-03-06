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

namespace :bmtcp do
  desc "Starts bmtcp server"
  task :start do
    # sh "erl -pa ebin -run bqueue_mtcp start -noshell"
    sh "erl -pa ebin -run bqueue_mtcp start"
  end
end

desc "Compiles all .erl files in src to .beam files in ebin"
task :compile_with_tests do
  FileList['src/**/*.erl'].each do |src|
    sh "erlc -D EUNIT -pa ebin -W #{ERLC_FLAGS} -o ebin #{src}"
  end
end

desc "Run all tests" 
task :test  => :compile_with_tests do
  modules = OBJ.map {|o| File.basename(o, ".beam") }
  output = `erl \
            -noshell \
            -pa ebin \
            -eval 'eunit:test([#{modules.join(",")}], [verbose])' \
            -s init stop`

  output.each_line do |line|
    case line
    when /= (EUnit) =/
      print line.gsub($1, green($1))
    when /\*failed\*/
      print red(line)
    when /(\.\.\..*ok)/
      print line.gsub($1,green($1))
    when /Failed:\s+(\d+)\.\s+Skipped:\s+(\d+)\.\s+Passed:\s+(\d+)\./
      puts "#{red("Failed: #{$1}")} Skipped: #{$2} #{green("Passed: #{$3}")}"
    when/(All \d+ tests passed.)/
      print green(line)
    else
      print line
    end
  end
end

def green(text)
  "\e[32m#{text}\e[0m"
end

def red(text)
  "\e[31m#{text}\e[0m"
end
