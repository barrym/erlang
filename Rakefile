SRC = FileList["*.erl"]
OBJ = SRC.pathmap("%X.beam")

rule ".beam" => ".erl" do |t|
  puts "yo"
  puts t.name
end

task :compile => OBJ do
  puts "hello"
end

task :default => :compile


namespace :blib do

  desc "Starts blib list_server"
  task :start  => :compile do
    sh "erl -noshell -s blib start -s init stop"
  end
end
