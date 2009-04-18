require 'rubygems'
require 'erlectricity'
require 'stringio'

@count = 0
puts "aa"

receive do |f|
  f.when(:echo, String) do |text|
    @count += 1
    f.send!(:result, "You said #{text} #{@count} times")
    f.receive_loop
  end
end
