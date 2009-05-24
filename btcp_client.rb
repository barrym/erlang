require 'rubygems'
require 'socket'
require 'timeout'

client = TCPSocket.new('localhost', '11300')
client.send("hello", 0)
timeout(5) do
  reply = client.gets
  puts reply
end
client.close
