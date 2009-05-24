#require 'rubygems'
require 'socket'
require 'timeout'
require 'btcp_client'

amount = 100
client = BTCP::Client.new
started = Time.now
amount.times do |n|
  client.add(n)
end
ended = Time.now
puts taken = ended - started
puts "Added : #{amount/taken}/s"

started = Time.now
amount.times do |n|
  client.reserve
end
ended = Time.now
puts taken = ended - started
puts "Taken : #{amount/taken}/s"
