#require 'rubygems'
require 'socket'
require 'timeout'
require 'btcp_client'

amount = 100_00
client = BTCP::Client.new
started = Time.now
amount.times do |n|
  client.add(n)
end
ended = Time.now
taken = ended - started
puts "Added : #{amount} in #{taken} seconds = #{amount/taken}/s"

started = Time.now
amount.times do |n|
  client.reserve
end
ended = Time.now
taken = ended - started
puts "Taken : #{amount} in #{taken} seconds = #{amount/taken}/s"
