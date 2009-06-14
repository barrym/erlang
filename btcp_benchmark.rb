#require 'rubygems'
require 'socket'
require 'timeout'
require 'btcp_client'

amount = (ARGV.pop || 15_000).to_i

threads = []

5.times do
  threads << Thread.new do
    client = BTCP::Client.new
    started = Time.now
    amount.times do |n|
      client.add(n)
    end
    ended = Time.now
    taken = ended - started
    puts "Added : #{amount} in #{taken} seconds = #{amount/taken}/s"
  end
end

5.times do
  threads << Thread.new do 
    client = BTCP::Client.new
    started = Time.now
    amount.times do |n|
      client.reserve
    end
    ended = Time.now
    taken = ended - started
    puts "Taken : #{amount} in #{taken} seconds = #{amount/taken}/s"
  end
end

threads.each {|t| t.join}
