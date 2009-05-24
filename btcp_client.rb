#require 'rubygems'
require 'socket'
require 'timeout'

module BTCP
  class Client
    
    def initialize
      @client = TCPSocket.new('localhost', 11300)
    end

    def add(data)
      send("ADD #{data}")
      get_reply == "OK"
    end

    def reserve
      send("RESERVE")
      get_reply
    end

    def kill
      send("kill")
      get_reply
    end

    private

    def send(command)
      @client.send("#{command}\r\n", 0)
    end

    def get_reply
      reply = nil
      timeout(5) do
        reply = @client.gets
      end
      reply.chomp
    end

  end
end
