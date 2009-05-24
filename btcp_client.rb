#require 'rubygems'
require 'socket'
require 'timeout'

module BTCP
  class Client
    
    def initialize
      @client = TCPSocket.new('localhost', 11300)
    end

    def add(data)
      @client.send("ADD #{data}\r\n", 0)
      get_reply
    end

    def reserve
      @client.send("RESERVE\r\n", 0)
      get_reply
    end

    private

    def get_reply
      reply = nil
      timeout(5) do
        reply = @client.gets
      end
      reply
    end

  end
end
