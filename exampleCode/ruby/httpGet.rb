#!/usr/local/bin/ruby

##
# @file      getURL.rb
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2008 by Mitch Richling.  All rights reserved.
# @brief     How to act as a HTTP client via Net::HTTP.@EOL
# @Keywords  
# @Std       Ruby 1.8
#
#            Various ways to interact with web servers
#            Test this script with webServerWEBrickTEST.rb as the web server.

require 'uri'
require 'net/http'
require 'net/https'

# The easy way to do a GET (works with both http and https):
res = Net::HTTP.get_response(URI.parse('http://127.0.0.1:8080/test?foo=bar'))
puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")

# It works over https too:
res = Net::HTTP.get_response(URI.parse('https://127.0.0.1:8080/test?foo=bar'))
puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")

# The easy way to do a POST:
res = Net::HTTP.post_form(URI.parse('http://127.0.0.1:8080/test'), { "foo" => "bar" })
puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")

# We can open a connection, and then perform multiple actions.
Net::HTTP.start('127.0.0.1', 8080) do |http|
  res = http.get('/test?foo=bar')
  puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")
  
  res = http.get('/test?foo=boobar')
  puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")
end

# One can construct a request object for more control.  The URI object is a
# handy way to pass things around.  Net::HTTPRequest has a ton of subclasses for
# diffrent request types includeing: Net::HTTP::Get, Net::HTTP::Head,
# Net::HTTP::Post, Net::HTTP::Put, Net::HTTP::Proppatch, Net::HTTP::Lock,
# Net::HTTP::Unlock, Net::HTTP::Options, Net::HTTP::Propfind, Net::HTTP::Delete,
# Net::HTTP::Move, Net::HTTP::Copy, Net::HTTP::Mkcol, Net::HTTP::Trace.
aURL = URI.parse('http://127.0.0.1:8080/test?foo=bar')
req = Net::HTTP::Get.new(aURL.path)
req.basic_auth('user', 'password')
req.set_form_data({'boober'=>'bober'}, ';')
Net::HTTP.start(aURL.host, aURL.port) do |http|
  res = http.request(req)
  # It worked if res is an instance of Net::HTTPSuccess or Net::HTTPRedirection...
  if (Net::HTTPSuccess === res) || (Net::HTTPRedirection === res) then
    puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")
    # Net::HTTPSuccess provides other methods:
    #   .message.inspect -- a string. 'OK' for a good get
    #   .code.inspect -- a string. 200 for good get, 404 for a "not found", ...
    #   .code_type.class -- a result code object (Net::HTTPOK for a good get)
  else
    puts(res.error!.inspect)
  end
end

# Use HTTPS in a similar way:
aURL = URI.parse('https://127.0.0.1:8181/test?foo=bar')
req = Net::HTTP::Get.new(aURL.path)
req.basic_auth('user', 'password')
req.set_form_data({'boober'=>'bober'}, ';')
http = Net::HTTP.new(aURL.host, aURL.port)
http.use_ssl = true
http.start do
  res = http.request(req)
  if (Net::HTTPSuccess === res) || (Net::HTTPRedirection === res) then
    puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")
  else
    puts(res.error!.inspect)
  end
end

# Use the http_proxy environment variable (similar to curl, wget, & lynx)
proxyHost, proxyPort = if (ENV.member?('http_proxy')) then
                         aURL = URI.parse(ENV['http_proxy'])
                         [ aURL.host, aURL.port ]
                       else
                         [ nil, 80 ]
                       end
puts("proxy host: #{proxyHost.inspect}")
puts("proxy port: #{proxyPort.inspect}")

# This is how we can go through a proxy.  If proxyHost is nil, then
# Net::HTTP::Proxy returns Net::HTTP so code below still works.
Net::HTTP::Proxy(proxyHost, proxyHost).start('127.0.0.1', 8080) do |http|
  res = http.get('/test')
  puts("Received #{res.body.length} bytes\n  #{res.body.inspect}")
end
