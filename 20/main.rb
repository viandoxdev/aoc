# frozen_string_literal: true

require 'net/http'

def fetch_input
  session = File.read('../session').chomp
  uri = URI('https://adventofcode.com/2022/day/20/input')
  req = Net::HTTP::Get.new(uri)
  req['Cookie'] = "session=#{session}"

  res = Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
    http.request req
  end

  res.body
end

def input
  File.read('input')
rescue Errno::ENOENT
  input = fetch_input
  File.write('input', input)
  input
end

def parse(str)
  str.split("\n").map(&:to_i)
end

def solve(input, rounds, part)
  arr = parse(input)
  arr.map! { |x| x * 811_589_153 } if part == 2

  indices = (0..(arr.length - 1)).to_a

  mod = arr.length - 1
  len = arr.length

  puts "[#{part}] Start"

  rounds.times do |r|
    indices.each do |from|
      value = arr[from]
      to = (from + value) % mod
      indices.map! do |x|
        if x == from
          to
        else
          x -= 1 if x >= from
          x += 1 if x >= to
          x % len
        end
      end
      arr.delete_at(from)
      arr.insert(to, value)
    end
    puts "[#{part}] Round #{r + 1} out of #{rounds}"
  end

  puts "[#{part}] Finished"
  puts ''

  zero_index = arr.find_index(0)
  arr[(zero_index + 1000) % len] + arr[(zero_index + 2000) % len] + arr[(zero_index + 3000) % len]
end

inp = input
part1 = solve(inp, 1, 1)
part2 = solve(inp, 10, 2)

puts "Part1: #{part1}"
puts "Part2: #{part2}"
