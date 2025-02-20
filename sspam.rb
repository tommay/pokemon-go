#!/usr/bin/env ruby

# Read a spamminess file and sort it by dps/turns_to_charge.  Not sure
# if this is a good metric or not.  Maybe it should be
# turns_to_charge^2 or something.

s = File.open(ARGV[0]) do |f|
  f.sort_by do |line|
    line =~ /^([0-9]+)\([0-9]+\) +([^ ]+)/
    dps = $2.to_f
    turns_to_charge = $1.to_f
    dps / (turns_to_charge - 0) ** 1
  end
end

puts s.reverse.join rescue nil # Ignore broken pipe errors on writes.
