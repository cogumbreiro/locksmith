#!/usr/bin/ruby

require 'timeout'

$cilly = "../cil/bin/cilly"

with_lib = [
#  "aget_comb.c",
#  "ctrace_comb.c",
#  "pfscan_comb.c",
#  "engine.c",
#  "smtprc_comb.c",
]

with_lib_exists = [
  "knot_comb.c",
]

without_lib = [
#  "consolemap_comb.c",
#  "ide-disk_comb.c",
#  "serial_core_comb.c",
#  "plip.ko_merged.c",
#  "eql.ko_merged.c",
#  "3c501.ko_merged.c",
]

without_lib_nolin = [
#  "esp-2.5.62.c",
#  "synclink.ko_merged.c",
#  "sis900.ko_merged.c",
#  "slip.ko_merged.c",
#  "hp100.ko_merged.c",
]

def cil_merge(files, out)
  ENV["CILLY_DONT_COMPILE_AFTER_MERGE"] = "1"
  ENV["CILLY_DONT_LINK_AFTER_MERGE"] = "1"
  cmd = "#{$cilly} --merge --mergedout=#{out} --dontlocksmith " +
    files.join(" ") + " > /dev/null 2>&1"
  status = system(cmd)
  ENV.delete("CILLY_DONT_COMPILE_AFTER_MERGE")
  ENV.delete("CILLY_DONT_LINK_AFTER_MERGE")
  if !status
    raise "Merge error: #{$?}"
  end
end

def locksmith(file, flags)
  start = Time.new
  cmd = "#{$cilly} -c --list-guardedby --list-shared #{flags}" +
    " #{file} > out/#{file}.out 2>&1"
  status = 0
  begin
    timeout(1200) {
      status = system(cmd)
    }
  rescue TimeoutError
    return "timeout"
  end
  finish = Time.new
#  if !status
#    return false
#  end
  races = `grep "Possible data race" out/#{file}.out | wc -l`
  races.chomp!
  return [finish - start, races]
end

def print_locksmith(file, flags)
  printf "#{file} #{flags}... "
  result = locksmith(file, flags)
  if result == false then
    print "FAILED\n"
  elsif result == "timeout" then
    print "TIMEOUT\n"
    result = [1200, 9999];
  else
    print "#{result[1]} warnings, #{result[0]}s\n"
  end
  return result
end

def run_three_times(file, flags)
  result_times = []
  result_warnings = []
  for i in 0..6   # Change this if you want more iterations
    result_times[i],result_warnings[i] = print_locksmith(file, flags)
  end
  for i in 0..5   # Change this too
    if result_warnings[i] != result_warnings[i+1]
      print "WARNING:  DIFFERENT WARNING COUNTS\n"
      break;
    end
  end
  result_times.sort!
  return [result_times[3], result_warnings[0]]   # And change the 3 to whatever the middle elt is
end

$main = sprintf("%30s & %5s & %8s & %4s & %4s & %4s \\\\\n",
               "Benchmark", "LOC", "Time (s)", "Warn", "Warn", "Warn")

$flags = sprintf("%30s        & %8s & %8s & %8s & %8s & %8s \\\\\\hline\n",
                "Benchmark", "All off", "Void on", "DownF on", "Uniq on", "Exist on")

$jg_warn = ""
$jg_warn_perc = ""
$jg_time = ""

def try_variations(file, base_flags, try_exists)
  none = run_three_times(file, "--no-void --no-down-fork --no-uniqueness --no-existentials #{base_flags}")
  no_due = run_three_times(file, "--no-down-fork --no-uniqueness --no-existentials #{base_flags}")
  no_ue = run_three_times(file, "--no-uniqueness --no-existentials #{base_flags}")
  no_e = run_three_times(file, "--no-existentials #{base_flags}")
  if try_exists then
    all = run_three_times(file, "")
  else
    all = no_e
  end

  real_name = file;
  ind = real_name.index('_');
  if ind then
    real_name = real_name[0..ind-1]
  end
  ind = real_name.index('.');
  if ind then
    real_name = real_name[0..ind-1]
  end

  $main = $main +
    sprintf("%30s & %5s & %8.1f & %4d & %4d & %4d\n", real_name, "XXX", all[0], all[1], all[1], all[1])

  $flags = $flags +
    sprintf("%30s (warn) & \\textbf{%8d} & \\textbf{%8d} & \\textbf{%8d} & \\textbf{%8d} & \\textbf{%8d} \\\\\n", real_name,
            none[1], no_due[1], no_ue[1], no_e[1], all[1]) +
    sprintf("%30s (time) & %5.1fs & %5.1fs & %5.1fs & %5.1fs & %5.1fs \\\\ \\hline\n", "",
            none[0], no_due[0], no_ue[0], no_e[0], all[0])


#  $jg_warn = $jg_warn +
#    sprintf("%s | %8d | %8d | %8d | %8d | %8d\n", real_name,
#            none[1], 
#            if no_due[1] == no_ue[1] then 0 else no_due[1] end,
#            if no_ue[1] == no_e[1] then 0 else no_ue[1] end,
#            if no_e[1] == all[1] then 0 else no_e[1] end,
#            all[1])

  $jg_warn_perc = $jg_warn_perc +
    sprintf("%s | %5.f | %5.f | %5.f | %5.f | %5.f\n", real_name,
            100, 
            ((if no_due[1] == no_ue[1] then 0 else no_due[1] end).to_f/none[1].to_f)*100,
            ((if no_ue[1] == no_e[1] then 0 else no_ue[1] end).to_f/none[1].to_f)*100,
            ((if no_e[1] == all[1] then 0 else no_e[1] end).to_f/none[1].to_f)*100,
            (all[1].to_f/none[1].to_f)*100)

#  $jg_time = $jg_time +
#    "newcurve\n" +
#    sprintf("pts 5 %5f 4 %5f 3 %5f 2 %5f 1 %5f\n",
#            none[0], no_due[0], no_ue[0], no_e[0], all[0]) +
#    sprintf("label : %s\n", real_name)
end

with_lib.each { |file|
  merged_file = file.sub("\.c", "_lib.c")
  cil_merge([file, "lib.c"], merged_file)
  try_variations(merged_file, "", false)
  File.unlink merged_file
}

with_lib_exists.each { |file|
  merged_file = file.sub("\.c", "_lib.c")
  cil_merge([file, "lib.c"], merged_file)
  try_variations(merged_file, "", true)
  File.unlink merged_file
}

without_lib.each { |file|
  try_variations(file, "", false)
}

without_lib_nolin.each { |file|
  try_variations(file, "--no-linearity", false)
}

print "\n\n"
print $main
print "\n\n"
print $flags
#print "\n\n"
#print $jg_warn
print "\n\n"
print $jg_warn_perc
#print "\n\n"
#print $jg_time
