#!/usr/bin/perl

# Print summary counts for a file

while (<>) {
   chomp();
   $_ =~ s/[1-9]/1/g;
   my ($topic, $file, $explicit, $org, $other, $person, $place, $future, $generic, $indirect, $periodic, $specific, $acf, $dps, $temporal) = split /,/, $_;

   $rows++;
   $dates += $explicit;
   $orgs += $org;
   $others += $other;
   $persons += $person;
   $places += $place;
   $futures += $future;
   $generics += $generic;
   $indirects += $indirect;
   $periodics += $periodic;
   $specifics += $specific;
   $temporals += $temporal;
}

print "$rows, $dates, $orgs, $others, $persons, $places, $futures, $generics, $indirects, $periodics, $specifics, $temporals\n";
