use strict;
use warnings;
use LWP::UserAgent;
use Class::Struct Entry => [sx => '$', sy => '$', bx => '$', by => '$', rad => '$', left => '$', right => '$'];
use Class::Struct Range => [start => '$', end => '$'];

sub dist {
	return abs($_[0] - $_[2]) + abs($_[1] - $_[3]);
}

sub part1 {
	my $input = $_[0];
	my @lines = split('\n', $input);
	my $row = 2000000;
	my %beacons = ();
	my %seen = ();

	foreach my $line (@lines) {
		my @fields = split(/[=,:]/, $line);
		my $sx = $fields[1] + 0;
		my $sy = $fields[3] + 0;
		my $bx = $fields[5] + 0;
		my $by = $fields[7] + 0;
		my $rad = dist($sx, $sy, $bx, $by);
		my $left = $sx - $rad + abs($row - $sy);
		my $right = $sx + $rad - abs($row - $sy);
		next if ($left > $right);
		@seen{$left..$right} = ();
		$beacons{$bx} = () if ($by == $row);
	}
	my $size = keys %seen;
	my $bsize = keys %beacons;
	return $size - $bsize;
}

sub part2 {
	my @entries = ();
	# parse input, compute radius
	foreach my $line (split('\n', $_[0])) {
		my @fields = split(/[=,:]/, $line);
		my $e = new Entry;
		$e->sx($fields[1] + 0);
		$e->sy($fields[3] + 0);
		$e->bx($fields[5] + 0);
		$e->by($fields[7] + 0);
		$e->rad(dist($e->sx, $e->sy, $e->bx, $e->by));
		$e->left($e->sx - $e->rad);
		$e->right($e->sx + $e->rad);
		push(@entries, $e);
	}
	my $x = -1;
	my $y = -1;
	foreach my $i (0..4000000) {
		print "$i/4000000";
		my $row = 4000000 - $i;
		my @ranges = ();
		foreach my $e (@entries) { # loop over sensors
			my $dy = abs($row - $e->sy);
			next if ($dy > $e->rad); # skip if sensor doesn't reach row
			
			# build range
			my $range = new Range;
			$range->start($e->left + $dy);
			$range->end($e->right - $dy);

			# ignore if range is out of bounds
			next if ($range->end < 0 || $range->start > 4000000);

			# normalize range in 0..4000000
			$range->start(0) if ($range->start < 0);
			$range->end(4000000) if ($range->end > 4000000);

			push(@ranges, $range);
		}
		# sort ranges before merging
		@ranges = sort {$a->start <=> $b->start} @ranges;

		my @merged = (shift @ranges);
		foreach my $range (@ranges) {
			my $last = $merged[$#merged];
			# if the current range starts before or exactly one after the last range, we can merge
			if ($range->start <= $last->end + 1) {
				$merged[$#merged]->end($range->end) if ($range->end > $last->end);
			} else {
				# we can't merge, start a new range
				push(@merged, $range);
			}
		}

		# print
		foreach my $range (@merged) {
			my $start = $range->start;
			my $end = $range->end;
			print " $start..$end"
		}
		
		# because there is only one valid location in the allowed range, we know
		# that it is in the only place where we have two ranges (instead of 0..4000000)
		if (@merged > 1) {
			# the two ranges are exactly one apart, get that
			$x = $merged[0]->end + 1;
			$y = $row;
			print "\n";
			last;
		}

		print "\n";
	}

	if ($x == -1 || $y == -1) {
		print "Failed\n";
	}

	# compute tuning
	return $x * 4000000 + $y;
}

# grab session from file
open(my $fh, '<', '../session'); 
my $session = <$fh>;
chomp($session);

# send request
my $ua = LWP::UserAgent->new();
my $response = $ua->get('https://adventofcode.com/2022/day/15/input', 'Cookie', "session=$session");
my $input = $response->decoded_content;

my $res1 = part1($input);
my $res2 = part2($input);
print "\n";
print "Part1: $res1\n";
print "Part2: $res2\n";
