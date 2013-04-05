# This script takes a tab delimited spreadsheet and a set of rules
# and rewrites the spreadsheet according to the rules. It is expected
# that the first line of the spreadsheet are headers and that all cells
# including the headers are in double quotes.

# Rules act by checking whether a column header matches and if so, rewrites 
# the cell according to the cell pattern and rewrite

# Every column name is checked against all the column patterns in the
# order listed in the rule file.  Any time a column matches the cell is matched
# and possibly rewritten. This may occur more than once if there are
# multiple rows with the same column.

# all rules are expected to match the full contents of the cell and
# are case insensitive.  For convenience, the (?i) and "^" and "$" are
# added by the program before matching. 
# 
# Rule file format:
# columns are separated by whitespace, and values need to be enclosed in double quotes (").
#  column pattern - pattern to match for column name 
#  match pattern - pattern to match against cell (in matched column)
#  rewrite - rewrite expression, assuming matched cell. $n is group number
#
# syntax is as in perl regular expression. http://perldoc.perl.org/perlre.html
#
# e.g 
# "procclass\d+"  "(\S+)(.*?)procedure"				"$1\_procedure" 
#
# This rule works on columns that are named procclass<some number>. If the cell
# ends with procedure, then it is rewritted to be the first word and "procedure" 
# separated by an underscore "_".
#
# The script is run with two arguments and writes to standard output
# Arg1: filename of the rules
# Arg2: filename of the SAS data
# Arg3: filename of the SAS code file
#
# e.g.  perl rewrite-spreadsheet.pl myrules.txt mySASdata.txt mySAScode.sas > myrewritten.txt


use strict;

open(my $patternfile,"<",$ARGV[0]) or usage();
open(my $datafile,"<",$ARGV[1]) or usage();
open(my $sasfile,"<",$ARGV[2]) or usage();
my @pats;
my ($columnpat,$cellpat,$cellrewrite);

sub compilePatterns {
  while (<$patternfile>) {
    chomp;
    #  print "processing \"$_\"\n\n";
    next if (/^#/ || /^\s*$/); # skip blank lines or lines starting with "#"
    my $beforeQuoteClean = $_;
    s/^\s*"\s*//; s/\s*"\s*$//; # remove leading or trailing quotes
    ($columnpat,$cellpat,$cellrewrite) = split /"\s+"/; # split columns by space between quotes
    # compile the patterns by evaluating a subrouting which we will call to match or replace
    my $columnChecker = "sub { @\_[0] =~ m/(?i)^\"{0,1}$columnpat\"{0,1}\$/}";
    my $cellRewriter =  "sub { \$a = @\_[0]; \$a =~ s/(?i)^\"$cellpat\"\$/\"$cellrewrite\"/; \$a}";
    push @pats,[ ( eval $columnChecker), # be permissive about quotes around headers
		 (eval  $cellRewriter ),
	       $columnChecker, $cellRewriter,$columnpat,$cellpat,$cellrewrite,$beforeQuoteClean,$_]; # for debugging
  }
}

sub applyPatterns
  { my @headers = getheaders();
    map { if (/^".*"$/) { $_ } else { s/(.*)/"$1"/; }} @headers;
    #print join(",",@headers) . "\n"; # use this to have the first line contain the column names

    my $numberColumns = scalar(@headers); # determine the number of columns
    while (<$datafile>) {
      next if (/^#/ || /^\s*$/);
      chomp;
      my $columnCounter = 0;
      #my @cells = split(/\t/);
      my @cells = csvsplit($_); # or csvsplit($line, $my_custom_seperator)
      #print @cells;
      for my $cell (@cells)
	{ for my $patternEntry (@pats)
	    {
	      if ($patternEntry->[0]->(@headers[$columnCounter]))
		{ $cell = $patternEntry->[1]->($cell) }
	    }

	  $cell =~ s/\s/_/g; # substitute spaces with underscores
       	  $columnCounter++;
	
	  ## if we are on the last column don't print comma
	  if ($columnCounter == $numberColumns) {
	    print "$cell";
	  } else {
	    print "$cell,";
	  }
	}
      print "\n";
    }
  }


sub getheaders {
  my $index = 0;
  my $headerflag = 0;
  my @headers = [];

  while (my $line = <$sasfile>) {
    # if header flag is on, put variable names in header list
    if ($headerflag == 1) {
      chomp($line);
      $line =~ s/^\s+|\s+$//g ;     # remove both leading and trailing whitespace
      @headers[$index] = "\"$line\""; # header name should be in quotes
      $index++;
    }

    # when we see the line "INPUT" start collecting headers
    if ($line =~ /INPUT/) {
      $headerflag = 1;
    }

    # when we see the line that ends with a "$" stop collecting headers
    if (($headerflag == 1) && ($line =~ /(.|\s)*\$/)) {
      $headerflag = 0;

      # remove '$' from end of line by searching up the first word boundary
      $line =~ s/^\s+|\s+$//g ;     # remove both leading and trailing whitespace
      my ($temp) = $line =~ m/(.*\b)/;
      @headers[$index - 1] = "\"$temp\""; # header name should be in quotes
    }
  }

  return @headers;
}

# found at: http://stackoverflow.com/questions/3065095/how-do-i-efficiently-parse-a-csv-file-in-perl
sub csvsplit {
        my $line = shift;
        my $sep = (shift or ',');

        return () unless $line;

        my @cells;
        $line =~ s/\r?\n$//;
	my $re = qr/(?:^|$sep)(?:"([^"]*)"|([^$sep]*))/;

        while($line =~ /$re/g) {
	  # my $value = defined $1 ? $1 : $2;
	  my $value = defined $1 ? "\"$1\"" : "\"$2\"";
	  push @cells, (defined $value ? $value : '');
        }

        return @cells;
}

compilePatterns();
applyPatterns();

1;

sub usage 
  { print STDERR "usage: rewrite-spreadsheet.pl pattern-file data-file sas-code-file\n"; exit(); }

