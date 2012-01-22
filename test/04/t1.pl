#!/usr/bin/perl
#
# gets result from rpl. Extracts plate, point size, ccl result. Performs
# ccl8 for every possible point. Compares input ccl result and PDL ccl8.
#
use strict;
use FileHandle;
use Data::Compare;
use Data::Dumper;
use Getopt::Long;
use lib "/home/user6/util/perl/dist/lib/perl/5.14.2/";
use PDL;
use PDL::Image2D;

use vars qw($verbose $exit_mm);

exit runall();

sub runall {
my $infile = '-';
my $outfile = '-';
$verbose = 0;
$exit_mm = 0;
GetOptions (
	"i|infile=s" => \$infile,
	"o|outfile=s" => \$outfile,
	"e|exit!" => \$exit_mm, # exit on any mismatch. Otherwise process all data
	"v|verbose=i" => \$verbose
);

my $fdi = new FileHandle "<$infile";
unless($fdi){
	print STDERR "can't open input file '$infile': $!\n";
	return 1;
}
my $fdo = new FileHandle ">$outfile";
unless($fdo){
	print STDERR "can't open output file '$outfile': $!\n";
	return 1;
}
my $flag = 0;
my $item = '';
my $rc = 2; # "no items" at the beginning
while(my $str = $fdi->getline()) {
	$flag=1 if($str =~ /^main:/);
	$item .= $str if $flag;
	if($str =~ /^main end/ && $flag){
		$rc = get_one_plate($fdo, $item);
		return $rc if $rc && $exit_mm;
		$item = '';
		$flag = 0;
	}
}
close $fdi;
close $fdo;
return $rc;
}

sub fill_src_data {
my($text) = @_;
my @data;
my @arr = split /[\r\n]+/, $text;
my @a2 = grep /\d/, @arr;
for my $str (@a2){
	my @c = $str =~ /(\d+)/g;
	push @data, \@c;
}
return \@data;
}

sub fill_one_item {
my($text, $all_items) = @_;
return if $text !~ /cell:\s*(\d+)/;
my $cur_cell = $1;
my($array) = $text =~ /dump_one_ccl, labels:(.*)main, ccl result, done/s;
my @arr2 = split /[\r\n]+/, $array;
for my $str (@arr2){
	next unless $str;
	next if $str =~ /[^\d\s_]/;
	my @str = $str =~ /([\d_]+)/g;
	next unless @str;
	map { $_ = -1 if $_ eq '_' ; $_ = int($_) + 1 } @str;
	push @{$all_items->{$cur_cell}}, \@str;
}
}

sub get_one_plate {
my($fdo, $item) = @_;
my($psz) = $item =~ /main:\s+\d+,\s+\d+,\s+(\d+)/;
my($src) = $item =~ /^begin data:(.*?)main, ccl result:/ms;
my @items = $item =~ /(main, ccl result, cell:.*?main, ccl result, done)/gs;
my $src_data = fill_src_data($src);
my %all_items;
for my $cur_item (@items){
	fill_one_item($cur_item, \%all_items);
}
return cmp_data($fdo, $src_data, \%all_items);
}

sub cmp_data {
my($fdo, $src_data, $src_res) = @_;
my $res = 3;
for my $cell (sort {$a<=>$b} keys %$src_res){
	print $fdo "cell: $cell\n" if $verbose > 2;
	my $cur_src = make_src_data($src_data, $cell);
	my $cur_src_p = pdl(@$cur_src);
	my $res_cc8 = cc8compt($cur_src_p);
	my $src_p = pdl(@{$src_res->{$cell}});
	print $fdo "cc8:\n" . $res_cc8 . "\n" if $verbose > 4;
	print $fdo "src:\n" . $src_p   . "\n" if $verbose > 4;
	my @cc8_list = $res_cc8->list;
	my @src_list = $src_p->list;
	if(compare($fdo, \@cc8_list, \@src_list)){
		print $fdo "matched data\n" if $verbose > 0;
		$res = 0;
	}else{
		print $fdo "not matched data\n" if $verbose > 0;
		print $fdo "input data:\n"   . $cur_src_p . "\n" if $verbose > 3;
		print $fdo "cc8 result:\n"   . $res_cc8   . "\n" if $verbose > 3;
		$res = 1;
		return $res if $exit_mm;
	}
}
return $res;
}

# fill the data with 1's for the given cell and 0's for everything other
sub make_src_data {
my($src_data, $cell) = @_;
my @res;
for my $row (@$src_data){
	my @r2 = @$row;
	map { $_ = ($_ == $cell) ? 1 : 0 } @r2;
	push @res, \@r2;
}
return \@res;
}

sub compare {
my($fdo, $cc8_list, $src_list) = @_;
return if @$cc8_list != @$src_list; # compare length only
my $ids_c = fetch_cells2($cc8_list);
my $ids_s = fetch_cells2($src_list);
my(@checked_c, @checked_s);
for(my $i = 0; $i < @$cc8_list && $i < @$src_list; $i++){
	print $fdo "i=$i\n" if $verbose > 5;
	my $label_c = $cc8_list->[$i];
	my $label_s = $src_list->[$i];
	print $fdo "label c: $label_c\n" if $verbose > 5;
	print $fdo "label s: $label_s\n" if $verbose > 5;
	next if $checked_c[$label_c] && $checked_s[$label_s];
	my $cell_c = $ids_c->{$label_c};
	my $cell_s = $ids_s->{$label_s};
	print $fdo "cell_c:\n" . Dumper($cell_c) . "\n" if $verbose > 6;
	print $fdo "cell_s:\n" . Dumper($cell_s) . "\n" if $verbose > 6;
	return unless Data::Compare::Compare($cell_s, $cell_c);
	$checked_c[$label_c] = $cell_c;
	$checked_s[$label_s] = $cell_s;
}
return 1;
}

# store array indices in a hash with the label as a key
sub fetch_cells2 {
my($list) = @_;
my %dat;
for(my $i=0; $i < @$list; $i++){
	my $label = $list->[$i];
	my $ids = $dat{$label} || [];
	push @$ids, $i;
	$dat{$label} = $ids;
}
return \%dat;
}
