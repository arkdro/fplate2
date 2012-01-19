#!/usr/bin/perl
#
# gets result from rpl. Extracts plate, point size, ccl result. Performs
# ccl8 for every possible point. Compares input ccl result and PDL ccl8.
#
use strict;
use Data::Compare;
use Data::Dumper;
use PDL;
use PDL::Image2D;

runall();

sub runall {

my @data;
my $psz = 0;
my %all_src_data;
my $cur_cell = -1;
my @cur_data;
while(<>) {
	$psz = $1 if(/plate gen: psz=(\d+)/);
	if(/^begin data:/ .. /^$/){
		next unless /\bc=\d+/;
		my @str = $_ =~ /c=(\d+)/g;
		push @data, \@str;
	}
	if(/^pass2 done, cell/ .. /^pass2 dump done/){
		$cur_cell = $1 if /\bc=(\d+)/;
		next if /[^\d\s_]/;
		my @str = $_ =~ /([\d_]+)/g;
		map { $_ = -1 if $_ eq '_' ; $_ = int($_) + 1 } @str;
		push @{$all_src_data{$cur_cell}}, \@str;
	}
}
my $res = cc8compt(pdl(@data));
cmp_data(\@data, \%all_src_data);
}

sub cmp_data {
my($src_data, $src_res) = @_;
for my $cell (sort {$a<=>$b} keys %$src_res){
	print "cell: $cell\n";
	my $cur_src = make_src_data($src_data, $cell);
	my $cur_src_p = pdl(@$cur_src);
	my $res_cc8 = cc8compt($cur_src_p);
	if(compare($res_cc8, $src_res->{$cell})){
		print "matched data\n";
	}else{
		print "not matched data\n";
		print "input data:\n"   . $cur_src_p . "\n";
		print "cc8 result:\n"   . $res_cc8   . "\n";
	}
}
}

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
my($cc8, $src) = @_;
my @cc8_list = $cc8->list;
my $src_p = pdl(@$src);
my @src_list = $src_p->list;
print "cc8:\n" . $cc8 . "\n";
print "src:\n" . $src_p . "\n";
return if @cc8_list != @src_list; # compare length only
my(@checked_c, @checked_s);
for(my $i = 0; $i < @cc8_list && $i < @src_list; $i++){
	print "i=$i\n";
	my $label_c = $cc8_list[$i];
	my $label_s = $src_list[$i];
	print "label c: $label_c\n";
	print "label s: $label_s\n";
	next if $checked_c[$label_c] && $checked_s[$label_s];
	my $cell_c = fetch_cells(\@cc8_list, $label_c);
	my $cell_s = fetch_cells(\@src_list, $label_s);
	print "cell_c:\n" . Dumper($cell_c) . "\n";
	print "cell_s:\n" . Dumper($cell_s) . "\n";
	return unless Data::Compare::Compare($cell_s, $cell_c);
	$checked_c[$label_c] = $cell_c;
	$checked_s[$label_s] = $cell_s;
}
return 1;
}

sub fetch_cells {
my($list, $label) = @_;
my @dat;
for(my $i=0; $i < @$list; $i++){
	push @dat, $i if $list->[$i] == $label;
}
return \@dat;
}
