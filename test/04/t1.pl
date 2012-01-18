#!/usr/bin/perl
#
# gets result from rpl. Extracts plate, point size, ccl result. Performs
# ccl8 for every possible point. Compares input ccl result and PDL ccl8.
#
use strict;
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
	my $src_res_p = pdl($src_res->{$cell});
	if(all(approx($res_cc8, $src_res_p))){
		print "matched data\n";
	}else{
		print "not matched data\n";
		print "input data:\n"   . $cur_src_p . "\n";
		print "input result:\n" . $src_res_p . "\n";
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
