#!/usr/bin/perl
#
# gets result from rpl. Extracts plate, point size, ccl result. Performs
# ccl8 for every possible point. Compares input ccl result and PDL ccl8.
#
use strict;
use 5.010;
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
	my $res_cc8_norm = normalize_ccl8_res($res_cc8);
	my $cur_src_res = normalize_src_res($src_res->{$cell});
	my $src_res_p = pdl($cur_src_res);
	my @cur_src_res_swapped = @$cur_src_res;
	swap_index($res_cc8_norm, \@cur_src_res_swapped);
	if(@$res_cc8_norm ~~ @cur_src_res_swapped){
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

sub normalize_src_res {
my($dat) = @_;
my %keys;
for my $row (@$dat){
	for my $cell (@$row){
		$keys{$cell} = 1;
	}
}
my @k = keys %keys;
my $out_keys = normalize_items(\@k);
my @res;
for my $row (@$dat){
	my @r2 = @$row;
	map { $_ = $out_keys->{$_} } @r2;
	push @res, @r2;
}
return \@res;
}

sub normalize_items {
my ($in_dat) = @_;
my @sk = sort {$a <=> $b} @$in_dat;
my %out_keys;
for(my $i = 0; $i < @sk; $i++){
	$out_keys{$sk[$i]} = $i;
}
return \%out_keys;
}

sub normalize_ccl8_res {
my($res) = @_;
my @lst = $res->list;
my %k;
map {$k{$_} = 1} @lst;
my @k2 = keys %k;
my $out_keys = normalize_items(\@k2);
map { $_ = $out_keys->{$_} } @lst;
return \@lst;
}

sub swap_index {
my ($res_cc8_norm, $cur_src_res) = @_;
for(my $i = 0; $i < @$res_cc8_norm; $i++){
	my $ic = $res_cc8_norm->[$i];
	my $is = $cur_src_res->[$i];
	if($ic != $is){
		swap_one_index($cur_src_res, $ic, $is);
		#swap_index($res_cc8_norm, $cur_src_res);
		last;
	}
}
}

sub swap_one_index {
my($cur_src_res, $ic, $is) = @_;

}
