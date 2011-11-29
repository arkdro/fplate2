#!/usr/bin/perl
# reads file with generated plate and writes command and data files for gnuplot
use strict;
use FileHandle;

sub runall {
my $base = int rand(1000000);
my @buf = ();
my $beg = 0;
my $id = 0;
while(my $str=<>){
	if($str =~ /pure_plate([12]):/){
		$id = $1;
		$beg = 1;
	}
	if($str =~ /^$/ && $beg){
		$beg = 0;
		print_plate($base, $id, \@buf);
		@buf = ();
	}
	if($beg) {
		my $row = clean_str($str);
		push @buf, $row if $row && @$row;
	}
}
}

sub clean_str{
my($str) = @_;
my @res;
my @buf = split /;/, $str;
for my $i (@buf){
	if($i =~/c=(\d+)/){
		push @res, $1;
	}
}
return \@res;
}

sub print_cmd_file {
my($fn_dat, $fn_cmd) = @_;
my $fd = new FileHandle ">$fn_cmd";
unless($fd){
	print STDERR "can open cmd file '$fn_cmd': $!\n";
	return;
}
print $fd qq{
set title "plate"
unset key
set tic scale 0
set palette defined ( \\
0 "red", \\
1 "green", \\
2 "yellow", \\
3 "blue", \\
4 "magenta", \\
5 "black", \\
6 "cyan", \\
7 "white" \\
)

set view map
splot "$fn_dat" matrix with image
pause -1 "Hit return to continue"
};
undef $fd;
}

sub print_plate {
my($base, $id, $buf) = @_;
my $fn = $base . "_" . $id;
my $file1 = $fn . ".dat";
my $file2 = $fn . ".cmd";
print_cmd_file($file1, $file2);
my $fd = new FileHandle ">$file1";
unless($fd){
	print STDERR "can open file '$file1': $!\n";
	return;
}
for(my $i=0; $i < @$buf; $i++){
	my $row = $buf->[$i];
	my @r2 = ();
	map { push @r2, $_; push @r2, $_} @$row;
	if($i % 2){
		pop @r2;
	}else{
		shift @r2;
	}
	my $str = join ' ', @r2;
	print $fd $str . "\n";
}
undef $fd;
}

runall();
