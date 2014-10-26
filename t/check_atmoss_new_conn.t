use strict;
use Test;
use vars qw($tests);

BEGIN {$tests = 2; plan tests => $tests}

my $null = '';
my $cmd;
my $str;
my $t=0;

$cmd = "perl -Mblib t/check_atmoss_new_conn.pl -h";
$str = `$cmd`;
print "Test was: $cmd\n" if ($?);
$t += ok $str, '/^check_atmoss_new_conn/';

my ($proxy, $account, $proxy_pass, $stuff) ;

do 'Nagios_Web_Trx_cache.pl' ;

if ( $proxy && $account && $proxy_pass ) {
  $cmd = "perl -Mblib t/check_atmoss_new_conn.pl -P $proxy -A $account -p $proxy_pass" ;
  $str = `$cmd` ;
  $t += ok $str, '/^ATMOSS Ok\./' ;
  print "Test was: $cmd\n" if ($?);
} elsif ( $proxy ) {
  $cmd = "perl -Mblib t/check_atmoss_new_conn.pl -P $proxy" ;
  $str = `$cmd` ;
  $t += ok $str, '/^ATMOSS Ok\./' ;
  print "Test was: $cmd\n" if ($?);
} else {
  $cmd = "perl -Mblib t/check_atmoss_new_conn.pl" ;
  $str = `$cmd` ;
  $t += ok $str, '/^ATMOSS Ok\./' ;
  print "Test was: $cmd\n" if ($?);
}

exit(0) if defined($Test::Harness::VERSION);
exit($tests - $t);
