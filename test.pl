# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
use Nagios::Web_Trx;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

use Test::Harness;

my ($proxy, $account, $proxy_pass, $stuff) ;

unless ( -s 't/Nagios_Web_Trx_cache.pl' ) {
  print "If you access the Internet with a proxy server, please enter the Proxy details when prompted. Skip otherwise.\n" ;
  print "Please enter the name of any required Proxy server [if necessary for Internet access]. Include a suffix of ':<port>' if port is not 80: " ;
  chomp( $proxy = <STDIN> ) ;
  print "\n" ;
  print "Please enter the name of the account (if using a proxy that requires authentication) to use on the Proxy server: " ;
  chomp( $account = <STDIN> ) ;
  print "\n" ;
  print "Please enter the name of the password (if using a proxy that requires authentication) to use for the Proxy server: " ;
  chomp( $proxy_pass = <STDIN> ) ;
  print "\n" ;
  $stuff = '$proxy      = ' . "'$proxy'"      . ' ;' . "\n" .
           '$account    = ' . "'$account'"    . ' ;' . "\n" .
           '$proxy_pass = ' . "'$proxy_pass'" . ' ;' . "\n" ;
  open(CACHE, '> t/Nagios_Web_Trx_cache.pl') or die "Can't open .t/Nagios_Web_Trx_cache.pl for output: $!" ;
  print CACHE $stuff ;
  close CACHE ;
} ;

runtests(qw(t/check_adds.t t/check_atmoss_new_conn.t)) ;


