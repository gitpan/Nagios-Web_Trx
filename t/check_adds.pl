#!/usr/bin/perl

use Nagios::Web_Trx;


$PROGNAME = 'check_adds.pl' ;

use Getopt::Long ;
GetOptions
        (
        "h|help"        => \&print_usage,
        "d|debug"       => \$debug,
        "P|proxy:s"	=> \$proxy,
	"A|account:s"   => \$account,
	"p|pass:s"	=> \$pass,
) ;

$Proxy = {} ;
$Proxy = { server => "http://$proxy/" } if $proxy ;
$Proxy->{account} = $account  if $account ;
$Proxy->{pass}    = $pass     if $pass ;

$ar = [ { Method    => "GET",
              Url       => "http://Pericles.IPAustralia.Gov.AU/adds2/ADDS.ADDS_START.intro",
              Qs_var    => [],
              Qs_fixed  => [], 
              Exp       => "Designs Data Searching - Introduction",
              Exp_Fault => "We were unable to process your request at this time" } ] ;

$web_trx = Nagios::Web_Trx->new($ar) ;
($rc, $message) = $web_trx->check( {}, debug => $debug, proxy => $Proxy ) ;

print $rc ? 'ADDS Ok. ' : 'Adds b0rked: ', $message, "\n" ; 

sub print_usage () {
        print "$PROGNAME Check of the IP Australia ADDS service.\n" ;
        print "$PROGNAME [-d | --debug]\n";
        print "$PROGNAME [-h | --help]\n";
        print "$PROGNAME [-P | --proxy] name of proxy server. Include :port as a suffix if required eg localhost:3128\n";
        print "$PROGNAME [-A | --account] account to use proxy server\n";
        print "$PROGNAME [-p | --pass] password to use proxy server\n";

	exit 0;
}
