#!/usr/local/bin/perl

$PROGNAME = 'check_atmoss_new_conn.pl' ;

use Nagios::Web_Trx;

use Getopt::Long ;
GetOptions
        (
        "h|help"        => \&print_usage,
        "d|debug"       => \$debug,
        "P|proxy:s"     => \$proxy,
        "A|account:s"   => \$account,
        "p|pass:s"      => \$pass,
) ;

$Proxy = {} ;
$Proxy = { server => "http://$proxy/" } if $proxy ;
$Proxy->{account} = $account  if $account ;
$Proxy->{pass}    = $pass     if $pass ;

use constant Intro              => 'http://Pericles.IPAustralia.Gov.AU/atmoss/falcon.application_start' ;
use constant ConnectToSearch    => 'http://Pericles.IPAustralia.Gov.AU/atmoss/Falcon_Users_Cookies.Define_User' ;
use constant MultiSessConn      => 'http://Pericles.IPAustralia.Gov.AU/atmoss/Falcon_Users_Cookies.Run_Create' ;
use constant Search             => 'http://Pericles.IPAustralia.Gov.AU/atmoss/Falcon.Result' ;
use constant ResultAbstract     => 'http://Pericles.IPAustralia.Gov.AU/atmoss/falcon_details.show_tm_details' ;
use constant ResultDetails      => 'http://Pericles.IPAustralia.Gov.AU/atmoss/Falcon_Details.Show_TM_Details' ;
use constant DeleteSearches     => 'http://Pericles.IPAustralia.Gov.AU/atmoss/Falcon_Searches.Delete_All_Searches' ;
   
use constant Int                => 'Welcome to ATMOSS' ;
use constant ConnSrch           => 'Connect to Trade Mark Search' ;
use constant MltiSess           => 'Fill in one or more of the fields below' ;
use constant Srch               => 'Your search request retrieved\s+\d+\s+match(es)?' ;
use constant ResAbs             => 'Trade Mark\s+:\s+\d+' ;
use constant ResDet             => ResAbs ;
use constant DelSrch            => MltiSess ;
   
use constant MSC_f              => [p_Anon => 'ANONYMOUS', p_user_type => 'Create New Connection', p_JS => 'N'] ;
use constant Srch_v             => [p_tmno1 => 'tmno'] ;
use constant RA_v               => [p_tm_number => 'tmno'] ;
use constant RA_f               => [p_detail => 'QUICK', p_rec_all => 1, p_rec_no => 1, p_search_no => 1, p_ExtDisp => 'D'] ;
use constant RD_v               => RA_v ;
use constant RD_f               => [p_Detail => 'DETAILED', p_rec_no => 1, p_search_no => 1,  p_ExtDisp => 'D'];
 
use constant OraFault           => 'We were unable to process your request at this time' ;

use constant URLS               => [
      {Method => 'GET',  Url => Intro,           Qs_var => [],      Qs_fixed => [], Exp => Int,     Exp_Fault => OraFault},
      {Method => 'GET',  Url => ConnectToSearch, Qs_var => [],      Qs_fixed => [], Exp => ConnSrch,Exp_Fault => OraFault},
      {Method => 'POST', Url => MultiSessConn,   Qs_var => [],      Qs_fixed => MSC_f,Exp => MltiSess,Exp_Fault => OraFault},
      {Method => 'POST', Url => Search,          Qs_var => Srch_v,  Qs_fixed => [], Exp => Srch,    Exp_Fault => OraFault},
      {Method => 'GET',  Url => ResultAbstract,  Qs_var => RA_v,    Qs_fixed => RA_f, Exp => ResAbs,Exp_Fault => OraFault},
      {Method => 'POST', Url => ResultDetails,   Qs_var => RD_v,    Qs_fixed => RD_f, Exp => ResDet,Exp_Fault => OraFault},
      {Method => 'POST', Url => DeleteSearches,  Qs_var => [],      Qs_fixed => [], Exp => DelSrch, Exp_Fault => OraFault},
            ] ;

@tmarks = @ARGV ? @ARGV : (3, 100092, 200099, 300006, 400075, 500067, 600076, 700066, 800061) ;
$i = @ARGV == 1 ? 0 : int( rand($#tmarks) + 0.5 ) ;
$tmno = $tmarks[$i] ;

$x = Nagios::Web_Trx->new( URLS ) ;
($rc, $message) =  $x->check( {tmno => $tmno}, debug => $debug, proxy => $Proxy ) ;

print $rc ? 'ATMOSS Ok. ' : 'ATMOSS b0rked: ', $message, "\n" ; 
     
sub print_usage () {
        print "$PROGNAME Check of IP Australia ATMOSS service.\n" ;
        print "$PROGNAME Trade Mark Number eg '3'\n" ;
        print "$PROGNAME [-d | --debug]\n";
        print "$PROGNAME [-h | --help]\n";
        print "$PROGNAME [-P | --proxy] name of proxy server. Include :port as a suffix if required eg localhost:3128\n";
        print "$PROGNAME [-A | --account] account to use proxy server\n";
        print "$PROGNAME [-p | --pass] password to use proxy server\n";

        exit 0;
}
         


