/* Reproduction of the definitions used to create the unchanged SAS fixtures.
   Run with a writable HOME in SAS 9.4M9, Linux x86-64, UTF-8. */
%let root=%sysget(HOME);
libname rules "&root";
proc format library=rules.ddiwr_rules;
    value numrule(default=24)
        low-<0='negative' 0='zero' 0<-10='positive' 10<-high='large'
        .='missing' .A='tagged' other='fallback';
    value $charrule(default=18)
        'A'-<'D'='early' 'D'-'F'='middle' other='unlisted';
run;
data rules.ddiwr_rules;
    length c $4;
    input n c $;
    format n numrule. c $charrule.;
    datalines;
-1 A
0 C
1 D
10 F
11 Z
. Q
.A X
.B E
;
run;
proc format library=rules.ddiwr_longrules;
    value $longrule(default=30)
        'abcdefghijklmnopqrstuv'-<'abcdefghijklmnopqrstuz'='long range'
        '漢字abcdefghijklmnopq'-'漢字abcdefghijklmnopz'='unicode range'
        other='other';
    value $limits low-<'A'='before' 'A'<-high='after' other='equal';
    value onlyother other='anything';
run;
