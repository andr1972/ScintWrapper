unit PerlLexer;

interface

uses
  Graphics, Scintilla;

type
  TPerlLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString = //http://accad.osu.edu/~mlewis/Class/Perl/perl.html
'#!/usr/local/bin/perl'#10+
'#'#10+
'# composite series of images over a background image'#10+
'#'#10+
#10+
'if ($#ARGV != 4) {'#10+
' print "usage: compem bg.rgb inbase outbase startNum stopNum\n";'#10+
' exit;'#10+
'}'#10+
#10+
'$bg = $ARGV[0];'#10+
'$inbase = $ARGV[1];'#10+
'$outbase = $ARGV[2];'#10+
'$start = $ARGV[3];'#10+
'$stop = $ARGV[4];'#10+
#10+
'# for each image'#10+
'for ($i=$start; $i <= $stop; $i++) {'#10+
#10+
'    # pad numbers'#10+
'    $num = $i;'#10+
'    if($i<10) { $num = "00$i"; }'#10+
'    elsif($i<100) { $num = "0$i"; }'#10+
#10+
'    # call unix command "over"'#10+
'    $cmd = "over $bg $inbase.$num $outbase.$num 0 0";'#10+
'    print $cmd."\n";'#10+
'    if(system($cmd)) { print "over failed\n"; }'#10+
'}';

const perlKeyWords: PAnsiChar =
    '$ACCUMULATOR $ARG $ARGV $BASETIME $CHILD_ERROR $DEBUGGING'+
    '$EFFECTIVE_GROUP_ID $EFFECTIVE_USER_ID $EGID $ENV $ERRNO'+
    '$EUID $EVAL_ERROR $EXECUTABLE_NAME $FORMAT_FORMFEED'+
    '$FORMAT_LINE_BREAK_CHARACTERS $FORMAT_LINES_LEFT'+
    '$FORMAT_LINES_PER_PAGE $FORMAT_NAME $FORMAT_PAGE_NUMBER'+
    '$FORMAT_TOP_NAME $GID $INPLACE_EDIT $INPUT_LINE_NUMBER'+
    '$INPUT_RECORD_SEPARATOR $LAST_PAREN_MATCH $LIST_SEPARATOR $MATCH'+
    '$MULTILINE_MATCHING $NR $OFMT $ORS $OS_ERROR'+
    '$OUTPUT_AUTOFLUSH $OUTPUT_FIELD_SEPARATOR $PERL_VERSION $PERLDB'+
    '$PID $POSTMATCH $PREMATCH $PROCESS_ID $PROGRAM_NAME'+
    '$REAL_GROUP_ID $REAL_USER_ID $RS $SIG $SUBSCRIPT_SEPARATOR'+
    '$SUBSEP $SYSTEM_FD_MAX $UID $WARNING %INC @ARGV @INC'+
    'abs accept alarm and atan2 bind binmode bless'+
    'caller chdir chmod chomp chop chown chr chroot'+
    'close closedir cmp connect constant cos crypt'+
    'dbmclose dbmopen defined delete diagnostics die do'+
    'dump each else elsif endgrent endhostent endnetent'+
    'endprotoent endpwent endservent eof eq eval exec'+
    'exists exit exp fcntl fileno flock for foreach'+
    'fork format formline ge getc getgrent getgrgid'+
    'getgrnam gethostbyaddr gethostbyname gethostent getlogin'+
    'getnetbyaddr getnetbyname getnetent getpeername getpgrp'+
    'getppid getpriority getprotobyname getprotobynumber'+
    'getprotoent getpwent getpwnam getpwuid getservbyname'+
    'getservbyport getservent getsockname getsockopt glob'+
    'gmtime goto grep gt hex if import index int'+
    'integer ioctl join keys kill last lc lcfirst le'+
    'length less link listen local locale localtime log'+
    'lstat lt m map mkdir msgctl msgget msgrcv msgsnd'+
    'my ne next no not oct open opendir or ord'+
    'pack package pipe pop pos print push q qq'+
    'quotemeta qw qx rand read readdir readlink recv'+
    'redo ref rename require reset return reverse'+
    'rewinddir rindex rmdir scalar seek seekdir select'+
    'semctl semget semop send setgrent sethostent setnetent'+
    'setpgrp setpriority setprotoent setpwent setservent'+
    'setsockopt shift shmctl shmget shmread shmwrite'+
    'shutdown sigtrap sin sleep socket socketpair sort'+
    'splice split sprintf sqrt srand stat strict study'+
    'sub subs substr symlink syscall sysread system'+
    'syswrite tell telldir tie time times tr truncate'+
    'uc ucfirst umask undef unless unlink unpack unshift'+
    'untie use utime values vars vec wait waitpid'+
    'wantarray warn while write xor';


function TPerlLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TPerlLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_PERL);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(perlKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_PL_WORD, 1);
  SetAStyle(SCE_PL_IDENTIFIER, clBlue);
  SetAStyle(SCE_PL_COMMENTLINE, TColor($808000));
  SetAStyle(SCE_PL_STRING, clFuchsia);
  SetAStyle(SCE_PL_PREPROCESSOR, offWhite);

  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.perl')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
