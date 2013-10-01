unit PyLexer;

interface

uses
  Graphics, Scintilla;

type
  TPyLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString =
'import itertools'#10+
#10+
'def iter_primes():'#10+
'     # an iterator of all numbers between 2 and +infinity'#10+
'     numbers = itertools.count(2)'#10+
#10+
'     # generate primes forever'#10+
'     while True:'#10+
'         # get the first number from the iterator (always a prime)'#10+
'         prime = numbers.next()'#10+
'         yield prime'#10+
#10+
'         # this code iteratively builds up a chain of'#10+
'         # filters...slightly tricky, but ponder it a bit'#10+
'         numbers = itertools.ifilter(prime.__rmod__, numbers)'#10+
#10+
'for p in iter_primes():'#10+
'    if p > 1000:'#10+
'        break'#10+
'    print p';

const pyKeyWords: PAnsiChar =
    'and assert break class continue def '+
    'del elif else except exec finally for '+
    'from global if import in is lambda '+
    'not or pass print raise return try '+
    'while yield';

function TPyLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TPyLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_PYTHON);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(pyKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_P_WORD, 1);
  SetAStyle(SCE_P_IDENTIFIER, clBlue);
  SetAStyle(SCE_P_COMMENTLINE, darkGreen);
  SetAStyle(SCE_P_COMMENTBLOCK, darkGreen);
  SetAStyle(SCE_P_STRING, clFuchsia);
  SetAStyle(SCE_P_STRINGEOL, clFuchsia);
  SetAStyle(SCE_P_CHARACTER, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.python')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
