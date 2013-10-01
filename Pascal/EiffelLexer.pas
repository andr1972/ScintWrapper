unit EiffelLexer;

interface

uses
  Graphics, Scintilla;

type
  TEiffelLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation
const
  sampleLines: AnsiString = //http://docs.eiffel.com/book/examples/example-command-line-arguments
'class'#10+
'    APPLICATION'#10+
'inherit'#10+
'    ARGUMENTS'#10+
'create'#10+
'    make'#10+
'feature {NONE} -- Initialization'#10+
'    make'#10+
'            -- Print values for arguments with options ''c'' and ''h''.'#10+
'        do'#10+
'            print ("Command line argument value for option ''c'' is: ")'#10+
'            print (separate_character_option_value (''c'') + "%N")'#10+
'            print ("Command line argument value for option ''h'' is: ")'#10+
'            print (separate_character_option_value (''h'') + "%N")'#10+
'            io.read_line    -- Keep console window open'#10+
'        end'#10+
'end';

eiffelKeyWords: PAnsiChar =
    'adapt '+
    'alias  all  and  array  as  assertion  bit  boolean '+
    'character  check  class  cluster  colon  comma  creation '+
    'current  debug  default  deferred  do  double  else  elseif '+
    'end  ensure  exclude  executable  expanded  export  external '+
    'false  feature  from  frozen  generate  identifier  if '+
    'ignore  implies  include  include_path  indexing  infix '+
    'inherit  inspect  integer  invariant  is  like  local  loop '+
    'make  no  not  object  obsolete  old  once  optimize '+
    'option  or  pointer  precompiled  precursor  prefix  real '+
    'redefine  rename  require  rescue  result  retry  root '+
    'select  separate  string  strip  system  then  trace  true '+
    'undefine  unique  until  use  variant  visible  void  when '+
    'xor  yes';


function TEiffelLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TEiffelLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_EIFFEL);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(eiffelKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  SetAStyle(SCE_EIFFEL_COMMENTLINE, TColor($808000));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_EIFFEL_WORD, 1);
  SetAStyle(SCE_EIFFEL_WORD, clBlue);
  SetAStyle(SCE_EIFFEL_NUMBER, TColor(005060));
  SetAStyle(SCE_EIFFEL_STRING, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.eiffel')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
