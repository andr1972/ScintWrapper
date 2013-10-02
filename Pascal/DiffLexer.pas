unit DiffLexer;

interface

uses
  Graphics, Scintilla;

type
  TDiffLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;
const
  sampleLines: AnsiString = //http://www.chemie.fu-berlin.de/chemnet/use/info/diff/diff_3.html
'*** lao	Sat Jan 26 23:30:39 1991'#10+
'--- tzu	Sat Jan 26 23:30:50 1991'#10+
'***************'#10+
'*** 1,7 ****'#10+
'- The Way that can be told of is not the eternal Way;'#10+
'- The name that can be named is not the eternal name.'#10+
'  The Nameless is the origin of Heaven and Earth;'#10+
'! The Named is the mother of all things.'#10+
'  Therefore let there always be non-being,'#10+
'    so we may see their subtlety,'#10+
'  And let there always be being,'#10+
'--- 1,6 ----'#10+
'  The Nameless is the origin of Heaven and Earth;'#10+
'! The named is the mother of all things.'#10+
'!'#10+
'  Therefore let there always be non-being,'#10+
'    so we may see their subtlety,'#10+
'  And let there always be being,'#10+
'***************'#10+
'*** 9,11 ****'#10+
'--- 8,13 ----'#10+
'  The two are the same,'#10+
'  But after they are produced,'#10+
'    they have different names.'#10+
'+ They both may be called deep and profound.'#10+
'+ Deeper and more profound,'#10+
'+ The door of all subtleties!';

function TDiffLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TDiffLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_DIFF);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  SetAStyle(SCE_DIFF_COMMENT, TColor($808000));
  SetAStyle(SCE_DIFF_DELETED, clRed);
  SetAStyle(SCE_DIFF_ADDED, clGreen);
  SetAStyle(SCE_DIFF_CHANGED, clBlue);
  SetAStyle(SCE_DIFF_POSITION, clBlue, TColor($D0D0FF));
  SetAStyle(SCE_DIFF_HEADER, clBlack, TColor($D0FFD0));
  SetAStyle(SCE_DIFF_COMMAND, clMaroon);
{val =2
val SCE_DIFF_HEADER=3
val SCE_DIFF_POSITION=4
val =5
val 6
}
{
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_LUA_WORD, 1);
  SetAStyle(SCE_LUA_WORD, darkBlue);

  SetAStyle(SCE_LUA_COMMENTLINE, TColor($808000));
  SetAStyle(SCE_LUA_COMMENTDOC, TColor($808000));
  SetAStyle(SCE_LUA_NUMBER, clNavy);
  SetAStyle(SCE_LUA_STRING, clFuchsia);
  SetAStyle(SCE_LUA_CHARACTER, clMaroon);
  SetAStyle(SCE_LUA_LITERALSTRING, clGray);
  SetAStyle(SCE_LUA_PREPROCESSOR, offWhite);
  SetAStyle(SCE_LUA_OPERATOR, clGreen);
  SetAStyle(SCE_LUA_IDENTIFIER, TColor($005090));
  SetAStyle(SCE_LUA_STRINGEOL, clFuchsia, clYellow);}
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.diff')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
