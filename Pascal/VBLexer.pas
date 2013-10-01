unit VBLexer;

interface

uses
  Graphics, Scintilla;

type
  TVBLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString = //http://www.vbtutor.net/VB_Sample/picviewer.htm
'Private Sub Combo1_Change()'#10+
'  ''To list all graphics files or all files'#10+
'  If ListIndex = 0 Then'#10+
'    File1.Pattern = ("*.bmp;*.wmf;*.jpg;*.gif")'#10+
'  Else'#10+
'    Fiel1.Pattern = ("*.*")'#10+
'  End If'#10+
'End Sub'#10+
#10+
'Private Sub Dir1_Change()'#10+
'  ''To choose drive'#10+
'  File1.Path = Dir1.Path'#10+
'  File1.Pattern = ("*.bmp;*.wmf;*.jpg;*.gif")'#10+
'End Sub'#10+
#10+
'Private Sub Drive1_Change()'#10+
'  Dir1.Path = Drive1.Drive'#10+
'End Sub'#10+
#10+
'Private Sub File1_Click()'#10+
'  ''To select a file'#10+
'  If Combo1.ListIndex = 0 Then'#10+
'    File1.Pattern = ("*.bmp;*.wmf;*.jpg;*.gif")'#10+
'  Else'#10+
'    File1.Pattern = ("*.*")'#10+
'  End If'#10+
#10+
'  If Right(File1.Path, 1) <> "\" Then'#10+
'    filenam = File1.Path + "\" + File1.FileName'#10+
'  Else'#10+
'    filenam = File1.Path + File1.FileName'#10+
'  End If'#10+
'End Sub'#10+
#10+
'Private Sub show_Click()'#10+
'  ''To show the selected graphics file'#10+
'  If Right(File1.Path, 1) <> "\" Then'#10+
'    filenam = File1.Path + "\" + File1.FileName'#10+
'  Else'#10+
'    filenam = File1.Path + File1.FileName'#10+
'  End If'#10+
'  Picture1.Picture = LoadPicture(filenam)'#10+
'End Sub';
const vbKeyWords: PAnsiChar =
    'abs and appactivate array as asc atn attribute '+
    'base beep begin boolean byte call case cbool '+
    'cbyte ccur cdate cdbl chdir chdrive chr cint '+
    'circle class clear clng close command compare const '+
    'cos createobject csng cstr curdir currency cvar '+
    'cverr date dateadd datediff datepart dateserial '+
    'datevalue ddb deftype dim dir do doevents double '+
    'each else elseif empty end environ eof eqv erase '+
    'err error exit exp explicit fileattr filecopy '+
    'filedatetime filelen fix for form format freefile '+
    'function fv get getattr getobject gosub goto hex '+
    'hour if iif imp input instr int integer ipmt '+
    'irr is isarray isdate isempty iserror ismissing '+
    'isnull isnumeric isobject kill lbound lcase left len '+
    'let line loc lock lof log long loop lset ltrim '+
    'me mid minute mirr mkdir mod module month msgbox '+
    'name new next not nothing now nper npv object '+
    'oct on open option or pmt ppmt print private '+
    'property pset public put pv qbcolor raise randomize '+
    'rate redim rem reset resume return rgb right '+
    'rmdir rnd rset rtrim second seek select sendkeys '+
    'set setattr sgn shell sin single sln space spc '+
    'sqr static stop str strcomp strconv string sub '+
    'switch syd system tab tan then time timer '+
    'timeserial timevalue to trim typename ubound ucase '+
    'unlock until val variant vartype version weekday '+
    'wend while width with write xor';



function TVBLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TVBLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_VB);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(vbKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_B_KEYWORD, 1);
  SetAStyle(SCE_B_IDENTIFIER, clBlue);
  SetAStyle(SCE_B_COMMENT, TColor($808000));
  SetAStyle(SCE_B_STRING, clFuchsia);
  SetAStyle(SCE_B_STRINGEOL, clFuchsia);
  SetAStyle(SCE_B_PREPROCESSOR, offWhite);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.vb')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
