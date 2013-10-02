unit AdaLexer;

interface

uses
  Graphics, Scintilla;

type
  TAdaLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;
const
  sampleLines: AnsiString = //http://sandbox.mc.edu/~bennet/ada/examples/gnatioex_adb.html
'-- This is one of the examples shipped with GNAT.'#10+
'With Gnat.IO; use Gnat.IO;'#10+
'procedure Gnatioex is'#10+
'   C : Character;'#10+
'   I : Integer;'#10+
'   S : String (1 .. 100);'#10+
'begin'#10+
'   Put_Line ("Hello. Welcome to the GNAT IO example" & "!");'#10+
'   New_Line;'#10+
'   Put ("Note: This example uses GNAT.IO which does not raise Data_Error");'#10+
'   Put_Line (" for bad data.");'#10+
'   Put ("      ");'#10+
'   Put_Line ("For that you need to use the standard package Ada.Text_IO.");'#10+
'   New_line;'#10+
'   Put ("Please enter a single character now followed by <CR> ");'#10+
'   Get (C);'#10+
'   Put ("Your character is: ");'#10+
'   Put (C);'#10+
'   Get (C);  --  read the <CR>'#10+
'   New_Line (2);'#10+
''#10+
'   Put ("Please enter a String now followed by <CR> :");'#10+
'   Get_Line (S, I);'#10+
'   Put ("Your String is : ");'#10+
'   Put_Line (S (1 .. I));'#10+
'   Put ("Its length is : ");'#10+
'   Put (I);'#10+
'   New_Line (2);'#10+
''#10+
'   Put ("Please enter an integer now followed by <CR> ");'#10+
'   Get (I);'#10+
'   Put ("Your number is: ");'#10+
'   Put (I);'#10+
'   New_Line (2);'#10+
'end;';

const adaKeyWords: PAnsiChar = //https://en.wikibooks.org/wiki/Ada_Programming/Keywords
'abort else new return abs elsif not reverse '+
'abstract end null accept entry select '+
'access exception of separate '+
'aliased exit or some all others subtype '+
'and for out synchronized array function overriding '+
'at tagged generic package task '+
'begin goto pragma terminate '+
'body private then if procedure type '+
'case in protected constant interface until '+
'is raise use declare range '+
'delay limited record when '+
'delta loop rem while '+
'digits renames with '+
'do mod requeue xor';

function TAdaLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TAdaLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_ADA);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(adaKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  SetAStyle(SCE_ADA_WORD, TColor($800000));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_ADA_WORD, 1);
  SetAStyle(SCE_ADA_IDENTIFIER, clBlue);
  SetAStyle(SCE_ADA_COMMENTLINE, TColor($808000));
  SetAStyle(SCE_ADA_STRING, clFuchsia);
  SetAStyle(SCE_ADA_NUMBER, TColor(005060));
  SetAStyle(SCE_ADA_ILLEGAL, clRed);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.ada')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
