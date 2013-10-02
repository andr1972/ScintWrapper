unit CppLexer;

interface

uses
  Graphics, Scintilla;

type
  TCppLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;

const
  sampleLines: AnsiString =
'#include <iostream>'#10+
'using namespace std;'#10+
#10+
'int main()'#10+
'{'#10+
'    std::cout << "Hello world of C++!!!\n";'#10+
'    for (int i=0; i<2;i++)'#10+
'    {'#10+
'	std::cout << i;'#10+
'	std::cout << i * 2;'#10+
'    }'#10+
'    return 0;'#10+
'}';

const cppKeyWords: PAnsiChar =
    '__asm __automated __cdecl __classid __closure __declspec '+
    '__dispid __except __export __fastcall __finally __import '+
    '__int16 __int32 __int64 __int8 __pascal __property '+
    '__published __rtti __stdcall __thread __try _asm _cdecl '+
    '_export _fastcall _import _pascal _stdcall asm auto '+
    'bool break case catch cdecl char class const '+
    'const_cast continue default delete do double '+
    'dynamic_cast else enum explicit extern false float '+
    'for friend goto if inline int interface long '+
    'mutable namespace new operator pascal private protected '+
    'public register reinterpret_cast return short signed '+
    'sizeof static static_cast struct switch template this '+
    'throw true try typedef typeid typename union '+
    'unsigned using virtual void volatile wchar_t while';

function TCppLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TCppLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_CPP);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(cppKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_C_WORD, 1);
  SetAStyle(SCE_C_IDENTIFIER, clBlue);
  SetAStyle(SCE_C_COMMENT, darkGreen);
  SetAStyle(SCE_C_COMMENTLINE, darkGreen);
  SetAStyle(SCE_C_PREPROCESSOR, clFuchsia, offWhite);
  SetAStyle(SCE_C_STRING, clFuchsia);
  SetAStyle(SCE_C_STRINGEOL, clFuchsia);
  SetAStyle(SCE_C_CHARACTER, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.cpp')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
