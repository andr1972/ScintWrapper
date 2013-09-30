unit PasLexer;

interface

uses
  Graphics, Scintilla;

type
  TPasLexer = class(TLexer)
  protected
  public
    procedure InitDefaults; override;
  end;

implementation

{ TPasLexer }

const pasKeyWords: PAnsiChar =
    'absolute abstract and array as asm assembler'+
    'automated begin case cdecl class const constructor'+
    'contains default deprecated destructor dispid'+
    'dispinterface div do downto dynamic else end except'+
    'export exports external far file final finalization'+
    'finally for forward function goto helper if'+
    'implementation implements in index inherited'+
    'initialization inline interface is label library'+
    'message mod name near nil nodefault not object of'+
    'on operator or out overload override package packed'+
    'pascal platform private procedure program property'+
    'protected public published raise read readonly record'+
    'register reintroduce repeat requires resourcestring'+
    'safecall sealed set shl shr stdcall stored string'+
    'stringresource then threadvar to try type unit until'+
    'uses var virtual while with write writeonly xor';

procedure TPasLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_PASCAL);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(pasKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_PAS_WORD, 1);
  SetAStyle(SCE_PAS_IDENTIFIER, clBlue);
  SetAStyle(SCE_PAS_COMMENT, darkGreen);
  SetAStyle(SCE_PAS_COMMENT2, darkGreen);
  SetAStyle(SCE_PAS_COMMENTLINE, darkGreen);
  SetAStyle(SCE_PAS_PREPROCESSOR, clFuchsia, offWhite);
  SetAStyle(SCE_PAS_STRING, clFuchsia);
  SetAStyle(SCE_PAS_STRINGEOL, clFuchsia);
  SetAStyle(SCE_PAS_CHARACTER, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.pascal')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
