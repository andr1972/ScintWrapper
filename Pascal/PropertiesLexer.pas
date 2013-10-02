unit PropertiesLexer;

interface

uses
  Graphics, Scintilla;

type
  TPropertiesLexer = class(TLexer)
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
'# Comment'#10+
'# Comment1'#10+
'#'#10+
'# Comment2'#10+
#10+
'# Project target.'#10+
'target=android-18';



function TPropertiesLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TPropertiesLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_PROPERTIES);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_PROPS_KEY, 1);
  SetAStyle(SCE_PROPS_COMMENT, TColor($808000));
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.properties')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
