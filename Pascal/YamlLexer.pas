unit YamlLexer;

interface

uses
  Graphics, Scintilla;

type
  TYamlLexer = class(TLexer)
  protected
  public
    procedure InitDefaults; override;
  end;

implementation

procedure TYamlLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_YAML);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  SetAStyle(SCE_YAML_IDENTIFIER, clBlue);
  SetAStyle(SCE_YAML_COMMENT, darkGreen);
  SetAStyle(SCE_YAML_TEXT, clFuchsia);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.yaml')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
