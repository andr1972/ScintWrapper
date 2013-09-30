unit XmlLexer;

interface

uses
  Graphics, Scintilla;

type
  TXmlLexer = class(TLexer)
  protected
  public
    procedure InitDefaults; override;
  end;

implementation

procedure TXmlLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_HTML);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_H_TAG, 1);
  SetAStyle(SCE_H_ATTRIBUTE, darkGreen);
  SetAStyle(SCE_H_DOUBLESTRING, clFuchsia);
  SetAStyle(SCE_H_SINGLESTRING, clFuchsia);
  SetAStyle(SCE_H_CDATA, clYellow offWhite);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.html')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
