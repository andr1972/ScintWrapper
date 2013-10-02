unit YamlLexer;

interface

uses
  Graphics, Scintilla;

type
  TYamlLexer = class(TLexer)
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
'receipt:     Oz-Ware Purchase Invoice'#10+
'date:        2012-08-06'#10+
'customer:'#10+
'    given:   Dorothy'#10+
'    family:  Gale'#10+
#10+
'items:'#10+
'    - part_no:   A4786'#10+
'      descrip:   Water Bucket (Filled)'#10+
'      price:     1.47'#10+
'      quantity:  4'#10+
#10+
'    - part_no:   E1628'#10+
'      descrip:   High Heeled "Ruby" Slippers'#10+
'      size:      8'#10+
'      price:     100.27'#10+
'      quantity:  1'#10+
#10+
'bill-to:  &id001'#10+
'    street: |'#10+
'            123 Tornado Alley'#10+
'            Suite 16'#10+
'    city:   East Centerville'#10+
'    state:  KS'#10+
#10+
'ship-to:  *id001'#10+
#10+
'specialDelivery:  >'#10+
'    Follow the Yellow Brick'#10+
'    Road to the Emerald City.'#10+
'    Pay no attention to the'#10+
'    man behind the curtain.'#10;

function TYamlLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

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
