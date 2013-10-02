unit XmlLexer;

interface

uses
  Graphics, Scintilla;

type
  TXmlLexer = class(TLexer)
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
  '<?xml version="1.0" encoding="UTF-8"?>'#10+
  '<project name="StartupActivity">'#10+
  #10+
  '    <!-- The local.properties file is created and updated by the ''android'' tool.'#10+
  '         It contains the path to the SDK. It should *NOT* be checked in in Version'#10+
  '         Control Systems. -->'#10+
  '    <property file="local.properties" />'#10+
  '    <property file="build.properties" />'#10+
  '    <path id="android.antlibs">'#10+
  '        <pathelement path="${sdk.dir}/tools/lib/anttasks.jar" />'#10+
  '        <pathelement path="${sdk.dir}/tools/lib/sdklib.jar" />'#10+
  '        <pathelement path="${sdk.dir}/tools/lib/androidprefs.jar" />'#10+
  '        <pathelement path="${sdk.dir}/tools/lib/apkbuilder.jar" />'#10+
  '        <pathelement path="${sdk.dir}/tools/lib/jarutils.jar" />'#10+
  '    </path>'#10+
  '</project>';

function TXmlLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

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
  SetAStyle(SCE_H_CDATA, clRed, offWhite);
  SetAStyle(SCE_H_COMMENT, TColor($808000));
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.html')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
