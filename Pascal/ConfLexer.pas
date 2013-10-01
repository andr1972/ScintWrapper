//Apache Configuration Files Lexer
unit ConfLexer;

interface

uses
  Graphics, Scintilla;

type
  TConfLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation
const
  sampleLines: AnsiString = //edited from http://www.pantz.org/software/apache/apache13config.html
'# ServerType is either inetd, or standalone.  Inetd mode is only supported on'#10+
'# Unix platforms.'#10+
'#'#10+
'ServerType standalone'#10+
#10+
'#'#10+
'# ServerRoot: The top of the directory tree under which the server''s'#10+
'# configuration, error, and log files are kept.'#10+
'ServerRoot "/usr/local/apache"'#10+
#10+
'# PidFile: The file in which the server should record its process'#10+
'# identification number when it starts.'#10+
'#'#10+
'PidFile /usr/local/apache/logs/httpd.pid'#10+
#10+
'Timeout 300'#10+
'KeepAlive On'#10+
'MaxKeepAliveRequests 100'#10+
'Port 80'#10+
#10+
'<IfDefine SSL>'#10+
'Listen 443'#10+
'</IfDefine>'#10+
#10+
'#'#10+
'# ServerAdmin: Your address, where problems with the server should be'#10+
'# e-mailed.  This address appears on some server-generated pages, such'#10+
'# as error documents.'#10+
'#'#10+
'ServerAdmin webmaster@here.com'#10+
#10+
'#'#10+
'# TypesConfig describes where the mime.types file (or equivalent) is'#10+
'# to be found.'#10+
'#'#10+
'<IfModule mod_mime.c>'#10+
'    TypesConfig /usr/local/apache/conf/mime.types'#10+
'</IfModule>'#10+
#10;

function TConfLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TConfLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_CONF);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  SetAStyle(SCE_CONF_COMMENT, TColor($808000));
  SetAStyle(SCE_CONF_NUMBER, clBlue);
  SetAStyle(SCE_CONF_IDENTIFIER, TColor($800000));
  SetAStyle(SCE_CONF_EXTENSION, clMaroon);
  SetAStyle(SCE_CONF_PARAMETER, clNavy);
  SetAStyle(SCE_CONF_STRING, clFuchsia);
  SetAStyle(SCE_CONF_OPERATOR, clRed);
  SetAStyle(SCE_CONF_IP, TColor($005090));
  SetAStyle(SCE_CONF_DIRECTIVE, clBlack, TColor($D0FFD0));

  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.conf')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
