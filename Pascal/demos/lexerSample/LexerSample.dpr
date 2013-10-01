program LexerSample;

uses
  Forms,
  LexerSampleFrm in 'LexerSampleFrm.pas' {Form1},
  XmlLexer in '..\..\XmlLexer.pas',
  LatexLexer in '..\..\LatexLexer.pas',
  DefaultLexer in '..\..\DefaultLexer.pas',
  HtmlLexer in '..\..\HtmlLexer.pas',
  PropertiesLexer in '..\..\PropertiesLexer.pas',
  BatchLexer in '..\..\BatchLexer.pas',
  AsmLexer in '..\..\AsmLexer.pas',
  MakefileLexer in '..\..\MakefileLexer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
