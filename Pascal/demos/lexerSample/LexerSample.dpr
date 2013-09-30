program LexerSample;

uses
  Forms,
  LexerSampleFrm in 'LexerSampleFrm.pas' {Form1},
  PasLexer in '..\..\PasLexer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
