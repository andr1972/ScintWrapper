program Benchmark;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms, Interfaces,
  BenchmarkFrm in 'BenchmarkFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
