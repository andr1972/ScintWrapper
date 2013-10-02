program Unicode;

uses
  Forms,
  UnicodeFrm in 'UnicodeFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
