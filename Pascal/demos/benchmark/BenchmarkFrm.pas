unit BenchmarkFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Scintilla;

type
  TForm1 = class(TForm)
    Scintilla1: TScintilla;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure RunBenchmark;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled:=false;
  Scintilla1.AccessMethod:=smMessages;
  RunBenchmark;
  Button1.Enabled:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Enabled:=false;
  Scintilla1.AccessMethod:=smDirect;
  RunBenchmark;
  Button2.Enabled:=true;
end;

procedure TForm1.RunBenchmark;
const COUNT=100000;
var
  i: integer;
  i64a,i64b,i64f:int64;
  Msg: AnsiString;
begin
  QueryPerformanceFrequency(i64f);
  QueryPerformanceCounter(i64a);
  Scintilla1.ClearAll;
  for i:=0 to COUNT-1 do Scintilla1.GetLength;
  QueryPerformanceCounter(i64b);
  Msg:=Format('Time of %d calls of GetLength is %.3f us per one',
              [Count, (i64b-i64a)/i64f*1e6/COUNT]);

  Scintilla1.SetWrapMode(SC_WRAP_WORD);
  Scintilla1.AddText(Msg);
  Scintilla1.AddText(#10#10);
  Scintilla1.AddText('Note: function GetLength is fast but other '+
  'functions has has significant overhead and aren''t so frequent');
  Scintilla1.AddText(#10);
  Scintilla1.AddText('so, in real life, message overhead is negligible ');
  Scintilla1.AddText('and provides thread safety');
end;

end.
