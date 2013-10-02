unit UnicodeFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Scintilla, StdCtrls;

type
  TForm1 = class(TForm)
    SaveDialog1: TSaveDialog;
    Scintilla1: TScintilla;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DefaultLexer;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Text: AnsiString;
  stream: TFileStream;
begin
  SaveDialog1.FileName:='utf8.txt';
  SaveDialog1.InitialDir:=ExtractFileDir(GetModuleName(0));
  if SaveDialog1.Execute then
  begin
    Text:=Scintilla1.GetText();
    stream:=TFileStream.Create(SaveDialog1.FileName, fmCreate);
    stream.Write(Text[1], Length(Text));
    stream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  stream: TFileStream;
  Text: AnsiString;
begin
  Scintilla1.ClearDocumentStyle;
  stream:=TFileStream.Create(IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(0)))+'utf8.txt', fmOpenRead);
  SetLength(Text, stream.Size);
  stream.Read(Text[1], Length(Text));
  stream.Free;
  Scintilla1.ClearAll;
  Scintilla1.AddText(Text);
end;

end.
