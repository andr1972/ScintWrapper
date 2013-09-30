unit LexerSampleFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PaScintilla;

type
  TForm1 = class(TForm)
    PaScintilla1: TPaScintilla;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation
uses
  HtmlLexer;

{$R *.dfm}
procedure TForm1.Button1Click(Sender: TObject);
var
  stream: TMemoryStream;
begin
  stream:=TMemoryStream.Create;
  stream.LoadFromFile('d:\Andrzej\scintilla\doc\Design.html');
  PaScintilla1.ClearAll;
  PaScintilla1.AddText(stream.Size, stream.Memory);
  stream.Free;
  PaScintilla1.LexerClass:=THtmlLexer;
  PaScintilla1.Fold;
end;

end.
