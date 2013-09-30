unit LexerSampleFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Scintilla;

type
  TForm1 = class(TForm)
    Scintilla1: TScintilla;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation
uses
  HtmlLexer, PasLexer, CppLexer, PyLexer, YamlLexer,
  XmlLexer;

{$R *.dfm}
procedure TForm1.Button1Click(Sender: TObject);
var
  stream: TMemoryStream;
begin
  stream:=TMemoryStream.Create;
  //stream.LoadFromFile('d:\code\ScintWrapper\Pascal\Scintilla.pas');
  //stream.LoadFromFile('d:\Andrzej\scintilla\doc\Design.html');
  //stream.LoadFromFile('d:\Andrzej\scintilla\src\Editor.cxx');
  //stream.LoadFromFile('d:\Andrzej\scintilla\test\simpleTests.py');
  //stream.LoadFromFile('d:\code\AsCommander.git\bin\AsCmd.yaml');
  stream.LoadFromFile('d:\code\arity-calculator\bin\AndroidManifest.xml');
  Scintilla1.ClearAll;
  Scintilla1.AddText(stream.Size, stream.Memory);
  stream.Free;
  Scintilla1.LexerClass:=TXmlLexer;
  Scintilla1.Fold;
end;

end.
