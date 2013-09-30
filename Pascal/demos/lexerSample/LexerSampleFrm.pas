unit LexerSampleFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Scintilla;

type
  TForm1 = class(TForm)
    Scintilla1: TScintilla;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
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
  ext: string;
begin
  if OpenDialog1.Execute then
  begin
    ext:= LowerCase(ExtractFileExt(OpenDialog1.FileName));
    if ext='.html' then Scintilla1.LexerClass:=THtmlLexer
    else if (ext='.pas')or(ext='.pp')or(ext='.inc') then Scintilla1.LexerClass:=TPasLexer
    else if (ext='.c')or(ext='.cpp')or(ext='.cxx')or(ext='.h')or(ext='.hpp') then Scintilla1.LexerClass:=TCppLexer
    else if ext='.py' then Scintilla1.LexerClass:=TPyLexer
    else if (ext='.yaml')or(ext='.yml') then Scintilla1.LexerClass:=TYamlLexer
    else if (ext='.xml') then Scintilla1.LexerClass:=TXmlLexer;
    stream:=TMemoryStream.Create;
    stream.LoadFromFile(OpenDialog1.FileName);
    Scintilla1.ClearAll;
    Scintilla1.AddText(stream.Size, stream.Memory);
    stream.Free;
    Scintilla1.Fold;
  end;
end;

end.
