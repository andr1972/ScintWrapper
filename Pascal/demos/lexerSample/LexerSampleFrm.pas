unit LexerSampleFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Scintilla, AdaLexer, EiffelLexer;

type
  TForm1 = class(TForm)
    Scintilla1: TScintilla;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ComboBox: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
  private
    procedure InitCombo;
  public
  end;

var
  Form1: TForm1;

implementation
uses
  DefaultLexer, HtmlLexer, PasLexer, CppLexer, PyLexer, YamlLexer,
  XmlLexer, SqlLexer, PerlLexer, VBLexer, PropertiesLexer, MakefileLexer,
  BatchLexer, LatexLexer, LuaLexer, DiffLexer, ConfLexer, LispLexer, AsmLexer;

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

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitCombo;
end;

procedure TForm1.InitCombo;
 procedure Add(const S: string; AType: TLexerClass);
 begin
   ComboBox.Items.AddObject(S, TObject(AType));
 end;
begin
  Add('Default',TDefaultLexer);
  Add('Pascal',TPasLexer);
  Add('XML',TXmlLexer);
  Add('HTML',THtmlLexer);
  Add('C++',TCppLexer);
  Add('Python',TPyLexer);
  Add('Yaml',TYamlLexer);
  Add('SQL',TSqlLexer);
  Add('Perl',TPerlLexer);
  Add('VB',TVBLexer);
  Add('Properties',TPropertiesLexer);
  Add('Makefile',TMakefileLexer);
  Add('Batch',TBatchLexer);
  Add('Latex',TLatexLexer);
  Add('Lua',TLuaLexer);
  Add('Diff',TDiffLexer);
  Add('Conf',TConfLexer);
  Add('Ada',TAdaLexer);
  Add('Lisp',TLispLexer);
  Add('Eiffel',TEiffelLexer);
  Add('Asm',TAsmLexer);
end;

procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  with (Sender as TComboBox) do
    Scintilla1.LexerClass:=TLexerClass(Items.Objects[ItemIndex]);
  Scintilla1.Lexer.Sample;
end;

end.
