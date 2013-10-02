unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Scintilla;

type
  TEditorForm = class(TForm)
    Scintilla1: TScintilla;
    MainMenu1: TMainMenu;
    miFont: TMenuItem;
    Unicode1: TMenuItem;
    Read1: TMenuItem;
    procedure miFontClick(Sender: TObject);
    procedure Read1Click(Sender: TObject);
  private
    procedure ReadUnicode;
  public
    { Public declarations }
  end;

var
  EditorForm: TEditorForm;

implementation

uses FontDlg;

{$R *.dfm}

procedure TEditorForm.miFontClick(Sender: TObject);
begin
  FontDialog.ShowModal;
end;

procedure TEditorForm.Read1Click(Sender: TObject);
begin
  ReadUnicode;
end;

procedure TEditorForm.ReadUnicode;
var
  Dir,Path: string;
  stream: TFileStream;
  Text: AnsiString;
begin
  Dir:=IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(0)));
  Path:=Dir+'..'+PathDelim+'unicode'+PathDelim+'utf8.txt';
  Path:=ExpandFileName(Path);
  stream:=TFileStream.Create(Path, fmOpenRead);
  SetLength(Text, stream.Size);
  stream.Read(Text[1], Length(Text));
  stream.Free;
  Scintilla1.ClearAll;
  Scintilla1.AddText(Text);
end;

end.
