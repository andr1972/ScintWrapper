unit FontDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Spin;

type
  TFontDialog = class(TForm)
    seSize: TSpinEdit;
    btnDefaultFont: TButton;
    btnDefaultSize: TButton;
    ListView: TListView;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure seSizeChange(Sender: TObject);
  private
    procedure FillFontList;
  public
  end;

var
  FontDialog: TFontDialog;

implementation

uses EditorFrm, Scintilla;

{$R *.dfm}

procedure TFontDialog.FillFontList;
var
  i: integer;
begin
  ListView.OnChange:=nil;
  ListView.Clear;
  for i:=0 to Screen.Fonts.Count-1 do
    ListView.AddItem(Screen.Fonts[i], nil);
  ListView.OnChange:=ListViewChange;  
end;

procedure TFontDialog.FormShow(Sender: TObject);
var
  CurrentFont: string;
  i: integer;
begin
  FillFontList;
  CurrentFont:=EditorForm.Scintilla1.StyleGetFont(STYLE_DEFAULT);
  seSize.OnChange:=nil;
  seSize.Value:=EditorForm.Scintilla1.StyleGetSize(STYLE_DEFAULT);
  seSize.OnChange:=seSizeChange;
  for i:=0 to ListView.Items.Count-1 do
  begin
    if ListView.Items[i].Caption=CurrentFont then
    begin
      ListView.Selected:=ListView.Items[i];
      break;
    end;
  end;
  if ListView.Selected<>nil then ListView.Selected.MakeVisible(false);
  ListView.SetFocus;
end;

procedure TFontDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;

procedure TFontDialog.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  EditorForm.Scintilla1.StyleSetFont(STYLE_DEFAULT, PAnsiChar(AnsiString(Item.Caption)));
end;

procedure TFontDialog.seSizeChange(Sender: TObject);
begin
  EditorForm.Scintilla1.StyleSetSize(STYLE_DEFAULT, seSize.Value);
end;

end.
