program FontDemo;

uses
  Forms,
  EditorFrm in 'EditorFrm.pas' {EditorForm},
  FontDlg in 'FontDlg.pas' {FontDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TEditorForm, EditorForm);
  Application.CreateForm(TFontDialog, FontDialog);
  Application.Run;
end.
