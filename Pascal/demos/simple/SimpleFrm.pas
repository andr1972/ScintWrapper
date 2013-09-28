unit SimpleFrm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PaScintilla;

type
  { TForm1 }

  TForm1 = class(TForm)
    PaScintilla1: TPaScintilla;
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PaScintilla1.AddText;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PaScintilla1.AddText;
end;

end.
