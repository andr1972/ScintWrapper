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
  Dialogs, StdCtrls, Scintilla;

type
  { TForm1 }

  TForm1 = class(TForm)
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Scintilla1.AddText('1234');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scintilla1.AddText('abcd');
end;

end.
