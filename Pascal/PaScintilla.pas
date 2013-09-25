unit PaScintilla;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls;

type
  TPaScintilla = class(TCustomControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PaScintilla', [TPaScintilla]);
end;

{ TPaScintilla }

constructor TPaScintilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 320;
  Height := 240;
end;

end.
