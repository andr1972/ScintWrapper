unit PaScintilla;

interface

uses
  Windows, Classes, SysUtils, Controls, Messages;

const
  cDScintillaDll  = 'Scintilla.dll';
  cDSciLexerDll   = 'SciLexer.dll';

type
  TPaScintilla = class(TWinControl)
  private
    FSciDllHandle: HMODULE;
    FSciDllModule: String;

    FDirectPointer: Pointer;

    procedure LoadSciLibraryIfNeeded;
    procedure FreeSciLibrary;

  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var AMessage: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
 public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
  published

  end;

implementation

{ TPaScintilla }

constructor TPaScintilla.Create(AOwner: TComponent);
begin
  FSciDllModule := cDSciLexerDll;
  inherited Create(AOwner);
  Width := 320;
  Height := 240;
end;

destructor TPaScintilla.Destroy;
begin
  inherited Destroy;

  FreeSciLibrary;
end;

procedure TPaScintilla.LoadSciLibraryIfNeeded;
begin
  if FSciDllHandle <> 0 then
    Exit;

  FSciDllHandle := LoadLibrary(PChar(FSciDllModule));
  if FSciDllHandle = 0 then
    RaiseLastOSError;
end;

procedure TPaScintilla.FreeSciLibrary;
begin
  if FSciDllHandle <> 0 then
  try
    FreeLibrary(FSciDllHandle);
  finally
    FSciDllHandle := 0;
  end;
end;

procedure TPaScintilla.CreateWnd;
begin
  // Load Scintilla if not loaded already.
  // Library must be loaded before subclassing/creating window
  LoadSciLibraryIfNeeded;

  inherited CreateWnd;
end;

procedure TPaScintilla.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Subclass Scintilla - WND Class was registred at DLL load proc
  CreateSubClass(Params, 'SCINTILLA');
end;


procedure TPaScintilla.WMEraseBkgnd(var AMessage: TWmEraseBkgnd);
begin
  if csDesigning in ComponentState then
    inherited
  else
    // Erase background not performed, prevent flickering
    AMessage.Result := 0;
end;

procedure TPaScintilla.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
inherited;
  // Allow key-codes like Enter, Tab, Arrows, and other to be passed to Scintilla
  AMessage.Result := AMessage.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  AMessage.Result := AMessage.Result or DLGC_WANTTAB;
  AMessage.Result := AMessage.Result or DLGC_WANTALLKEYS;
end;

end.
