unit PaScintilla;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, LResources,
{$ENDIF}
{$IFDEF MSWindows}
  Windows,
{$ELSE}
  libc,
{$ENDIF}
  Classes, SysUtils, Controls, Messages;

const
{$ifdef MSWindows}
  cPaSciLexerDll = 'SciLexer.dll';
{$else}
  cPaSciLexerDll = 'SciLexer.so';
{$endif}

type
  TPaScintillaFunction = function(APointer: Pointer; AMessage: Integer; WParam: Integer; LParam: Integer): Integer; cdecl;
  TPaScintillaMethod = (smWindows, smDirect);

  TKeyMod = LongWord; //Key + (Mod << 16)
  TPosition = integer;

  TSciCell = packed record
    charByte: AnsiChar;
    styleByte: Byte;
  end;

  TSciCharacterRange = record
    cpMin: Integer;
    cpMax: Integer;
  end;

  TSciTextRange = record
    chrg: TSciCharacterRange;
    lpstrText: PAnsiChar;
  end;

  PSciTextToFind = ^TSciTextToFind;
  TSciTextToFind = record
    chrg: TSciCharacterRange;
    lpstrText: PAnsiChar;
    chrgText: TSciCharacterRange;
  end;

  PSciRangeToFormat = ^TSciRangeToFormat;
  TSciRangeToFormat = record
    hdc: HDC;                         // The HDC (device context) we print to
    hdcTarget: HDC;                   // The HDC we use for measuring (may be same as hdc)
    rc: TRect;                        // Rectangle in which to print
    rcPage: TRect;                    // Physically printable page size
    chrg: TSciCharacterRange;         // Range of characters to print
  end;

  TPaScintilla = class(TWinControl)
  private
    FSciDllHandle: HMODULE;
    FDirectPointer: Pointer;
    FDirectFunction: TPaScintillaFunction;
    FAccessMethod: TPaScintillaMethod;
    procedure LoadSciLibraryIfNeeded;
    procedure FreeSciLibrary;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var AMessage: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMCreate(var AMessage: TWMCreate); message WM_CREATE;
    procedure WMDestroy(var AMessage: TWMDestroy); message WM_DESTROY;
    /// <summary>Handles notification messages from Scintilla</summary>
    procedure CNNotify(var AMessage: TWMNotify); message CN_NOTIFY;
      /// <summary>Sends message to Scintilla control.
    function SendEditor(AMessage: Integer; WParam: Integer = 0; LParam: Integer = 0): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddText(const AText: AnsiString);
  published
    property AccessMethod: TPaScintillaMethod read FAccessMethod write FAccessMethod default smDirect;
  end;

  procedure Register;

implementation

{$I pas.gen}
{ TPaScintilla }

constructor TPaScintilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAccessMethod := smDirect;
  Width := 320;
  Height := 240;
  HandleNeeded;
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
{$ifdef MSWindows}
  FSciDllHandle := LoadLibrary(PChar(cPaSciLexerDll));
{$else}
  FSciDllHandle := dlopen(cPaSciLexerDll, RTLD_LAZY);
{$endif}
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

procedure TPaScintilla.WMCreate(var AMessage: TWMCreate);
begin
  inherited;
  FDirectFunction := TPaScintillaFunction(Windows.SendMessage(
    WindowHandle, SCI_GETDIRECTFUNCTION, 0, 0));
  FDirectPointer := Pointer(Windows.SendMessage(
    WindowHandle, SCI_GETDIRECTPOINTER, 0, 0));
end;

procedure TPaScintilla.WMDestroy(var AMessage: TWMDestroy);
begin
  inherited;
  // No longer valid after window destory
  FDirectFunction := nil;
  FDirectPointer := nil;
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

procedure TPaScintilla.AddText(const AText: AnsiString);
begin
  SendEditor(SCI_ADDTEXT, Length(AText), integer(PAnsiChar(AText)));
end;

procedure Register;
begin
  RegisterComponents('PaScintilla', [TPaScintilla]);
end;

procedure TPaScintilla.CNNotify(var AMessage: TWMNotify);
begin
  if HandleAllocated and (AMessage.NMHdr^.hwndFrom = Self.Handle) then
    writeln(AMessage.NMHdr^.code)
  else
    inherited;
end;



function TPaScintilla.SendEditor(AMessage, WParam,
  LParam: Integer): Integer;
begin
  if (FAccessMethod = smWindows) then
    Result := Windows.SendMessage(Self.Handle, AMessage, WParam, LParam)
  else
    Result := FDirectFunction(FDirectPointer, AMessage, WParam, LParam);
end;


initialization
{$IFDEF FPC}
  {$I PaScint.lrs}
{$ENDIF}
end.
