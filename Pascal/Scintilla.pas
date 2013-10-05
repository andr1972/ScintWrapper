unit Scintilla;

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
  Classes, SysUtils, Controls, Messages, Graphics;

const
{$ifdef MSWindows}
  cPaSciLexerDll = 'SciLexer.dll';
{$else}
  cPaSciLexerDll = 'SciLexer.so';
{$endif}

type
  TScintillaFunction = function(APointer: Pointer; AMessage: Integer; WParam: Integer; LParam: Integer): Integer; cdecl;
  TScintillaMethod = (smMessages, smDirect);

  TKeyMod = LongWord; //Key + (Mod << 16)
  TPosition = integer;

  TSciCell = packed record
    charByte: AnsiChar;
    styleByte: Byte;
    fillToInt: Word;
  end;

  PSciCharacterRange = ^TSciCharacterRange;
  TSciCharacterRange = packed record
    cpMin: Integer;
    cpMax: Integer;
  end;

  PSciTextRange = ^TSciTextRange;
  TSciTextRange = packed record
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

  type
  TSciNotifyHeader = TNMHdr;

  PSciSCNotification = ^TSciSCNotification;
  TSciSCNotification = packed record
    NotifyHeader: TSciNotifyHeader;
    position: Integer;
	  // SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_MARGINCLICK,
	  // SCN_NEEDSHOWN, SCN_DWELLSTART, SCN_DWELLEND, SCN_CALLTIPCLICK,
	  // SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, SCN_HOTSPOTRELEASECLICK,
	  // SCN_INDICATORCLICK, SCN_INDICATORRELEASE,
	  // SCN_USERLISTSELECTION, SCN_AUTOCSELECTION

    ch: Integer;                    // SCN_CHARADDED, SCN_KEY
    modifiers: Integer;
	  // SCN_KEY, SCN_DOUBLECLICK, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK,
	  // SCN_HOTSPOTRELEASECLICK, SCN_INDICATORCLICK, SCN_INDICATORRELEASE,

    modificationType: Integer;      // SCN_MODIFIED
    text: PAnsiChar;                // SCN_MODIFIED
    length: Integer;                // SCN_MODIFIED
    linesAdded: Integer;            // SCN_MODIFIED
    message: Integer;               // SCN_MACRORECORD
    wParam: Integer;                // SCN_MACRORECORD
    lParam: Integer;                // SCN_MACRORECORD
    line: Integer;                  // SCN_MODIFIED
    foldLevelNow: Integer;          // SCN_MODIFIED
    foldLevelPrev: Integer;         // SCN_MODIFIED
    margin: Integer;                // SCN_MARGINCLICK
    listType: Integer;              // SCN_USERLISTSELECTION
    x: Integer;                     // SCN_DWELLSTART, SCN_DWELLEND
    y: Integer;                     // SCN_DWELLSTART, SCN_DWELLEND
    token: Integer;                 // SCN_MODIFIED with SC_MOD_CONTAINER
    annotationLinesAdded: Integer;	// SCN_MODIFIED with SC_MOD_CHANGEANNOTATION
    updated: Integer;	              // SCN_UPDATEUI
  end;

  TScintilla = class;

  TLexer = class
  protected
    FOwner: TScintilla;
    procedure SetAStyle(style: integer; fore: TColor; back: TColor=clWhite; size: integer=-1; face: PAnsiChar=nil);
    function getSampleLines: AnsiString; virtual; abstract;
  public
    constructor Create(AOwner: TScintilla); virtual;
    procedure InitDefaults; virtual; abstract;
    procedure Sample;
  end;
  TLexerClass = class of TLexer;

  { TScintilla }

  TScintilla = class(TWinControl)
  private
    FSciDllHandle: HMODULE;
    FDirectPointer: Pointer;
    FDirectFunction: TScintillaFunction;
    FAccessMethod: TScintillaMethod;
    FLexer: TLexer;
    FLexerClass: TLexerClass;
    procedure LoadSciLibraryIfNeeded;
    procedure FreeSciLibrary;
    procedure SetLexerClass(const Value: TLexerClass);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var AMessage: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var AMessage: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMDestroy(var AMessage: TWMDestroy); message WM_DESTROY;
    /// <summary>Handles notification messages from Scintilla</summary>
    procedure CNNotify(var AMessage: TWMNotify); message CN_NOTIFY;
    procedure MarginClick(nmhdr: PNMHdr);
  public
    {$I funDecl.inc}
    /// <summary>Sends message to Scintilla control.
    function SendEditor(AMessage: Integer; WParam: Integer = 0; LParam: Integer = 0): Integer;
    procedure Fold; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Lexer: TLexer read FLexer;
  published
    property AccessMethod: TScintillaMethod read FAccessMethod write FAccessMethod default smDirect;
    property LexerClass: TLexerClass read FLexerClass write SetLexerClass;
    property Align;
    property Anchors;
  end;

  procedure Register;

{$I consts.inc}
implementation

uses
  DefaultLexer;

{ TScintilla }

{$I funBodies.inc}

constructor TScintilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent:=AOwner as TWinControl;
  FAccessMethod := smDirect;
  Width := 320;
  Height := 240;
  FLexer := TDefaultLexer.Create(self);
  FLexerClass := TDefaultLexer;
  if not(csDesigning in ComponentState) then
  begin
    LoadSciLibraryIfNeeded;
    HandleNeeded;
    FDirectFunction := TScintillaFunction(
           Windows.SendMessage(WindowHandle, SCI_GETDIRECTFUNCTION, 0, 0));
    FDirectPointer := Pointer(
           Windows.SendMessage(WindowHandle, SCI_GETDIRECTPOINTER, 0, 0));
    SendEditor(Sci_SetCodePage, SC_CP_UTF8);
    SendEditor(SCI_SetKeysUnicode, 1);
    SendEditor(SCI_STYLESETFORE, STYLE_DEFAULT, clBlack);
    SendEditor(SCI_STYLESETBACK, STYLE_DEFAULT, clWhite);
    SendEditor(SCI_STYLESETSIZE, STYLE_DEFAULT, 10);
  end;
end;

destructor TScintilla.Destroy;
begin
  inherited Destroy;
  FLexer.Free;
  FreeSciLibrary;
end;

procedure TScintilla.LoadSciLibraryIfNeeded;
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

procedure TScintilla.FreeSciLibrary;
begin
  if FSciDllHandle <> 0 then
    try
      FreeLibrary(FSciDllHandle);
    finally
      FSciDllHandle := 0;
    end;
end;

procedure TScintilla.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'SCINTILLA');
end;

procedure TScintilla.WMDestroy(var AMessage: TWMDestroy);
begin
  inherited;
  // No longer valid after window destory
  FDirectFunction := nil;
  FDirectPointer := nil;
end;

procedure TScintilla.WMEraseBkgnd(var AMessage: TWmEraseBkgnd);
begin
  if csDesigning in ComponentState then
    inherited
  else
    // Erase background not performed, prevent flickering
    AMessage.Result := 0;
end;

procedure TScintilla.WMGetDlgCode(var AMessage: TWMGetDlgCode);
begin
  inherited;
  // Allow key-codes like Enter, Tab, Arrows, and other to be passed to Scintilla
  AMessage.Result := AMessage.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  AMessage.Result := AMessage.Result or DLGC_WANTTAB;
  AMessage.Result := AMessage.Result or DLGC_WANTALLKEYS;
end;

procedure Register;
begin
  RegisterComponents('Scintilla', [TScintilla]);
end;

procedure TScintilla.MarginClick(nmhdr: PNMHdr);
var
  notify: PSciSCNotification;
  line_number: integer;
begin
  notify:=PSciSCNotification(nmhdr);
  //const int modifiers = notify->modifiers;
  //const int position = notify->position;
  //const int margin = notify->margin;
  line_number := SendEditor(SCI_LINEFROMPOSITION, notify.position);
  case notify.margin of
    1: SendEditor(SCI_TOGGLEFOLD, line_number, 0);
  end;
end;

procedure TScintilla.CNNotify(var AMessage: TWMNotify);
begin
  if HandleAllocated and (AMessage.NMHdr^.hwndFrom = Self.Handle) then
  begin
    if AMessage.NMHdr^.code=SCN_MARGINCLICK then MarginClick(AMessage.NMHdr);
    //writeln(AMessage.NMHdr^.code)
  end else
    inherited;
end;

function TScintilla.SendEditor(AMessage: Integer; WParam: Integer;
  LParam: Integer): Integer;
begin
  if (FAccessMethod = smMessages) then
    Result := Windows.SendMessage(Self.Handle, AMessage, WParam, LParam)
  else
    Result := FDirectFunction(FDirectPointer, AMessage, WParam, LParam);
end;

procedure TScintilla.Fold;
begin
  SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold')), integer(PAnsiChar(AnsiString('1'))) );
  SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.comment')), integer(PAnsiChar(AnsiString('1'))) );
  SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.preprocessor')), integer(PAnsiChar(AnsiString('1'))) );
  SendEditor(SCI_SETMARGINWIDTHN, 1, 0);
  SendEditor(SCI_SETMARGINSENSITIVEN, 1, 1);
  SendEditor(SCI_SETMARGINTYPEN,  1, SC_MARGIN_SYMBOL);
  SendEditor(SCI_SETMARGINMASKN, 1, SC_MASK_FOLDERS);
  SendEditor(SCI_SETMARGINWIDTHN, 1, 16);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDER, SC_MARK_PLUS);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEROPEN, SC_MARK_MINUS);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY);
  SendEditor(SCI_MARKERDEFINE, SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY);
  SendEditor(SCI_SETFOLDFLAGS, 16, 0); // 16  	Draw line below if not expanded
  SendEditor(SCI_SETMARGINCURSORN, 1, 0);
end;

procedure TScintilla.SetLexerClass(const Value: TLexerClass);
begin
  if Value<>FLexerClass then
  begin
    FLexer.Free;
    FLexerClass := Value;
    FLexer:=FLexerClass.Create(self);
    FLexer.InitDefaults;
  end;
end;

{ TLexer }

constructor TLexer.Create(AOwner: TScintilla);
begin
  FOwner:=AOwner;
end;

procedure TLexer.Sample;
begin
  FOwner.ClearAll;
  FOwner.AddText(getSampleLines());
  InitDefaults;
  FOwner.Fold;
end;

procedure TLexer.SetAStyle(style: integer; fore: TColor; back: TColor=clWhite; size: integer=-1; face: PAnsiChar=nil);
begin
  FOwner.SendEditor(SCI_STYLESETFORE, style, fore);
  FOwner.SendEditor(SCI_STYLESETBACK, style, back);
  if size >= 1 then
          FOwner.SendEditor(SCI_STYLESETSIZE, style, size);
  if face<>nil then
          FOwner.SendEditor(SCI_STYLESETFONT, style, integer(face));
end;



initialization
{$IFDEF FPC}
  {$I Scint.lrs}
{$ENDIF}
end.
