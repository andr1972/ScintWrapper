unit LuaLexer;

interface

uses
  Graphics, Scintilla;

type
  TLuaLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;
const
  sampleLines: AnsiString = //http://www.lua.org/pil/13.1.html
'Set = {}'#10+
#10+
'function Set.new (t)'#10+
'  local set = {}'#10+
'  for _, l in ipairs(t) do set[l] = true end'#10+
'  return set'#10+
'end'#10+
#10+
'function Set.union (a,b)'#10+
'  --[['#10+
'    print(10)         -- no action (comment)'#10+
'  --]]'#10+
'  local res = Set.new{}'#10+
'  for k in pairs(a) do res[k] = true end'#10+
'  for k in pairs(b) do res[k] = true end'#10+
'  return res'#10+
'end'#10+
#10+
'function Set.intersection (a,b)'#10+
'  local res = Set.new{}'#10+
'  for k in pairs(a) do'#10+
'    res[k] = b[k]'#10+
'  end'#10+
'  return res'#10+
'end';

const luaKeyWords: PAnsiChar =
  'and break do else elseif'#10+
  'end false for function if'#10+
  'in local nil not or'#10+
  'repeat return then true until while';

function TLuaLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TLuaLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_LUA);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(luaKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_LUA_WORD, 1);
  SetAStyle(SCE_LUA_WORD, darkBlue);
  SetAStyle(SCE_LUA_COMMENT, TColor($808000));
  SetAStyle(SCE_LUA_COMMENTLINE, TColor($808000));
  SetAStyle(SCE_LUA_COMMENTDOC, TColor($808000));
  SetAStyle(SCE_LUA_NUMBER, clNavy);
  SetAStyle(SCE_LUA_STRING, clFuchsia);
  SetAStyle(SCE_LUA_CHARACTER, clMaroon);
  SetAStyle(SCE_LUA_LITERALSTRING, clGray);
  SetAStyle(SCE_LUA_PREPROCESSOR, offWhite);
  SetAStyle(SCE_LUA_OPERATOR, clGreen);
  SetAStyle(SCE_LUA_IDENTIFIER, TColor($005090));
  SetAStyle(SCE_LUA_STRINGEOL, clFuchsia, clYellow);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.lua')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
