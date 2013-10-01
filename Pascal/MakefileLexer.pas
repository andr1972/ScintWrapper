unit MakefileLexer;

interface

uses
  Graphics, Scintilla;

type
  TMakefileLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

const
  sampleLines: AnsiString =
'# Make file for Scintilla on Windows'#10+
'# Copyright 1998-2010 by Neil Hodgson <neilh@scintilla.org>'#10+
'# The License.txt file describes the conditions under which this software may be distributed.'#10+
'# This makefile assumes the mingw32 version of GCC 3.x or 4.x is used and changes will'#10+
'# be needed to use other compilers.'#10+
#10+
'.SUFFIXES: .cxx'#10+
'CC = g++'#10+
'DEL = del /q'#10+
#10+
'COMPONENT = ../bin/Scintilla.dll'#10+
'LEXCOMPONENT = ../bin/SciLexer.dll'#10+
'LEXLIB = Lexers.a'#10+
#10+
'vpath %.h ../src ../include ../lexlib'#10+
'vpath %.cxx ../src ../lexlib ../lexers'#10+
#10+
'LDFLAGS=-shared -static -Wl,--enable-runtime-pseudo-reloc-v2 -mwindows -Wl,--add-stdcall-alias'#10+
'LIBS=-lstdc++ -limm32 -lole32 -luuid'#10+
'# Add -MMD to get dependencies'#10+
'INCLUDEDIRS=-I ../include -I ../src -I../lexlib'#10+
'CXXBASEFLAGS=--std=c++0x -Wall -Wno-missing-braces -Wno-char-subscripts -pedantic $(INCLUDEDIRS) -fno-rtti -DDISABLE_D2D'#10+
#10+
'ifdef DEBUG'#10+
'CXXFLAGS=-DDEBUG -g $(CXXBASEFLAGS)'#10+
'else'#10+
'CXXFLAGS=-DNDEBUG -Os $(CXXBASEFLAGS)'#10+
'STRIPFLAG=-s'#10+
'endif'#10+
#10+
'.cxx.o:'#10+
'	$(CC) $(CXXFLAGS) -c $<'#10+
#10+
'ALL:	$(COMPONENT) $(LEXCOMPONENT) $(LEXLIB) ScintillaWinS.o'#10+
#10+
'clean:'#10+
'	$(DEL) *.exe *.o *.obj *.dll *.res *.map *.plist'#10+
#10+
'analyze:'#10+
'	clang --analyze -D DISABLE_D2D $(CXXFLAGS) *.cxx ../src/*.cxx ../lexlib/*.cxx ../lexers/*.cxx'#10+
#10+
'deps:'#10+
'	$(CC) -MM $(CXXFLAGS) *.cxx ../src/*.cxx ../lexlib/*.cxx ../lexers/*.cxx >deps.mak'#10+
#10+
'LEXOBJS:=$(addsuffix .o,$(basename $(notdir $(wildcard ../lexers/Lex*.cxx))))'#10+
#10+
#10+
'BASEOBJS = \'#10+
'	AutoComplete.o \'#10+
'	CallTip.o \'#10+
'	CaseConvert.o \'#10+
'	CaseFolder.o \'#10+
'	CellBuffer.o \'#10+
'	CharacterCategory.o \'#10+
'	CharacterSet.o \'#10+
'	CharClassify.o \'#10+
'	ContractionState.o \'#10+
'	Decoration.o \'#10+
'	Document.o \'#10+
'	Editor.o \'#10+
'	KeyMap.o \'#10+
'	Indicator.o \'#10+
'	LineMarker.o \'#10+
'	PerLine.o \'#10+
'	PlatWin.o \'#10+
'	PositionCache.o \'#10+
'	PropSetSimple.o \'#10+
'	RESearch.o \'#10+
'	RunStyles.o \'#10+
'	ScintRes.o \'#10+
'	Selection.o \'#10+
'	Style.o \'#10+
'	UniConversion.o \'#10+
'	ViewStyle.o \'#10+
'	XPM.o'#10+
#10+
'SOBJS = ScintillaWin.o ScintillaBase.o $(BASEOBJS)'#10+
#10+
'$(COMPONENT): $(SOBJS) Scintilla.def'#10+
'	$(CC) $(LDFLAGS) -o $@ $(STRIPFLAG) $(SOBJS) $(CXXFLAGS) $(LIBS)'#10+
#10+
'LOBJS = \'#10+
'	Accessor.o \'#10+
'	Catalogue.o \'#10+
'	ExternalLexer.o \'#10+
'	LexerBase.o \'#10+
'	LexerModule.o \'#10+
'	LexerSimple.o \'#10+
'	ScintillaWinL.o \'#10+
'	ScintillaBaseL.o \'#10+
'	StyleContext.o \'#10+
'	WordList.o \'#10+
'	$(BASEOBJS) \'#10+
'	$(LEXOBJS)'#10+
'$(LEXCOMPONENT): $(LOBJS) Scintilla.def'#10+
'	$(CC) $(LDFLAGS) -o $@ $(STRIPFLAG) $(LOBJS) $(CXXFLAGS) $(LIBS)'#10+
#10+
'$(LEXLIB): $(LEXOBJS)'#10+
'	$(AR) rc $@ $^'#10+
'	ranlib $@'#10+
#10+
'# Automatically generate dependencies for most files with "make deps"'#10+
'include deps.mak'#10+
#10+
'# These dependencies are maintained by hand as they do not use the default output name'#10+
#10+
'ScintillaBaseL.o: ScintillaBase.cxx Platform.h \'#10+
' ILexer.h Scintilla.h SciLexer.h PropSetSimple.h \'#10+
' SplitVector.h Partitioning.h RunStyles.h \'#10+
' ContractionState.h CellBuffer.h CallTip.h \'#10+
' KeyMap.h Indicator.h XPM.h LineMarker.h \'#10+
' Style.h ViewStyle.h AutoComplete.h \'#10+
' CharClassify.h Decoration.h Document.h \'#10+
' Selection.h PositionCache.h Editor.h \'#10+
' ScintillaBase.h LexAccessor.h Accessor.h \'#10+
' LexerModule.h Catalogue.h CaseFolder.h'#10+
#10+
'ScintillaWinL.o: ScintillaWin.cxx Platform.h \'#10+
' ILexer.h Scintilla.h SplitVector.h \'#10+
' Partitioning.h RunStyles.h ContractionState.h \'#10+
' CellBuffer.h CallTip.h KeyMap.h Indicator.h \'#10+
' XPM.h LineMarker.h Style.h AutoComplete.h \'#10+
' ViewStyle.h CharClassify.h Decoration.h \'#10+
' Document.h Selection.h PositionCache.h \'#10+
' Editor.h ScintillaBase.h UniConversion.h \'#10+
' LexAccessor.h Accessor.h \'#10+
' LexerModule.h Catalogue.h CaseConvert.h \'#10+
' CaseFolder.h'#10+
#10+
'ScintillaWinS.o: ScintillaWin.cxx Platform.h \'#10+
' ILexer.h Scintilla.h SplitVector.h \'#10+
' Partitioning.h RunStyles.h ContractionState.h \'#10+
' CellBuffer.h CallTip.h KeyMap.h Indicator.h \'#10+
' XPM.h LineMarker.h Style.h AutoComplete.h \'#10+
' ViewStyle.h CharClassify.h Decoration.h \'#10+
' Document.h Selection.h PositionCache.h \'#10+
' Editor.h ScintillaBase.h UniConversion.h \'#10+
' CaseConvert.h CaseFolder.h'#10+
#10+
'ScintillaBaseL.o:'#10+
'	$(CC) $(CXXFLAGS) -D SCI_LEXER -c $< -o $@'#10+
#10+
'ScintillaWinS.o:'#10+
'	$(CC) $(CXXFLAGS) -D STATIC_BUILD -c $< -o $@'#10+
#10+
'ScintillaWinL.o:'#10+
'	$(CC) $(CXXFLAGS) -D SCI_LEXER -c $< -o $@'#10+
#10+
'ScintRes.o:	ScintRes.rc'#10+
'	windres ScintRes.rc $@';


function TMakefileLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure TMakefileLexer.InitDefaults;
var
  offWhite,darkGreen,darkBlue: TColor;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_MAKEFILE);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);
  SetAStyle(SCE_MAKE_COMMENT, TColor($808000));
  SetAStyle(SCE_MAKE_PREPROCESSOR, offWhite);
  SetAStyle(SCE_MAKE_IDENTIFIER, darkBlue);
  SetAStyle(SCE_MAKE_TARGET, TColor($005090));
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.makefile')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
