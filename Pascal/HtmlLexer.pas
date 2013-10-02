unit HtmlLexer;

interface

uses
  Graphics, Scintilla;

type
  THtmlLexer = class(TLexer)
  protected
    function getSampleLines: AnsiString; override;
  public
    procedure InitDefaults; override;
  end;

implementation

uses
  LexConsts;

const
  sampleLines: AnsiString =
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#10+
'"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#10+
'<html xmlns="http://www.w3.org/1999/xhtml">'#10+
'<head>'#10+
'<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />'#10+
'<title>JavaScript Function</title>'#10+
'<script language="javascript" type="text/javascript">'#10+
'<!--'#10+
'function demoFunction() {'#10+
'document.write("Hello");'#10+
'}'#10+
'-->'#10+
'</script>'#10+
'</head>'#10+
'<body>'#10+
'<script language="javascript" type="text/javascript">demoFunction();</script>'#10+
'</body>'#10+
'</html>';

const htmlKeyWords: PAnsiChar =
	'a abbr acronym address applet area b base basefont '+
	'bdo big blockquote body br button caption center '+
	'cite code col colgroup dd del dfn dir div dl dt em '+
	'fieldset font form frame frameset h1 h2 h3 h4 h5 h6 '+
	'head hr html i iframe img input ins isindex kbd label '+
	'legend li link map menu meta noframes noscript '+
	'object ol optgroup option p param pre q s samp '+
	'script select small span strike strong style sub sup '+
	'table tbody td textarea tfoot th thead title tr tt u ul '+
	'var xmlns '+
	'abbr accept-charset accept accesskey action align alink '+
	'alt archive axis background bgcolor border '+
	'cellpadding cellspacing char charoff charset checked cite '+
	'class classid clear codebase codetype color cols colspan '+
	'compact content coords '+
	'data datafld dataformatas datapagesize datasrc datetime '+
	'declare defer dir disabled enctype '+
	'face for frame frameborder '+
	'headers height href hreflang hspace http-equiv '+
	'id ismap label lang language link longdesc '+
	'marginwidth marginheight maxlength media method multiple '+
	'name nohref noresize noshade nowrap '+
	'object onblur onchange onclick ondblclick onfocus '+
	'onkeydown onkeypress onkeyup onload onmousedown '+
	'onmousemove onmouseover onmouseout onmouseup '+
	'onreset onselect onsubmit onunload '+
	'profile prompt readonly rel rev rows rowspan rules '+
	'scheme scope shape size span src standby start style '+
	'summary tabindex target text title type usemap '+
	'valign value valuetype version vlink vspace width '+
	'text password checkbox radio submit reset '+
	'file hidden image '+
	'public !doctype xml';

 jsKeyWords: PAnsiChar =
	'break case catch continue default '+
	'do else for function if return throw try var while';

 vbsKeyWords: PAnsiChar =
	'and as byref byval case call const '+
	'continue dim do each else elseif end error exit false for function global '+
	'goto if in loop me new next not nothing on optional or private public '+
	'redim rem resume select set sub then to true type while with '+
	'boolean byte currency date double integer long object single string type '+
	'variant';

function THtmlLexer.getSampleLines: AnsiString;
begin
  result:=sampleLines;
end;

procedure THtmlLexer.InitDefaults;
var
  red,offWhite,darkGreen,darkBlue,lightBlue: TColor;
  bstyle,bastyle,jstyle,jastyle: integer;
  buf: array[0..32] of AnsiChar;
  i: integer;
begin
  FOwner.SendEditor(SCI_SETLEXER, SCLEX_HTML);
  FOwner.SendEditor(SCI_SETSTYLEBITS, 7);

  FOwner.SendEditor(SCI_SETKEYWORDS, 0, integer(htmlKeyWords));
  FOwner.SendEditor(SCI_SETKEYWORDS, 1, integer(jsKeyWords));
  FOwner.SendEditor(SCI_SETKEYWORDS, 2, integer(vbsKeyWords));
  // Set up the global default style. These attributes are used wherever no explicit choices are made.
  SetAStyle(STYLE_DEFAULT, clBlack, clWhite, 10, 'Courier New');
  FOwner.SendEditor(SCI_STYLECLEARALL);	// Copies global style to all others

  red := TColor($0000FF);
  offWhite := TColor($D0E0E0);
  darkGreen := TColor($008000);
  darkBlue := TColor($800000);

  // Hypertext default is used for all the document's text
  SetAStyle(SCE_H_DEFAULT, clBlack);

  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_H_TAG, 1);
  SetAStyle(SCE_H_TAGUNKNOWN, red);
  SetAStyle(SCE_H_ATTRIBUTE, darkGreen);
  SetAStyle(SCE_H_ATTRIBUTEUNKNOWN, red);
  SetAStyle(SCE_H_DOUBLESTRING, clFuchsia);
  SetAStyle(SCE_H_SINGLESTRING, clFuchsia);
  SetAStyle(SCE_H_CDATA, clYellow offWhite);
  SetAStyle(SCE_H_NUMBER, TColor($800080));
  SetAStyle(SCE_H_COMMENT, TColor($808000));
  FOwner.SendEditor(SCI_STYLESETBOLD, SCE_HJ_KEYWORD, 1);
  SetAStyle(SCE_HJ_DOUBLESTRING, clFuchsia);
  SetAStyle(SCE_HJ_SINGLESTRING, clFuchsia);
  SetAStyle(SCE_HJ_NUMBER, darkGreen);
  FOwner.SendEditor(SCI_SETPROPERTY, integer(PAnsiChar('fold.html')), integer(PAnsiChar(AnsiString('1'))) );
end;

end.
