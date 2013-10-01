object Form1: TForm1
  Left = 192
  Top = 107
  Width = 483
  Height = 417
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    475
    390)
  PixelsPerInch = 96
  TextHeight = 13
  object Scintilla1: TScintilla
    Left = 24
    Top = 8
    Width = 441
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Button1: TButton
    Left = 368
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ComboBox: TComboBox
    Left = 24
    Top = 352
    Width = 145
    Height = 21
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboBoxChange
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      '*|*|html|*.html|pas|*.pas;*.pp;*inc|c/cpp|*.c;*.cpp;*.cxx;*.h;*.' +
      'hpp|Python|*.py|Yaml|*.yaml;*.yml|Xml|*.xml'
    Left = 216
    Top = 344
  end
end
